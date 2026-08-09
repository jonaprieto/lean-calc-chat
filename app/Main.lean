/-
Copyright (c) 2026 Jonathan Prieto-Cubides. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Claude
-/

import Calc
import GripDiagnostics
import TermColor.Repl
import TermColor.Repl.FileCompletion
import TermColor.Repl.Terminal

/-!
# Main: the terminal, the keyboard, and the clock

Everything stateful lives here. `Calc.View` renders `App` into `Text`; this file clears the
screen, reads one key at a time, drives the live widgets, and prints results.

Three run modes:
* a TTY on both ends gives the full-screen chat loop;
* anything else (a pipe, CI, `CALC_NONINTERACTIVE=1`) gets a static transcript;
* `calc "2+3*4"` evaluates its arguments and exits.
-/

open TermColor
open TermColor.Diagnostics
open TermColor.Layout
open TermColor.Terminal
open TermColor.Widgets
open TermColor.Repl
open scoped TermColor.Style
open Grip
open Calc

/-! ## Configuration -/

/-- Selects the startup palette. `/theme` changes it at runtime. -/
private def themeEnvVar : String := "CALC_THEME"

/-- Forces the static transcript even on a TTY. -/
private def nonInteractiveEnvVar : String := "CALC_NONINTERACTIVE"

/-- Milliseconds between animation frames. -/
private def tickMs : UInt32 := 60

/-- Interactive redraw cadence; input processing is independent of this frame budget. -/
private def renderMs : UInt32 := 16

/-- Lines moved by one history scroll command or mouse wheel event. -/
private def historyScrollStep : Nat := 3

/-- Milliseconds a showcase step holds before advancing. -/
private def stepMs : UInt32 := 320

/-- Frames the thinking indicator runs for before a result appears. -/
private def thinkingFrames : Nat := 14

/-- Terminal size assumed when the real one cannot be determined. -/
private def fallbackSize : Size := { columns := defaultWidth, rows := 24 }

/-- Rows the chat loop reserves for one blank line around the transcript. -/
private def chromeRows : Nat := 2

/-- Longest expression the prompt accepts. -/
private def inputConfig : TextInputConfig := { width := 120, maxLength := 120 }

private def multilineInputConfig : Repl.MultilineConfig :=
  { text := inputConfig, lineBreak := .ctrl 'n' }

/-! ## State -/

private structure JobResult where
  entry : Entry
  answer : Option (Float × (Nat × (String × String))) := none

private structure App where
  theme : ColorScheme := aurora
  themeName : String := defaultThemeName
  ans : Float := 0.0
  nextCell : Nat := 1
  entries : List Entry := []
  history : List (Nat × (String × String)) := []
  historyOpen : Bool := false
  focusColumn : Nat := 0
  historyOffset : Nat := 0
  repl : Repl.State := {}
  activeJobs : Nat := 0
  jobResult : Option JobResult := none
  busy : Bool := false
  running : Bool := true

private def currentSize : IO Size := Repl.Terminal.currentSize fallbackSize

private def elapsedSince (started : Nat) : IO Nat := do
  pure ((← IO.monoNanosNow) - started)

private def messageFailure (cell : Option Nat) (message : String) : Entry :=
  .failure cell #[] (Diagnostic.error message)

private def resolveTheme : IO App := do
  match ← IO.getEnv themeEnvVar with
  | none => pure {}
  | some name =>
      match themeByName name with
      | some scheme => pure { theme := scheme, themeName := name }
      | none =>
          pure { entries :=
            [messageFailure none s!"{themeEnvVar}={name} is not a theme. Try: {themeNames}."] }

/-! ## Rendering -/

/-- Keep the newest entries that fit in `budget` rows, oldest first.

`Screen` retains rendered lines and performs the terminal diff; the transcript remains a viewport
so the app can keep its header and prompt visible. Entries are stored newest first, so rendering
stops as soon as the viewport is full. -/
private def fitEntries (theme : ColorScheme) (width budget : Nat)
    (entries : List Entry) : List Text :=
  let rec keep (remaining : Nat) (kept : List Text) : List Entry → List Text
    | [] => kept
    | entry :: older =>
        let view := entryView theme width entry
        if view.height > remaining then
          if kept.isEmpty then
            -- A single entry taller than the viewport would otherwise blank the transcript.
            [joinLines ((splitLines view).drop (view.height - remaining))]
          else
            kept
        else
          keep (remaining - view.height) (view :: kept) older
  keep budget [] entries

private def withBackground (theme : ColorScheme) (text : Text) : Text :=
  { segments := text.segments.map fun segment =>
      { segment with style := Style.bg theme.background <+> segment.style } }

private def opaqueScreen (theme : ColorScheme) (size : Size) (content : Text) : Text :=
  let width := max 1 size.columns
  let rows := max 1 size.rows
  let blank := Text.styled (String.ofList (List.replicate width ' '))
    (Style.bg theme.background)
  let lines := (splitLines (wrapLines width content)).take rows
  let lines := lines.map (fun line => withBackground theme (padRight width line))
  joinLines (lines ++ List.replicate (rows - lines.length) blank)

private def calcContent (app : App) (size : Size) (showPrompt : Bool) : Text :=
  let width := size.columns
  let head :=
    if app.entries.isEmpty then banner app.theme width else compactHeader app.theme width
  let menu := app.repl.completion.map fun completion =>
    Text.plain "\n" ++ renderCompletionMenu
      { width := boxInnerWidth (frameWidth width) } completion
  let status := if app.busy then
      Text.plain "\n" ++ Text.styled "Computing…" (Style.fg app.theme.cyan)
    else Text.empty
  let foot := promptView app.theme width app.repl.input ++ menu.getD Text.empty ++
    status ++ Text.plain "\n" ++
    footerView app.theme width app.themeName app.ans app.busy ++
    if app.historyOpen then
      Text.plain "\n" ++ Text.styled
        (if app.focusColumn == 0 then "H/K switch columns • calc active"
         else "H/K switch columns • history active")
        (Style.dim <+> Style.fg app.theme.comment)
    else Text.empty
  let used := head.height + (if showPrompt then foot.height else 0) + chromeRows
  let budget := if size.rows > used then size.rows - used else 1
  let views := fitEntries app.theme width budget app.entries
  let transcript := if views.isEmpty then emptyTranscript app.theme else joinLines views
  head ++ Text.plain "\n" ++
    fillHeight budget transcript ++
    (if showPrompt then Text.plain "\n" ++ foot else Text.empty)

private def screenView (app : App) (size : Size) (showPrompt : Bool) : Text :=
  match app.historyOpen, historyDrawerWidths size.columns with
  | true, some (leftWidth, rightWidth) =>
      let left := calcContent app { size with columns := leftWidth } showPrompt
      let history := historyDrawerView app.theme rightWidth app.history (app.focusColumn == 1)
        size.rows app.historyOffset
      opaqueScreen app.theme size <|
        columns [leftWidth, rightWidth] tableGap [left, history] []
          (Text.styled "│" (Style.fg app.theme.selection))
  | _, _ => opaqueScreen app.theme size (calcContent app size showPrompt)

/-! ## Live widgets -/

/-- The spinner indicator, redrawn in place while a result is computed. -/
private def thinking (theme : ColorScheme) : IO Unit := do
  unless ← stdoutSupportsControl do
    return
  let mut region := LiveRegion.start
  for frame in List.range thinkingFrames do
    region ← region.updateText (thinkingView theme frame)
    IO.sleep tickMs
  let _ ← region.finish
  pure ()

private def showcaseHeader (theme : ColorScheme) : List Text :=
  [ Text.styled "stage" (Style.bold <+> Style.fg theme.purple)
  , Text.styled "state" (Style.bold <+> Style.fg theme.purple) ]

/-- Drive every live object the stack ships, in order. `Calc.widgetGallery` renders the same
configuration as one still frame for non-interactive output. -/
private def showcase (theme : ColorScheme) (width : Nat) : IO Unit := do
  unless ← stdoutSupportsControl do
    return
  let label := fun (text : String) => Text.styled text (Style.fg theme.foreground)
  let progressConfig := progressConfig theme width
  let mut progress := LiveRegion.start
  for current in List.range 11 do
    progress ← progress.updateText
      (progressBar progressConfig { current, total := 10, label := label "progress" })
    IO.sleep tickMs
  let _ ← progress.finish
  let scanningConfig := indeterminateConfig theme width
  let mut scanning := LiveRegion.start
  for frame in List.range 18 do
    scanning ← scanning.updateText
      (indeterminateProgressBar scanningConfig { frame, label := label "indeterminate" })
    IO.sleep tickMs
  let _ ← scanning.finish
  let spinnerConfig := spinnerConfig theme
  let mut spinner := LiveRegion.start
  for frame in List.range 14 do
    spinner ← spinner.updateText (renderSpinner spinnerConfig { frame, label := label "spinner" })
    IO.sleep tickMs
  let _ ← spinner.finish
  let mut computation := LiveRegion.start
  for frame in List.range 11 do
    computation ← computation.updateText (computationView theme frame)
    IO.sleep tickMs
  let _ ← computation.finish
  let mut collapsible := LiveRegion.start
  for expanded in [false, true] do
    collapsible ← collapsible.updateText (collapsibleView theme width expanded)
    IO.sleep stepMs
  let _ ← collapsible.finish
  let timerStart ← IO.monoNanosNow
  let mut timer := LiveRegion.start
  for _ in List.range 11 do
    let now ← IO.monoNanosNow
    timer ← timer.updateText (timerView theme ((now - timerStart) / 1_000_000))
    IO.sleep tickMs
  let _ ← timer.finish
  let mut status := LiveRegion.start
  status ← status.updateText (renderStatus .warning
    (Text.styled "status: warning" (Style.fg theme.yellow)))
  IO.sleep stepMs
  status ← status.updateText (renderStatus .success
    (Text.styled "status: ready" (Style.fg theme.green)))
  let _ ← status.finish
  let tableWidths := showcaseTableWidths width
  let mut table := LiveRegion.start
  table ← table.updateText (renderTable tableWidths
    [showcaseHeader theme
      , [Text.plain "table", Text.styled "running" (Style.fg theme.yellow)]] tableGap)
  IO.sleep stepMs
  table ← table.updateText (renderTable tableWidths
    [showcaseHeader theme
      , [Text.plain "table", Text.styled "done" (Style.fg theme.green)]] tableGap)
  let _ ← table.finish
  pure ()

/-! ## Commands -/

private def words (line : String) : List String :=
  (line.splitOn " ").filter (fun word => !word.isEmpty)

private def commandNames : List String :=
  ["/help", "/history", "/showcase", "/theme", "/load", "/clear", "/quit", "/exit"]

private def completionCandidates (input : TextInputState) : List Repl.Completion :=
  match words input.value with
  | [fragment] =>
      commandNames.filter (·.startsWith fragment) |>.map (fun replacement => { replacement })
  | ["/theme", fragment] =>
      (themes.map Prod.fst).filter (·.startsWith fragment) |>.map
        (fun name => { replacement := s!"/theme {name}" })
  | _ => []

private def completionIO (input : TextInputState) : IO (List Repl.Completion) :=
  if input.value.startsWith "/load " then
    defaultFileCompletions input
  else
    pure (completionCandidates input)

private def cellInput : List Entry → Nat → Option String
  | [], _ => none
  | .ask number input :: rest, cell =>
      if number == cell then some input else cellInput rest cell
  | _ :: rest, cell => cellInput rest cell

private def cellOutput : List Entry → Nat → Option String
  | [], _ => none
  | .answer number value _ :: rest, cell =>
      if number == cell then some value else cellOutput rest cell
  | _ :: rest, cell => cellOutput rest cell

private def cellReference (entries : List Entry) (cell : Nat) : Option String :=
  (cellOutput entries cell).orElse (fun _ => cellInput entries cell)

private def parseFailure (cell : Nat) (input : String) (error : ParseError) : Entry :=
  let source := Source.fromBytes "input" input.toUTF8
  .failure (some cell) #[source] (GripDiagnostics.diagnostic source error)

private def replaceReferences (entries : List Entry) (input : String) : Except String String :=
  let step (state : Except String (Bool × (List Char × List Char))) (character : Char) :=
    match state with
    | .error message => .error message
    | .ok (inReference, (digits, built)) =>
        if inReference then
          if character.isDigit then
            .ok (true, (character :: digits, built))
          else if character == ']' then
            match (String.ofList digits.reverse).toNat? with
            | none => .error "cell references use [n]"
            | some cell =>
                match cellReference entries cell with
                | none => .error s!"cell [{cell}] does not exist"
                | some value =>
                    let replacement := if value.startsWith "/" then value else "(" ++ value ++ ")"
                    .ok (false, ([], replacement.toList.reverse ++ built))
          else .error "cell references use [n]"
        else if character == '[' then
          .ok (true, ([], built))
        else
          .ok (false, ([], character :: built))
  match input.toList.foldl step (.ok (false, ([], []))) with
  | .error message => .error message
  | .ok (true, _) => .error "cell references use [n]"
  | .ok (false, (_, built)) => .ok (String.ofList built.reverse)

private def push (app : App) (entry : Entry) : App :=
  { app with entries := entry :: app.entries }

private def finishJob (app : App) : App :=
  let remaining := app.activeJobs.pred
  { app with activeJobs := remaining, busy := remaining > 0 }

private def applyJobResult (app : App) (result : JobResult) : App :=
  let app := push app result.entry
  match result.answer with
  | none => app
  | some (value, history) =>
      { app with ans := value, history := history :: app.history }

private def mergeJobResult (current completed : App) : App :=
  let current := match completed.jobResult with
    | some result => applyJobResult current result
    | none => current
  finishJob { current with jobResult := none }

private def runCommand (app : App) (cell : Nat) (line : String) : App :=
  match words line with
  | ["/help"] => push app (.note .help)
  | ["/history"] =>
      let opening := !app.historyOpen
      { app with
        historyOpen := opening
        historyOffset := if opening then 0 else app.historyOffset }
  | ["/theme"] => push app (.note (.theme app.themeName))
  | ["/theme", name] =>
      match themeByName name with
      | some scheme =>
          push { app with theme := scheme, themeName := name }
            (.note (.theme name))
      | none => push app (messageFailure (some cell)
          s!"'{name}' is not a theme. Try: {themeNames}.")
  | ["/clear"] => { app with entries := [] }
  | ["/quit"] | ["/exit"] => { app with running := false }
  | _ => push app (messageFailure (some cell) s!"'{line}' is not a command. Try /help.")

private def submit (app : App) (raw : String) : IO App := do
  let width := (← currentSize).columns
  let cell := app.nextCell
  let app := push { app with
      nextCell := cell + 1 } (Entry.ask cell raw)
  if raw.startsWith "/" then
    if words raw == ["/showcase"] then
      let (_, ()) ← Repl.Terminal.suspend (showcase app.theme width)
      return push app (.note (.widgets 6))
    if let ["/load", path] := words raw then
      let source ← try IO.FS.readFile ⟨path⟩ catch error =>
        let next := push app (messageFailure (some cell) s!"could not read '{path}': {error}")
        return next
      let source := source.trimAscii.toString
      match replaceReferences app.entries source with
      | .error message => return push app (messageFailure (some cell) message)
      | .ok expanded =>
          let started ← IO.monoNanosNow
          match evaluateDetailed app.ans expanded with
          | .ok value =>
              let text := formatValue value
              let elapsed ← elapsedSince started
              return { push app (Entry.answer cell text (some elapsed)) with
                ans := value
                history := (cell, (raw, text)) :: app.history }
          | .error (.parse error) => return push app (parseFailure cell expanded error)
          | .error (.evaluation message) =>
              return push app (messageFailure (some cell) message)
    let app := runCommand app cell raw
    return app
  match replaceReferences app.entries raw with
  | .error message => return push app (messageFailure (some cell) message)
  | .ok expanded =>
      if expanded.startsWith "/" then
        if words expanded == ["/showcase"] then
          let (_, ()) ← Repl.Terminal.suspend (showcase app.theme width)
          return push app (.note (.widgets 6))
        let app := runCommand app cell expanded
        return app
      let started ← IO.monoNanosNow
      match evaluateDetailed app.ans expanded with
      | .ok value =>
          let text := formatValue value
          let elapsed ← elapsedSince started
          return { push app (Entry.answer cell text (some elapsed)) with
            ans := value
            history := (cell, (raw, text)) :: app.history }
      | .error (.parse error) => return push app (parseFailure cell expanded error)
      | .error (.evaluation message) =>
          return push app (messageFailure (some cell) message)

private def backgroundJobs : Repl.Terminal.JobConfig App where
  shouldRun := fun _ line => !line.startsWith "/"
  start := fun app raw =>
    let cell := app.nextCell
    { app with
      nextCell := cell + 1
      entries := Entry.ask cell raw :: app.entries
      activeJobs := app.activeJobs + 1
      busy := true }
  run := fun cancellation app raw => do
    -- Let the event reader install its wake path before a very fast job completes.
    unless ← Repl.Terminal.Cancellation.sleep cancellation 10 do
      return app
    let cell := app.nextCell - 1
    match replaceReferences app.entries raw with
    | .error message =>
        pure { app with jobResult := (some
          { entry := messageFailure (some cell) message }) }
    | .ok expanded =>
        let started ← IO.monoNanosNow
        match evaluateDetailed app.ans expanded with
        | .ok value =>
            let text := formatValue value
            let elapsed ← elapsedSince started
            pure { app with jobResult := (some
              { entry := .answer cell text (some elapsed)
                answer := some (value, (cell, (raw, text))) }) }
        | .error (.parse error) =>
            pure { app with jobResult := (some
              { entry := parseFailure cell expanded error }) }
        | .error (.evaluation message) =>
            pure { app with jobResult := (some
              { entry := messageFailure (some cell) message }) }
  finish := mergeJobResult
  cancel := fun app => { app with activeJobs := 0, jobResult := none, busy := false }
  fail := fun app message => finishJob (push app (messageFailure none message))

private def handleHistoryKey (app : App) (key : Key) : Option App :=
  if !app.historyOpen then none
  else
    match key with
    | .char 'H' => some { app with focusColumn := 0 }
    | .char 'K' => some { app with focusColumn := 1 }
    | .up | .pageUp =>
        if app.focusColumn == 1 then
          some { app with historyOffset := app.historyOffset + historyScrollStep }
        else none
    | .down | .pageDown =>
        if app.focusColumn == 1 then
          some { app with historyOffset := app.historyOffset - historyScrollStep }
        else none
    | _ => none

private def handleHistoryMouse (app : App) (size : Size) (mouse : MouseEvent) : Option App :=
  if !app.historyOpen then none
  else
    match historyDrawerWidths size.columns with
    | none => none
    | some (leftWidth, rightWidth) =>
        let historyLeft := leftWidth + tableGap + 1
        let historyRight := historyLeft + rightWidth - 1
        let inHistory := mouse.column >= historyLeft && mouse.column <= historyRight
        let inCalc := mouse.column >= 1 && mouse.column <= leftWidth
        match mouse.action with
        | .scrollUp =>
            if inHistory then
              some { app with
                focusColumn := 1
                historyOffset := app.historyOffset + historyScrollStep }
            else none
        | .scrollDown =>
            if inHistory then
              some { app with
                focusColumn := 1
                historyOffset := app.historyOffset - historyScrollStep }
            else none
        | .press =>
            if mouse.button != .left then none
            else if inHistory then some { app with focusColumn := 1 }
            else if inCalc then some { app with focusColumn := 0 }
            else none
        | _ => none

/-! ## Run modes -/

private def interactive (start : App) : IO Unit := do
  clearScreen
  Repl.Terminal.run
    { initial := start
      inputConfig := inputConfig
      multiline := some multilineInputConfig
      fallbackSize := fallbackSize
      tickMs := renderMs
      mouse := true
      view := fun app size => screenView app size true
      complete := fun _ input => completionIO input
      handleKey := handleHistoryKey
      handleMouse := handleHistoryMouse
      getState := fun app => app.repl
      setState := fun app repl => { app with repl }
      submit := submit
      jobs := some backgroundJobs
      isRunning := fun app => app.running
      quit := fun app => { app with running := false } }

private def staticSamples : List String :=
  ["2+3*4", "(1+2)^5", "2^-2", "sqrt(2)", "10/4 + 7%3", "ans * 2", "1/0", "2 * (3 + "]

private def staticDemo (start : App) : IO Unit := do
  let theme := start.theme
  let width ← terminalWidth
  writeTextLine (banner theme width)
  IO.println ""
  -- Anything `resolveTheme` complained about, e.g. an unknown CALC_THEME.
  for entry in start.entries do
    writeTextLine (entryView theme width entry)
  let mut ans : Float := 0.0
  let mut rows : List (Nat × (String × String)) := []
  let mut cell := 1
  for sample in staticSamples do
    writeTextLine (entryView theme width (.ask cell sample))
    let started ← IO.monoNanosNow
    match evaluateDetailed ans sample with
    | .ok value =>
        let text := formatValue value
        let elapsed ← elapsedSince started
        ans := value
        rows := rows ++ [(cell, (sample, text))]
        writeTextLine (entryView theme width (.answer cell text (some elapsed)))
    | .error (.parse error) =>
        writeTextLine (entryView theme width (parseFailure cell sample error))
    | .error (.evaluation message) =>
        writeTextLine (entryView theme width (messageFailure (some cell) message))
    cell := cell + 1
  IO.println ""
  -- The live objects need cursor control; show one frame of each instead.
  writeTextLine (thinkingView theme 6)
  IO.println ""
  writeTextLine (entryView theme width (.note (.widgets 6)))
  writeTextLine (entryView theme width (.note (.history rows)))
  writeTextLine (entryView theme width (.note (.theme start.themeName)))
  writeTextLine (entryView theme width (.note .help))

private def usage : String :=
  s!"calc {version} — a chat-shaped calculator on the termcolor stack

usage:
  calc                 interactive chat on a TTY, static transcript otherwise
  calc <expression>    evaluate once and print the result
  calc --help          this text
  calc --version       print the version

environment:
  {themeEnvVar}={themeNames}
  {nonInteractiveEnvVar}=1     force the static transcript"

def main (args : List String) : IO Unit := do
  match args with
  | [] =>
      let start ← resolveTheme
      let interactiveTerminal := (← stdoutIsTty) && (← stdinIsTty) && (← stdoutSupportsControl)
      let blocked := (← IO.getEnv "CI").isSome || (← IO.getEnv nonInteractiveEnvVar).isSome
      if interactiveTerminal && !blocked then
        try interactive start
        catch _ =>
          IO.println "raw input unavailable; showing a static transcript"
          staticDemo start
      else
        staticDemo start
  | ["--help"] | ["-h"] => IO.println usage
  | ["--version"] | ["-V"] => IO.println s!"calc {version}"
  | args =>
      match evaluate 0.0 (String.intercalate " " args) with
      | .ok value => IO.println (formatValue value)
      | .error message =>
          IO.eprintln message
          IO.Process.exit 1
