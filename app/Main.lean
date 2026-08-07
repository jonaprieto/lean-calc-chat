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

/-- Milliseconds a showcase step holds before advancing. -/
private def stepMs : UInt32 := 320

/-- Frames the thinking indicator runs for before a result appears. -/
private def thinkingFrames : Nat := 14

/-- Terminal size assumed when the real one cannot be determined. -/
private def fallbackSize : Size := { columns := defaultWidth, rows := 24 }

/-- Rows the chat loop reserves for the blank lines between banner, transcript, and prompt. -/
private def chromeRows : Nat := 4

/-- Longest expression the prompt accepts. -/
private def inputConfig : TextInputConfig := { width := 120, maxLength := 120 }

private def multilineInputConfig : Repl.MultilineConfig :=
  { text := inputConfig, lineBreak := .ctrl 'n' }

/-! ## State -/

private structure JobResult where
  entry : Entry
  answer : Option (Float × (String × String)) := none

private structure App where
  theme : ColorScheme := terracotta
  themeName : String := defaultThemeName
  ans : Float := 0.0
  nextCell : Nat := 1
  entries : List Entry := []
  history : List (String × String) := []
  repl : Repl.State := {}
  activeJobs : Nat := 0
  jobResult : Option JobResult := none
  busy : Bool := false
  running : Bool := true

private def currentSize : IO Size := Repl.Terminal.currentSize fallbackSize

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
so the app can keep its header and prompt visible. -/
private def fitEntries (theme : ColorScheme) (width budget : Nat)
    (entries : List Entry) : List Text :=
  let newestFirst := (entries.map (entryView theme width)).reverse
  let rec keep (remaining : Nat) (kept : List Text) : List Text → List Text
    | [] => kept
    | view :: older =>
        if view.height > remaining then kept
        else keep (remaining - view.height) (view :: kept) older
  match keep budget [] newestFirst, newestFirst with
  -- A single entry taller than the viewport would otherwise blank the transcript: show its tail.
  | [], newest :: _ => [joinLines ((splitLines newest).drop (newest.height - budget))]
  | kept, _ => kept

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

private def screenView (app : App) (size : Size) (showPrompt : Bool) : Text :=
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
    footerView app.theme width app.themeName app.ans
  let used := head.height + (if showPrompt then foot.height else 0) + chromeRows
  let budget := if size.rows > used then size.rows - used else 1
  let views := fitEntries app.theme width budget app.entries
  opaqueScreen app.theme size <|
    head ++ Text.plain "\n\n" ++
      (if views.isEmpty then emptyTranscript app.theme else joinLines views) ++
      (if showPrompt then Text.plain "\n\n" ++ foot else Text.empty)

/-! ## Live widgets -/

/-- The spinner-and-shimmer indicator, redrawn in place while a result is computed. -/
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
  let shimmerConfig := shimmerConfig theme
  let mut glow := LiveRegion.start
  for frame in List.range 16 do
    glow ← glow.updateText (shimmer shimmerConfig { frame } (Text.plain "shimmer"))
    IO.sleep tickMs
  let _ ← glow.finish
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
  | .answer number value :: rest, cell =>
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
  { app with entries := app.entries ++ [entry] }

private def finishJob (app : App) : App :=
  let remaining := app.activeJobs.pred
  { app with activeJobs := remaining, busy := remaining > 0 }

private def applyJobResult (app : App) (result : JobResult) : App :=
  let app := push app result.entry
  match result.answer with
  | none => app
  | some (value, history) =>
      { app with ans := value, history := app.history ++ [history] }

private def mergeJobResult (current completed : App) : App :=
  let current := match completed.jobResult with
    | some result => applyJobResult current result
    | none => current
  finishJob { current with jobResult := none }

private def runCommand (app : App) (cell : Nat) (line : String) : App :=
  match words line with
  | ["/help"] => push app (.note .help)
  | ["/history"] => push app (.note (.history app.history))
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
          match evaluateDetailed app.ans expanded with
          | .ok value =>
              let text := formatValue value
              return { push app (Entry.answer cell text) with
                ans := value
                history := app.history ++ [(raw, text)] }
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
      thinking app.theme
      match evaluateDetailed app.ans expanded with
      | .ok value =>
          let text := formatValue value
          return { push app (Entry.answer cell text) with
            ans := value
            history := app.history ++ [(raw, text)] }
      | .error (.parse error) => return push app (parseFailure cell expanded error)
      | .error (.evaluation message) =>
          return push app (messageFailure (some cell) message)

private def backgroundJobs : Repl.Terminal.JobConfig App where
  shouldRun := fun _ line => !line.startsWith "/"
  start := fun app raw =>
    let cell := app.nextCell
    { app with
      nextCell := cell + 1
      entries := app.entries ++ [Entry.ask cell raw]
      activeJobs := app.activeJobs + 1
      busy := true }
  run := fun cancellation app raw => do
    unless ← Repl.Terminal.Cancellation.sleep cancellation 500 do
      return app
    if ← Repl.Terminal.Cancellation.isCancelled cancellation then
      return app
    let cell := app.nextCell - 1
    match replaceReferences app.entries raw with
    | .error message =>
        pure { app with jobResult := (some
          { entry := messageFailure (some cell) message }) }
    | .ok expanded =>
        match evaluateDetailed app.ans expanded with
        | .ok value =>
            let text := formatValue value
            pure { app with jobResult := (some
              { entry := .answer cell text
                answer := some (value, (raw, text)) }) }
        | .error (.parse error) =>
            pure { app with jobResult := (some
              { entry := parseFailure cell expanded error }) }
        | .error (.evaluation message) =>
            pure { app with jobResult := (some
              { entry := messageFailure (some cell) message }) }
  finish := mergeJobResult
  cancel := fun app => { app with activeJobs := 0, jobResult := none, busy := false }
  fail := fun app message => finishJob (push app (messageFailure none message))

/-! ## Run modes -/

private def interactive (start : App) : IO Unit := do
  Repl.Terminal.run
    { initial := start
      inputConfig := inputConfig
      multiline := some multilineInputConfig
      fallbackSize := fallbackSize
      tickMs := tickMs
      view := fun app size => screenView app size true
      complete := fun _ input => completionIO input
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
  let mut rows : List (String × String) := []
  let mut cell := 1
  for sample in staticSamples do
    writeTextLine (entryView theme width (.ask cell sample))
    match evaluateDetailed ans sample with
    | .ok value =>
        let text := formatValue value
        ans := value
        rows := rows ++ [(sample, text)]
        writeTextLine (entryView theme width (.answer cell text))
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
