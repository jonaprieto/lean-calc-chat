/-
Copyright (c) 2026 Jonathan Prieto-Cubides. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Claude
-/

import TermColor.ColorScheme
import TermColor.Diagnostics
import TermColor.Repl
import TermColor.Repl.Command
import TermColor.Terminal
import Calc.Eval
import Argus

/-!
# Calc.View: every pixel of the chat UI, as pure `Text`

Nothing in this file performs IO. `Main` owns the terminal, the keyboard, and the clock; this file
turns application state into styled text at a given width, so the whole interface renders into a
string and can be diffed with no terminal attached.

Two conventions keep the layout honest:

* every view takes the `ColorScheme` it draws with, so `/theme` repaints the UI without any
  global state;
* every width is derived from the named metrics below rather than written inline, because the
  border-and-padding arithmetic (`outer - 4`) has to agree in eight places at once.
-/

open TermColor
open TermColor.Diagnostics
open TermColor.Layout
open TermColor.Repl
open TermColor.Widgets
open Argus
open scoped TermColor.Style

namespace Calc

/-- Shown in the banner title. Kept in step with the `version` in `lakefile.lean`. -/
def version : String := "0.5.0"

/-! ## Themes

Five palettes, selectable at startup with `CALC_THEME` or at runtime with `/theme`. The three
presets ship with `termcolor`; `aurora` and `terracotta` are this app's own.
-/

/-- Cool aurora colors, the default identity for Lean Calc. -/
def aurora : ColorScheme where
  background := .rgb 13 24 37
  foreground := .rgb 225 241 245
  selection := .rgb 39 70 83
  comment := .rgb 116 155 165
  red := .rgb 243 112 131
  orange := .rgb 255 184 92
  yellow := .rgb 242 220 135
  green := .rgb 111 218 169
  cyan := .rgb 83 211 217
  blue := .rgb 110 168 255
  purple := .rgb 178 146 255
  pink := .rgb 245 132 203

/-- Warm terracotta on slate. -/
def terracotta : ColorScheme where
  background := .rgb 26 28 38
  foreground := .rgb 214 216 226
  selection := .rgb 74 78 96
  comment := .rgb 122 128 148
  red := .rgb 224 108 117
  orange := .rgb 217 119 87
  yellow := .rgb 229 192 123
  green := .rgb 152 195 121
  cyan := .rgb 137 205 209
  blue := .rgb 97 175 239
  purple := .rgb 198 120 221
  pink := .rgb 224 140 180

/-- Every palette the UI can be drawn in, in the order `/theme` lists them. -/
def themes : List (String × ColorScheme) :=
  [ ("aurora", aurora)
  , ("terracotta", terracotta)
  , ("catppuccin", ColorScheme.catppuccin)
  , ("dracula", ColorScheme.dracula)
  , ("monokai", ColorScheme.monokai) ]

/-- The palette used when nothing selects one. -/
def defaultThemeName : String := "aurora"

/-- Look a palette up by name. -/
def themeByName (name : String) : Option ColorScheme := List.lookup name themes

/-- The comma-separated names shown by `/help`. -/
def themeNames : String := String.intercalate ", " (themes.map Prod.fst)

/-! ## Slash-command specification -/

inductive Command where
  | help
  | history
  | showcase
  | theme (name : Option String)
  | load (path : String)
  | clear
  | quit
deriving Repr, BEq, DecidableEq

private def themeParam : Param String :=
  Param.named "THEME" (Param.enum (themes.map fun (name, _) => (name, name)))

def commandSpec : CommandSpec Command :=
  Argus.group "calc"
    [ Argus.cmd "help" (Spec.map (fun _ => .help) (Spec.const ()))
        (description := "Show calculator and command help")
    , Argus.cmd "history" (Spec.map (fun _ => .history) (Spec.const ()))
        (description := "Toggle the history drawer")
    , Argus.cmd "showcase" (Spec.map (fun _ => .showcase) (Spec.const ()))
        (description := "Run the live widget showcase")
    , Argus.cmd "theme"
        (Spec.map Command.theme (Spec.opt (Spec.arg "THEME" "Palette name" themeParam)))
        (description := "Show or select a color theme")
    , Argus.cmd "load" (Spec.map Command.load (Spec.arg "PATH" "Expression file" Param.path))
        (description := "Evaluate one expression from a file")
    , Argus.cmd "clear" (Spec.map (fun _ => .clear) (Spec.const ()))
        (description := "Clear the transcript")
    , Argus.cmd "quit" (Spec.map (fun _ => .quit) (Spec.const ()))
        (description := "Leave the calculator")
    , Argus.cmd "exit" (Spec.map (fun _ => .quit) (Spec.const ()))
        (description := "Leave the calculator") ]

def commandHelpRows : List (String × String) :=
  (commandHelp commandSpec).map fun command => (command.usage, command.description)

/-! ## Metrics

`Layout.boxInnerWidth` is the same arithmetic used by `Layout.box`, so every caller sizes content
with the library's public contract.
-/

/-- Narrowest the UI is drawn. Below this, content truncates rather than reflowing. -/
def minFrameWidth : Nat := 34

/-- The outer width of the banner, prompt, and footer at a given terminal width. -/
def frameWidth (width : Nat) : Nat := max minFrameWidth width

/-- Content width inside a `Layout.box` of outer width `outer`, at the default padding of 1. -/
def boxInnerWidth (outer : Nat) : Nat := Layout.boxInnerWidth { padding := 1 } outer

/-- Columns the transcript reserves for the `›` marker, and so for every continuation line. -/
def askIndent : Nat := 2

/-- Columns the transcript reserves for the `=` marker on a result. -/
def answerIndent : Nat := 4

/-- Panels produced by slash commands sit inside the transcript gutter, so they are narrower than
the frame by exactly that gutter. -/
def panelWidth (width : Nat) : Nat := frameWidth width - askIndent

/-! `Text` has no vertical alignment primitive; keep layout panes at their assigned height. -/
def fillHeight (height : Nat) (text : Text) : Text :=
  let height := max 1 height
  let lines := (splitLines text).take height
  joinLines (lines ++ List.replicate (height - lines.length) Text.empty)

example : (fillHeight 4 (Text.plain "a\nb")).height = 4 := by native_decide

/-- Width of the gap `Layout.columns` leaves between two cells in a table. -/
def tableGap : Nat := 2

/-- Share of a two-column split given to the left column, as a percentage. -/
def leftColumnShare : Nat := 60

/-- Width of the left column of a two-column split of `total`. -/
def splitLeft (total : Nat) : Nat := total * leftColumnShare / 100

/-- Smallest terminal width that can show the calculator and history side by side. -/
def historyDrawerMinWidth : Nat := minFrameWidth * 2 + tableGap

/-- Widths for the calculator and history drawer, when both panes fit. -/
def historyDrawerWidths (width : Nat) : Option (Nat × Nat) :=
  let total := frameWidth width
  if total < historyDrawerMinWidth then none
  else
    let available := total - tableGap
    let left := min (available - minFrameWidth)
      (max minFrameWidth (available * leftColumnShare / 100))
    some (left, available - left)

example : historyDrawerWidths 70 = some (34, 34) := by native_decide

/-- Narrowest a banner pane is allowed to get before it stops shrinking and starts truncating. -/
def minPaneWidth : Nat := 12

/-- Narrowest a table column is allowed to get. -/
def minColumnWidth : Nat := 8

/-- Width of the left column of the `/help` table, sized to its widest entry. -/
def helpKeyWidth : Nat := 21

/-- Bounds on the width of the progress and indeterminate bars. -/
def minBarWidth : Nat := 10

private def indentText (count : Nat) : Text :=
  Text.plain (String.ofList (List.replicate count ' '))

/-! ## Banner -/

private def mascot (theme : ColorScheme) : Text :=
  joinLines
    [ Text.styled "    *    " (Style.fg theme.yellow)
    , Text.styled "  / | \\  " (Style.fg theme.cyan)
    , Text.styled "<--o-->" (Style.fg theme.green)
    , Text.styled "  \\ | /  " (Style.fg theme.blue)
    , Text.styled "    *    " (Style.fg theme.purple) ]

private def bannerLeft (theme : ColorScheme) (width : Nat) : Text :=
  align width .center (truncate width (
    Text.styled "Welcome back!" (Style.bold <+> Style.fg theme.foreground) ++
    Text.plain "\n\n" ++ mascot theme ++ Text.plain "\n\n" ++
    Text.styled "Lean 4 • termcolor stack" (Style.fg theme.comment) ++ Text.plain "\n" ++
    Text.hyperlink "https://github.com/jonaprieto/lean-termcolor"
      (Text.styled "github.com/jonaprieto/lean-termcolor" (Style.fg theme.comment))))

private def bannerRight (theme : ColorScheme) (width : Nat) : Text :=
  let heading := fun (label : String) =>
    Text.styled label (Style.bold <+> Style.fg theme.orange) ++ Text.plain "\n"
  let line := fun (label : String) =>
    Text.styled label (Style.fg theme.foreground) ++ Text.plain "\n"
  align width .left (truncate width (
    heading "Tips for getting started" ++
    line "Type an expression: 2+3*4" ++
    line "ans reuses the last result" ++
    Text.plain "\n\n" ++
    heading "What's new" ++
    line "/showcase runs the widgets" ++
    line "/theme repaints the UI" ++
    Text.styled "enter evaluates • esc quits"
      (Style.italic <+> Style.fg theme.comment)))

/-- The welcome banner: two panes and a rule, inside a titled box. -/
def banner (theme : ColorScheme) (width : Nat) : Text :=
  let outer := frameWidth width
  -- One space of gap around the styled separator.
  let inner := boxInnerWidth outer
  let paneSpace := inner - 1
  let leftWidth := max minPaneWidth (splitLeft paneSpace)
  let rightWidth := max minPaneWidth (paneSpace - leftWidth)
  let left := bannerLeft theme leftWidth
  let right := bannerRight theme rightWidth
  box (columns [leftWidth, rightWidth] 1 [left, right] []
    (Text.styled "│" (Style.fg theme.selection)))
    { title := some (Text.styled s!"Lean Calc v{version}"
        (Style.bold <+> Style.fg theme.orange))
      , titleAlignment := .left
      , borderStyle := Style.fg theme.orange
      , maxWidth := some outer }

/-- The banner reduced to one line, shown once the conversation has started so the transcript gets
the rows back. `/clear` empties the transcript and brings the full banner back. -/
def compactHeader (theme : ColorScheme) (width : Nat) : Text :=
  let outer := frameWidth width
  let title := s!" Lean Calc v{version} "
  let rule := fun (count : Nat) =>
    Text.styled (String.ofList (List.replicate count '─')) (Style.fg theme.selection)
  let used := title.length + 2
  rule 2 ++ Text.styled title (Style.bold <+> Style.fg theme.orange) ++
    rule (if outer > used then outer - used else 0)

/-! ## Transcript -/

/-! Notes keep their data instead of pre-rendered `Text`, so changing the theme repaints old panels
too. -/
inductive Note where
  | help
  | history (rows : List (Nat × (String × String)))
  | theme (current : String)
  | widgets (frame : Nat)

/-- One line of the conversation. -/
inductive Entry where
  /-- What the user typed. -/
  | ask (cell : Nat) (input : String)
  /-- A formatted result. -/
  | answer (cell : Nat) (value : String) (elapsed : Option Nat)
  /-- A source-backed or source-free diagnostic failure. -/
  | failure (cell : Option Nat) (sources : Sources) (value : Diagnostic)
  /-- A panel produced by a slash command. -/
  | note (body : Note)

/-- Shown while the transcript is empty. -/
def emptyTranscript (theme : ColorScheme) : Text :=
  indentText askIndent ++
    Text.styled "Type an expression and press enter, or /help."
      (Style.dim <+> Style.fg theme.comment)

/-! ## Prompt and footer -/

private def roundedChars : BoxChars :=
  { topLeft := '╭', topRight := '╮', bottomLeft := '╰', bottomRight := '╯' }

/-- Columns the `› ` prompt marker takes inside the input bar. -/
private def promptMarkerWidth : Nat := 2

/-- The rounded prompt bar at the bottom of the screen. -/
def promptView (theme : ColorScheme) (width : Nat) (state : TextInputState) : Text :=
  let outer := frameWidth width
  box (Text.styled "› " (Style.fg theme.orange) ++
      TermColor.Repl.renderMultilineTextInputBody
        { width := boxInnerWidth outer - promptMarkerWidth
          textStyle := Style.fg theme.foreground
          cursorStyle := Style.reverse }
        state true)
    { chars := roundedChars
      , borderStyle := Style.fg theme.selection
      , maxWidth := some outer }

/-- Format a measured operation duration for an answer line. -/
def formatElapsed (nanoseconds : Nat) : String :=
  if nanoseconds < 1_000_000 then
    s!"{max 1 (nanoseconds / 1_000)} μs"
  else if nanoseconds < 1_000_000_000 then
    s!"{nanoseconds / 1_000_000}.{nanoseconds % 1_000_000 / 100_000} ms"
  else
    s!"{nanoseconds / 1_000_000_000}.{nanoseconds % 1_000_000_000 / 100_000_000} s"

example : formatElapsed 1_500_000 = "1.5 ms" := by native_decide

/-- The status line under the prompt. -/
def footerView (theme : ColorScheme) (width : Nat) (themeName : String) (ans : Float)
    (busy : Bool) : Text :=
  let outer := frameWidth width
  let leftWidth := outer * 2 / 3
  let status := if busy then "[CALC:BUSY]" else "[CALC:READY]"
  columns [leftWidth, outer - leftWidth] 0
    [ Text.styled status (Style.bold <+> Style.fg theme.orange) ++
        Text.styled s!"  ans = {formatValue ans}  •  theme {themeName}"
          (Style.dim <+> Style.fg theme.comment)
    , Text.styled "/help" (Style.underline <+> Style.fg theme.orange) ]
    [.left, .right]

/-! ## Widget configuration

Shared by the live showcase in `Main` and the still gallery below, so the animated and static
renderings cannot drift apart.
-/

/-- Bar width for the progress widgets at a given frame width. -/
def barWidth (width : Nat) : Nat :=
  max minBarWidth (boxInnerWidth (panelWidth width) - 16)

/-- Progress-bar styling. -/
def progressConfig (theme : ColorScheme) (width : Nat) : ProgressConfig :=
  { width := barWidth width
    , filledStyle := Style.fg theme.blue
    , emptyStyle := Style.dim <+> Style.fg theme.selection
    , percentageStyle := Style.fg theme.foreground }

/-- Indeterminate-bar styling: the same bar with a bouncing segment. -/
def indeterminateConfig (theme : ColorScheme) (width : Nat) : ProgressConfig :=
  { progressConfig theme width with
    filledStyle := Style.fg theme.cyan
    , indeterminateWidth := 8
    , showPercentage := false }

/-- Spinner styling. `SpinnerConfig` has no style field, so the frames carry it. -/
def spinnerConfig (theme : ColorScheme) : SpinnerConfig :=
  { frames := defaultSpinnerFrames.map fun frame =>
      Text.styled frame.plainText (Style.fg theme.orange) }

/-- Column widths for the showcase table. -/
def showcaseTableWidths (width : Nat) : List Nat :=
  let inner := boxInnerWidth (panelWidth width) - tableGap
  let left := max minColumnWidth (splitLeft inner)
  [left, max minColumnWidth (inner - left)]

/-- The bounded workload used by the live showcase. -/
def showcaseComputation (frame : Nat) : Nat × Nat :=
  let limit := frame * 2_000
  -- ponytail: O(n) demo workload; replace with a domain computation if the
  -- showcase needs more load.
  (limit, (List.range limit).foldl (fun total value => total + value * value) 0)

example : showcaseComputation 1 = (2_000, 2_664_667_000) := by native_decide

/-- A spinner frame plus the current result of the showcase computation. -/
def thinkingView (theme : ColorScheme) (frame : Nat) : Text :=
  indentText askIndent ++ spinnerFrame (spinnerConfig theme) frame ++ Text.plain " " ++
    Text.styled "Computing…" (Style.fg theme.comment)

/-- Show the current result of the showcase computation. -/
def computationView (theme : ColorScheme) (frame : Nat) : Text :=
  let (limit, total) := showcaseComputation frame
  Text.styled s!"sum squares < {limit} = {total}" (Style.fg theme.cyan)

/-- Configuration shared by the collapsed and expanded showcase states. -/
def collapsibleConfig (theme : ColorScheme) : CollapsibleConfig :=
  { collapsedMarker := Text.styled "▸ " (Style.fg theme.cyan)
    , expandedMarker := Text.styled "▾ " (Style.fg theme.cyan)
    , summaryStyle := Style.fg theme.foreground
    , bodyStyle := Style.dim <+> Style.fg theme.comment
    , bodyPrefix := Text.plain "  "
    , maxBodyLines := 3
    , overflowText := Text.styled "… more" (Style.fg theme.yellow) }

/-- Render one state of a collapsible build log. -/
def collapsibleView (theme : ColorScheme) (width : Nat) (expanded : Bool) : Text :=
  (renderCollapsible (collapsibleConfig theme) (boxInnerWidth (panelWidth width))
    (Text.plain "build · 4 logs")
    (Text.plain "fetch\nparse\ncompile\npackage")
    { expanded }).text

/-- Render elapsed time without tying the pure view to a clock. -/
def timerView (theme : ColorScheme) (milliseconds : Nat) : Text :=
  Text.styled s!"timer {milliseconds / 1000}.{milliseconds % 1000 / 100}s"
    (Style.fg theme.yellow)

/-! ## Slash-command panels -/

private def panel (theme : ColorScheme) (width : Nat) (title : String) (body : Text) : Text :=
  box body
    { title := some (Text.styled title (Style.bold <+> Style.fg theme.cyan))
      , borderStyle := Style.fg theme.selection
      , maxWidth := some (panelWidth width) }

private def headerRow (theme : ColorScheme) (left right : String) : List Text :=
  [ Text.styled left (Style.bold <+> Style.fg theme.purple)
  , Text.styled right (Style.bold <+> Style.fg theme.purple) ]

/-- Two-column table widths for a panel, given the width of its left column. -/
private def panelTableWidths (width leftWidth : Nat) : List Nat :=
  let inner := boxInnerWidth (panelWidth width) - tableGap
  [leftWidth, max minColumnWidth (inner - leftWidth)]

/-- The `/help` panel. -/
def helpView (theme : ColorScheme) (width : Nat) : Text :=
  let command := fun (text : String) => Text.styled text (Style.fg theme.cyan)
  let commandRows := commandHelpRows.map fun (usage, description) =>
    [command usage, Text.plain description]
  panel theme width "help" (renderTable (panelTableWidths width helpKeyWidth)
    (
    [ headerRow theme "input" "meaning"
    , [Text.plain "2+3*4   (1+2)^5", Text.plain "arithmetic, usual precedence"]
    , [Text.plain "-3^2    2^-2", Text.plain "unary minus, right-associative ^"]
    , [Text.plain "10/4    7%3", Text.plain "division and modulo"]
    , [Text.plain "sqrt abs floor ceil", Text.plain "round ln exp sin cos tan"]
    , [Text.plain "pi tau e ans", Text.plain "constants and the last result"]
    , [Text.plain "[n]", Text.plain "reuse cell n's result or expression"]
    ] ++ commandRows ++ [
    [command "ctrl-n", Text.plain "insert a line break; enter evaluates"]
    , [command "H/K", Text.plain "switch calculator/history columns"]
    , [command "↑/↓ + mouse wheel", Text.plain "scroll the focused history drawer"]
    , [command "CALC_THEME", Text.plain themeNames] ]) tableGap)

/-- The `/history` panel. Rows are newest last. -/
private def historyBody (theme : ColorScheme) (width : Nat)
    (rows : List (Nat × (String × String))) : Text :=
  if rows.isEmpty then
    Text.styled "Nothing evaluated yet."
      (Style.dim <+> Style.fg theme.comment)
  else
    let tableWidth := boxInnerWidth (panelWidth width)
    let available := tableWidth - 2 * tableGap
    let indexWidth := max 1 (rows.foldl (fun widest (cell, _) =>
      max widest (toString cell).length) 0)
    let textWidth := available - indexWidth
    let expressionWidth := max minColumnWidth (textWidth * leftColumnShare / 100)
    renderTable [indexWidth, expressionWidth,
      max minColumnWidth (textWidth - expressionWidth)]
      ([ Text.styled "#" (Style.bold <+> Style.fg theme.purple)
       , Text.styled "expression" (Style.bold <+> Style.fg theme.purple)
       , Text.styled "result" (Style.bold <+> Style.fg theme.purple) ] ::
        rows.map (fun (cell, (input, value)) =>
          [ Text.styled (toString cell) (Style.fg theme.comment)
          , Text.styled input (Style.fg theme.foreground)
          , Text.styled value (Style.fg theme.green) ])) tableGap
      [.right, .left, .right]

private def historyRowsWindow (height offset : Nat)
    (rows : List (Nat × (String × String))) : List (Nat × (String × String)) :=
  -- App history is newest-first; reverse only the visible slice for chronological display.
  let count := if height > 3 then height - 3 else 1
  let start := min rows.length offset
  ((rows.drop start).take count).reverse

example : (historyRowsWindow 4 0
    [(1, ("1", "1")), (2, ("2", "2")), (3, ("3", "3"))]).length = 1 := by
  native_decide

private def historyWindow (height offset : Nat) (body : Text) : Text :=
  let viewport := if height > 2 then height - 2 else 1
  let lines := splitLines body
  let maxOffset := lines.length - min lines.length viewport
  let start := maxOffset - min maxOffset offset
  fillHeight viewport (joinLines ((lines.drop start).take viewport))

example : (historyWindow 4 0 (Text.plain "a\nb\nc")).height = 2 := by native_decide

/-- The `/history` panel. Rows are newest last. -/
def historyView (theme : ColorScheme) (width : Nat)
    (rows : List (Nat × (String × String))) : Text :=
  panel theme width "history" (historyBody theme width rows)

/-- The persistent history drawer. `width` is its exact outer column width. -/
def historyDrawerView (theme : ColorScheme) (width : Nat)
    (rows : List (Nat × (String × String))) (focused : Bool) (height offset : Nat) : Text :=
  let viewWidth := width + askIndent
  let title := if focused then "history • active" else "history"
  let rows := historyRowsWindow height offset rows
  let body := historyWindow height 0 (historyBody theme viewWidth rows)
  let innerWidth := boxInnerWidth (panelWidth viewWidth)
  panel theme viewWidth title (padRight innerWidth body)

/-- The `/theme` panel: every palette, drawn in its own colours. -/
def themeView (theme : ColorScheme) (width : Nat) (current : String) : Text :=
  let nameWidth := themes.foldl (fun widest (name, _) => max widest name.length) 0 + 1
  panel theme width "themes" (joinLines (themes.map (fun (name, scheme) =>
    let marker := if name == current then "● " else "○ "
    Text.styled marker (Style.fg scheme.orange) ++
    padRight nameWidth (Text.styled name (Style.bold <+> Style.fg scheme.foreground)) ++
    Text.styled "██" (Style.fg scheme.orange) ++
    Text.styled "██" (Style.fg scheme.green) ++
    Text.styled "██" (Style.fg scheme.cyan) ++
    Text.styled "██" (Style.fg scheme.purple) ++
    Text.styled "██" (Style.fg scheme.red))))

/-- Every widget the stack ships, rendered as one still frame. The live showcase in `Main` drives
the same pure renderers through `LiveRegion`. -/
def widgetGallery (theme : ColorScheme) (width : Nat) (frame : Nat) : Text :=
  panel theme width "widgets" (joinLines
    [ progressBar (progressConfig theme width)
        { current := 7, total := 10, label := Text.styled "progress" (Style.fg theme.foreground) }
    , indeterminateProgressBar (indeterminateConfig theme width)
        { frame, label := Text.styled "indeterminate" (Style.fg theme.foreground) }
    , renderSpinner (spinnerConfig theme)
        { frame, label := Text.styled "spinner" (Style.fg theme.foreground) }
    , computationView theme frame
    , timerView theme (frame * 60)
    , collapsibleView theme width false
    , collapsibleView theme width true
    , renderStatus .success (Text.styled "status" (Style.fg theme.green))
    , renderTable (showcaseTableWidths width)
        [ headerRow theme "table" "value"
        , [Text.plain "rows wrap", Text.styled "yes" (Style.fg theme.green)] ] tableGap ])

private def cellMarker
    (theme : ColorScheme) (cell : Nat) (symbol : String) (style : Style) : Text :=
  Text.styled s!"[{cell}] " (Style.dim <+> Style.fg theme.comment) ++
    Text.styled s!"{symbol} " style

private def resultMarker (symbol : String) (style : Style) : Text :=
  Text.styled s!"  {symbol} " style

private def markerWidth (cell : Nat) (symbol : String) : Nat :=
  s!"[{cell}] {symbol} ".length

private def noteView (theme : ColorScheme) (width : Nat) : Note → Text
  | .help => helpView theme width
  | .history rows => historyView theme width rows
  | .theme current => themeView theme width current
  | .widgets frame => widgetGallery theme width frame

private def diagnosticView (theme : ColorScheme) (width : Nat) (sources : Sources)
    (diagnostic : Diagnostic) : Text :=
  TermColor.Diagnostics.render sources diagnostic
    { width := max 1 (frameWidth width - askIndent), contextLines := 0, hyperlinks := false } theme

/-- Render one transcript entry with its numbered cell gutter. -/
def entryView (theme : ColorScheme) (width : Nat) (entry : Entry) : Text :=
  let inner := frameWidth width
  match entry with
  | .ask cell input =>
      let marker := cellMarker theme cell "›" (Style.bold <+> Style.fg theme.orange)
      gutter marker (markerWidth cell "›") inner (Text.styled input (Style.fg theme.foreground))
  | .answer _ value elapsed =>
      let timing := match elapsed with
        | none => Text.empty
        | some nanoseconds =>
            Text.styled s!"  ({formatElapsed nanoseconds})"
              (Style.dim <+> Style.fg theme.comment)
      gutter (resultMarker "=" (Style.fg theme.green)) answerIndent inner
        (Text.styled value (Style.bold <+> Style.fg theme.green) ++ timing)
  | .failure _ sources diagnostic =>
      gutter (resultMarker "!" (Style.bold <+> Style.fg theme.red)) answerIndent inner
        (diagnosticView theme width sources diagnostic)
  | .note body => gutter (indentText askIndent) askIndent inner (noteView theme width body)

end Calc
