/-
Copyright (c) 2026 Jonathan Prieto-Cubides. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Claude
-/

import TermColor.ColorScheme
import TermColor.Diagnostics
import TermColor.Repl
import TermColor.Terminal
import Calc.Eval

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
open TermColor.Widgets
open scoped TermColor.Style

namespace Calc

/-- Shown in the banner title. Kept in step with the `version` in `lakefile.lean`. -/
def version : String := "0.5.0"

/-! ## Themes

Four palettes, selectable at startup with `CALC_THEME` or at runtime with `/theme`. The three
presets ship with `termcolor`; `terracotta` is this app's own.
-/

/-- Warm terracotta on slate, in the spirit of the Claude Code banner. -/
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
  [ ("terracotta", terracotta)
  , ("catppuccin", ColorScheme.catppuccin)
  , ("dracula", ColorScheme.dracula)
  , ("monokai", ColorScheme.monokai) ]

/-- The palette used when nothing selects one. -/
def defaultThemeName : String := "terracotta"

/-- Look a palette up by name. -/
def themeByName (name : String) : Option ColorScheme := List.lookup name themes

/-- `"terracotta, catppuccin, dracula, monokai"`. -/
def themeNames : String := String.intercalate ", " (themes.map Prod.fst)

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

/-- Width of the gap `Layout.columns` leaves between two cells in a table. -/
def tableGap : Nat := 2

/-- Share of a two-column split given to the left column, as a percentage. -/
def leftColumnShare : Nat := 60

/-- Width of the left column of a two-column split of `total`. -/
def splitLeft (total : Nat) : Nat := total * leftColumnShare / 100

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

private def mascotLines : List String :=
  [ " ▄▄▄▄▄▄▄▄ "
  , "██  ██  ██"
  , "██████████"
  , "▀█▀    ▀█▀" ]

private def mascot (theme : ColorScheme) : Text :=
  joinLines (mascotLines.map (fun line => Text.styled line (Style.fg theme.orange)))

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
    Text.plain "\n" ++
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
  | history (rows : List (String × String))
  | theme (current : String)
  | widgets (frame : Nat)

/-- One line of the conversation. -/
inductive Entry where
  /-- What the user typed. -/
  | ask (cell : Nat) (input : String)
  /-- A formatted result. -/
  | answer (cell : Nat) (value : String)
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

/-- The status line under the prompt. -/
def footerView (theme : ColorScheme) (width : Nat) (themeName : String) (ans : Float) : Text :=
  let outer := frameWidth width
  let leftWidth := outer * 2 / 3
  columns [leftWidth, outer - leftWidth] 0
    [ Text.styled "[CALC:READY]" (Style.bold <+> Style.fg theme.orange) ++
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

/-- Brightness sweep used by the thinking indicator and the shimmer showcase. -/
def shimmerConfig (theme : ColorScheme) : ShimmerConfig :=
  { base := theme.comment, highlight := theme.orange, band := 5, phaseStep := 1 }

/-- Column widths for the showcase table. -/
def showcaseTableWidths (width : Nat) : List Nat :=
  let inner := boxInnerWidth (panelWidth width) - tableGap
  let left := max minColumnWidth (splitLeft inner)
  [left, max minColumnWidth (inner - left)]

/-- A spinner frame plus a shimmering label, for one animation frame. -/
def thinkingView (theme : ColorScheme) (frame : Nat) : Text :=
  indentText askIndent ++ spinnerFrame (spinnerConfig theme) frame ++ Text.plain " " ++
    shimmer (shimmerConfig theme) { frame } (Text.plain "Computing…")

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
  panel theme width "help" (renderTable (panelTableWidths width helpKeyWidth)
    [ headerRow theme "input" "meaning"
    , [Text.plain "2+3*4   (1+2)^5", Text.plain "arithmetic, usual precedence"]
    , [Text.plain "-3^2    2^-2", Text.plain "unary minus, right-associative ^"]
    , [Text.plain "10/4    7%3", Text.plain "division and modulo"]
    , [Text.plain "sqrt abs floor ceil", Text.plain "round ln exp sin cos tan"]
    , [Text.plain "pi tau e ans", Text.plain "constants and the last result"]
    , [Text.plain "[n]", Text.plain "reuse cell n's result or expression"]
    , [command "/help  /history", Text.plain "this table, past results"]
    , [command "/showcase", Text.plain "run every live widget in the stack"]
    , [command "/theme <name>", Text.plain themeNames]
    , [command "/load <path>", Text.plain "evaluate one expression from a file"]
    , [command "ctrl-n", Text.plain "insert a line break; enter evaluates"]
    , [command "/clear /quit", Text.plain "reset the chat, leave"] ] tableGap)

/-- The `/history` panel. Rows are newest last. -/
def historyView (theme : ColorScheme) (width : Nat) (rows : List (String × String)) : Text :=
  if rows.isEmpty then
    panel theme width "history" (Text.styled "Nothing evaluated yet."
      (Style.dim <+> Style.fg theme.comment))
  else
    let inner := boxInnerWidth (panelWidth width) - tableGap
    let leftWidth := max minPaneWidth (splitLeft inner)
    panel theme width "history" (renderTable [leftWidth, max minColumnWidth (inner - leftWidth)]
      (headerRow theme "expression" "result" ::
        rows.map (fun (input, value) =>
          [ Text.styled input (Style.fg theme.foreground)
          , Text.styled value (Style.fg theme.green) ])) tableGap [.left, .right])

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
the same configuration through `LiveProgress`, `LiveIndeterminateProgress`, `LiveSpinner`,
`LiveShimmer`, `LiveStatus`, and `LiveTable`. -/
def widgetGallery (theme : ColorScheme) (width : Nat) (frame : Nat) : Text :=
  panel theme width "widgets" (joinLines
    [ progressBar (progressConfig theme width)
        { current := 7, total := 10, label := Text.styled "progress" (Style.fg theme.foreground) }
    , indeterminateProgressBar (indeterminateConfig theme width)
        { frame, label := Text.styled "indeterminate" (Style.fg theme.foreground) }
    , renderSpinner (spinnerConfig theme)
        { frame, label := Text.styled "spinner" (Style.fg theme.foreground) }
    , shimmer (shimmerConfig theme) { frame } (Text.plain "shimmer")
    , renderStatus .success (Text.styled "status" (Style.fg theme.green))
    , renderTable (showcaseTableWidths width)
        [ headerRow theme "table" "value"
        , [Text.plain "rows wrap", Text.styled "yes" (Style.fg theme.green)] ] tableGap ])

private def cellMarker (theme : ColorScheme) (cell : Nat) (symbol : String) (style : Style) : Text :=
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
  | .answer _ value =>
      gutter (resultMarker "=" (Style.fg theme.green)) answerIndent inner
        (Text.styled value (Style.bold <+> Style.fg theme.green))
  | .failure _ sources diagnostic =>
      gutter (resultMarker "!" (Style.bold <+> Style.fg theme.red)) answerIndent inner
        (diagnosticView theme width sources diagnostic)
  | .note body => gutter (indentText askIndent) askIndent inner (noteView theme width body)

end Calc
