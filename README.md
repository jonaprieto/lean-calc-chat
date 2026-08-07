# lean-calc-chat

A chat-shaped calculator for the terminal, written in Lean 4. Banner, scrolling transcript, rounded
prompt bar, status footer, slash commands, live widgets, four switchable themes.

It exists to answer one question: **how much of a real terminal UI can you build out of the
[`termcolor`](https://github.com/jonaprieto/lean-termcolor) stack before you have to write terminal
code yourself?** The answer, and the places where the answer is "not yet", are recorded below.

The expression grammar is a [grip](https://github.com/jonaprieto/lean-grip) byte parser, so parse
failures come back positioned and render with a caret.

```sh
lake build calc
lake exe calc                    # interactive chat on a TTY, static transcript otherwise
lake exe calc "2+3*4"            # one-shot: prints 14
lake exe calc --help
CALC_THEME=dracula lake exe calc
CALC_NONINTERACTIVE=1 lake exe calc
```

```
┌ Lean Calc v0.5.0 ──────────────────────────────────────────────────────────┐
│               Welcome back!                │ Tips for getting started      │
│                                            │ Type an expression: 2+3*4     │
│                  ▄▄▄▄▄▄▄▄                  │ ans reuses the last result    │
│                 ██  ██  ██                 │                               │
│                 ██████████                 │ What's new                    │
│                 ▀█▀    ▀█▀                 │ /showcase runs the widgets    │
│                                            │ /theme repaints the UI        │
│          Lean 4 • termcolor stack          │ enter evaluates • esc quits   │
│      github.com/jonaprieto/lean-termcolor  │                               │
└────────────────────────────────────────────────────────────────────────────┘

[1] › 10/4 + 7%3
  = 3.5
[2] › 2 * (3 +
  ! error: parse error
      ╰─> input:1:3
      │
    1 │ 2 * (3 +
      │   ^ expected an operator or the end of the expression

╭────────────────────────────────────────────────────────────────────────────╮
│ › sqrt(2)                                                                  │
╰────────────────────────────────────────────────────────────────────────────╯
[CALC:READY]  ans = 3.5  •  theme terracotta                             /help
```

The banner collapses to a one-line header once the conversation starts, so the transcript gets the
rows back. The frame uses the available terminal width and redraws after a live resize. `/clear`
brings the banner back. Input cells are numbered; their outputs and errors inherit that cell id.

## Using it

`enter` evaluates; `esc` quits immediately.
`up`/`down` recall submitted inputs; `tab` completes commands and theme options adaptively.
`ctrl-n` inserts a line break without submitting a multiline expression.
Interactive evaluations run as independent background jobs, so input and resize redraws remain
responsive while earlier cells finish; results merge back into the current transcript by cell.

The reusable input/history/completion core is maintained in
[`lean-termcolor-repl`](https://github.com/jonaprieto/lean-termcolor-repl); terminal-loop
and transient-command lifecycle now live there too.

| command | what it does |
| --- | --- |
| `/help` | the table of everything below |
| `/history` | every expression and result so far |
| `/showcase` | runs `LiveProgress`, `LiveIndeterminateProgress`, `LiveSpinner`, `LiveShimmer`, `LiveStatus`, and `LiveTable` in sequence |
| `/theme <name>` | `terracotta`, `catppuccin`, `dracula`, `monokai` -- repaints immediately |
| `/clear` | empty the transcript |
| `/quit` | leave |

Operators are `+ - * / % ^` with the usual precedence, `^` right-associative and binding tighter
than unary minus: `-3^2` is `-9`, `2^3^2` is `512`, `2^-2` is `0.25`. Functions: `sqrt abs floor
ceil round ln exp sin cos tan`. Constants: `pi tau e ans`.

Use `[n]` in a later expression to reuse cell `n`'s result; cells without a result reuse their
input expression.

## Layout

| file | contents |
| --- | --- |
| `Calc/Eval.lean` | grip grammar, evaluator, result formatting, self-check |
| `Calc/View.lean` | every pixel, as pure `Text` at a given width and palette. No IO |
| `app/Main.lean` | terminal, keyboard, clock, run modes |

`Calc.View` is total and pure: it takes the `ColorScheme` it draws with and returns `Text`, so the
whole interface renders into a string with no terminal attached, and `/theme` is a field update
rather than global state. `Main` is the only file that knows a terminal exists.

Every width comes from a named metric (`frameWidth`, `boxInnerWidth`, `askIndent`, `panelWidth`,
`splitLeft`, ...) rather than an inline constant, because the border-and-padding arithmetic has to
agree in eight places at once.

## How far the stack reaches

The direct requirements pull the TermColor UI stack plus Grip's source diagnostics adapter;
`termcolor`, `-layout`, and `-widgets` come in transitively.

- **termcolor** -- `Text`/`Segment`, `Style` (`fg`/`bold`/`dim`/`italic`/`underline`/`reverse`,
  combined with `<+>`), `Color.rgb`, `ColorScheme` and all three built-in palettes plus one of our
  own, `Text.hyperlink` (the banner link is a real OSC 8 hyperlink), and `TermColor.render` behind
  `writeTextLine` for `NO_COLOR`/`FORCE_COLOR` handling and truecolor-to-256 downgrading.
- **termcolor-layout** -- `box` (with custom `BoxChars` for the rounded prompt bar), `columns`,
  `align`, `truncate`, `padRight`, `wrapLines`, `splitLines`, `joinLines`, `Text.width`,
  `Text.height`. Every width is display width, so the box drawing survives `•`, `…`, and `█`.
- **termcolor-widgets** -- `progressBar`, `indeterminateProgressBar`, `renderSpinner`,
  `spinnerFrame`, `shimmer`/`ShimmerConfig`, `renderStatus`, `renderTable`, and
  `TextInputState`/`updateTextInput`/`Key` for the prompt's editing model.
- **termcolor-terminal** -- `clearScreen`, `hideCursor`/`showCursor`, `writeTextLine`,
  `terminalSize`/`terminalWidth`, `stdoutSupportsControl`, `stdinIsTty`/`stdoutIsTty`,
  `withRawInput`, `readKey`, `LiveRegion`, and every live object: `LiveProgress`,
  `LiveIndeterminateProgress`, `LiveSpinner`, `LiveShimmer`, `LiveStatus`, `LiveTable`.
- **termcolor-diagnostics** -- source spans, themed error/help rendering, context lines, and
  width-aware caret diagnostics as pure `Text`.
- **grip-diagnostics** -- the byte-offset adapter from Grip parser failures to TermColor
  diagnostics.
- **termcolor-repl** -- pure input history, key handling, and adaptive completion; the
  calculator still owns its terminal runner and transcript model.
- **grip** -- `GParser.fix`, `dispatch`, `many`, `map2`, `optional`, `capture`, `captureWith?`,
  `takeWhile1`, `oneOf`, `ws`, `ch`, `eof`, `<?>`, `GParser.parse`, `ParseError.pretty`. The whole
  grammar is `partial`-free: recursion is `fix`, repetition is `many`, and `many`'s always-consume
  grade is checked at compile time.

Still unused: `renderSlider`, `renderCheckbox`, `renderButton`, `moveFocus`, `parseKey`, and the
alternate screen. A calculator has no form to fill in, and entering the alternate screen only to
`clearScreen` every frame buys nothing.

## Stack status

The upstream releases now cover the former layout, screen, input, terminal-size, and control-key
gaps. This project uses `Layout.boxInnerWidth`, `Layout.gutter`, column separators,
`Widgets.textInputBody`, and `Terminal.Screen` directly.

Two calculator-specific gaps remain: `Float` formatting is limited by Lean's `Float.toString`, and
Grip still loses the furthest failure position inside a repetition. Those belong to the calculator
and parser layers, not the TermColor stack.

`lake build` and `lake exe calc` are the supported entry points. Raw `lake env lean --run` scratch
files that import multiple `TermColor.*` satellite packages still hit the upstream Lake `LEAN_PATH`
module-path limitation ([termcolor#11](https://github.com/jonaprieto/lean-termcolor/issues/11)).

## Checks

`Calc/Eval.lean` ends in a `selfCheck` covering precedence, both associativities, unary minus, the
function table, and every failure path. It is discharged with `native_decide` rather than `decide`
because Lean's kernel does not reduce `Float`. `lake build` fails if it regresses.

## License

Apache-2.0.
