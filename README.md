# calc-chat

[![CI](https://github.com/jonaprieto/lean-calc-chat/actions/workflows/ci.yml/badge.svg)](https://github.com/jonaprieto/lean-calc-chat/actions/workflows/ci.yml)
[![Release](https://img.shields.io/github/v/release/jonaprieto/lean-calc-chat?display_name=tag&sort=semver)](https://github.com/jonaprieto/lean-calc-chat/releases)
[![Lean 4](https://img.shields.io/badge/Lean%204-v4.33.0-6f42c1)](lean-toolchain)
[![Docs](https://img.shields.io/badge/docs-GitHub%20Pages-4c8bf5)](https://jonaprieto.github.io/lean-calc-chat/)
[![License](https://img.shields.io/badge/license-Apache--2.0-green)](LICENSE)

Chat-shaped terminal calculator in Lean 4. It combines Grip parsing, pure TermColor rendering,
diagnostics, widgets, terminal input, themes, history, and concurrent background evaluations.

<p align="center"><img src="docs/assets/calc-chat.png" alt="Calc Chat demo" width="700"></p>

## Run

```sh
lake build calc
lake exe calc
lake exe calc "2+3*4"
CALC_THEME=dracula lake exe calc
CALC_NONINTERACTIVE=1 lake exe calc
```

The default palette is `aurora`; `CALC_THEME` or `/theme` can select another palette.

On a TTY, `Enter` evaluates, `Esc` quits, arrow keys recall input history, `Tab` completes
commands, and `Ctrl-N` inserts a line break. Submitted expressions run independently; completed
results merge into the current transcript by cell, with evaluation time shown on each answer.
`/help`, `/history`, `/showcase`, `/theme`,
`/clear`, and `/quit` are available commands. `/history` toggles the side drawer when the window
is wide enough; uppercase `H` and `K` focus the calculator and history columns. With history
focused, arrow keys or the mouse wheel scroll it; clicking either pane changes focus.

Supported operators are `+ - * / % ^`; built-in functions include `sqrt`, `abs`, `floor`, `ceil`,
`round`, `ln`, `exp`, `sin`, `cos`, and `tan`. Constants include `pi`, `tau`, `e`, and `ans`.

## Build

```sh
lake build calc
CALC_NONINTERACTIVE=1 lake exe calc
```

`Calc.Eval` contains the byte parser and evaluator. `Calc.View` is pure width-aware rendering;
`app/Main.lean` owns terminal input and background job execution.

## Related projects

Built on [`grip`](https://github.com/jonaprieto/lean-grip),
[`grip-diagnostics`](https://github.com/jonaprieto/lean-grip-diagnostics),
[`termcolor-diagnostics`](https://github.com/jonaprieto/lean-termcolor-diagnostics),
[`termcolor-widgets`](https://github.com/jonaprieto/lean-termcolor-widgets),
[`termcolor-terminal`](https://github.com/jonaprieto/lean-termcolor-terminal), and
[`argus`](https://github.com/jonaprieto/lean-argus). [`termcolor-repl`](https://github.com/jonaprieto/lean-termcolor-repl)
supplies reusable input and job-loop primitives.

## License

Apache-2.0.
