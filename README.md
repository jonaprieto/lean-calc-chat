# calc-chat

[![CI](https://github.com/jonaprieto/lean-calc-chat/actions/workflows/ci.yml/badge.svg)](https://github.com/jonaprieto/lean-calc-chat/actions/workflows/ci.yml)
[![Lean 4](https://img.shields.io/badge/Lean%204-library-5f5f5f)](lean-toolchain)
[![License](https://img.shields.io/badge/license-Apache--2.0-green)](LICENSE)

Chat-shaped terminal calculator in Lean 4. It combines Grip parsing, pure TermColor rendering,
diagnostics, widgets, terminal input, themes, history, and concurrent background evaluations.

Version: `v0.5.0`

## Run

```sh
lake build calc
lake exe calc
lake exe calc "2+3*4"
CALC_THEME=dracula lake exe calc
CALC_NONINTERACTIVE=1 lake exe calc
```

On a TTY, `Enter` evaluates, `Esc` quits, arrow keys recall history, `Tab` completes commands,
and `Ctrl-N` inserts a line break. Submitted expressions run independently; completed results
merge into the current transcript by cell. `/help`, `/history`, `/showcase`, `/theme`, `/clear`,
and `/quit` are available commands.

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

[`termcolor-repl`](https://github.com/jonaprieto/lean-termcolor-repl) supplies reusable input and
job-loop primitives. [`termcolor-terminal`](https://github.com/jonaprieto/lean-termcolor-terminal)
owns terminal control.

## License

Apache-2.0.
