# TODO

The reusable input/history/completion core now lives in
[`lean-termcolor-repl`](https://github.com/jonaprieto/lean-termcolor-repl).

- [ ] Move `readKeyWithResize` and the generic interactive loop into
  `termcolor-repl`.
- [ ] Add a redraw/suspend API for `/showcase` and other transient live commands.
- [ ] Keep calculator-specific transcript, diagnostics, themes, and evaluation here.
