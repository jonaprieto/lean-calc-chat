import Lake
open Lake DSL

package «calc-chat» where
  version := v!"0.5.10"
  leanOptions := #[⟨`autoImplicit, false⟩, ⟨`relaxedAutoImplicit, false⟩]

require «termcolor-terminal» from git
  "https://github.com/jonaprieto/lean-termcolor-terminal.git"
  @ "v0.3.8"

require «termcolor-widgets» from git
  "https://github.com/jonaprieto/lean-termcolor-widgets.git"
  @ "v0.1.15"

require «termcolor-diagnostics» from git
  "https://github.com/jonaprieto/lean-termcolor-diagnostics.git"
  @ "v0.1.18"

require «grip» from git
  "https://github.com/jonaprieto/lean-grip.git"
  @ "v0.3.6"

require «grip-diagnostics» from git
  "https://github.com/jonaprieto/lean-grip-diagnostics.git"
  @ "v0.2.9"

require «termcolor-repl» from git
  "https://github.com/jonaprieto/lean-termcolor-repl.git"
  @ "v0.8.8"

require argus from git
  "https://github.com/jonaprieto/lean-argus.git"
  @ "v0.5.5"

@[default_target]
lean_lib «Calc» where
  roots := #[`Calc]
  globs := #[.andSubmodules `Calc]

lean_exe «calc» where
  root := `Main
  srcDir := "app"
