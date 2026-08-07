import Lake
open Lake DSL

package «calc-chat» where
  version := v!"0.5.0"
  leanOptions := #[⟨`autoImplicit, false⟩, ⟨`relaxedAutoImplicit, false⟩]

require «termcolor-terminal» from git
  "https://github.com/jonaprieto/lean-termcolor-terminal.git"
  @ "v0.1.11"

require «termcolor-diagnostics» from git
  "https://github.com/jonaprieto/lean-termcolor-diagnostics.git"
  @ "v0.1.10"

require «grip» from git
  "https://github.com/jonaprieto/lean-grip.git"
  @ "main"

require «grip-diagnostics» from git
  "https://github.com/jonaprieto/lean-grip-diagnostics.git"
  @ "v0.2.3"

require «termcolor-repl» from git
  "https://github.com/jonaprieto/lean-termcolor-repl.git"
  @ "main"

@[default_target]
lean_lib «Calc» where
  roots := #[`Calc]
  globs := #[.andSubmodules `Calc]

lean_exe «calc» where
  root := `Main
  srcDir := "app"
