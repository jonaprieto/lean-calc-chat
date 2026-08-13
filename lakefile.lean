import Lake
open Lake DSL

package «calc-chat» where
  version := v!"0.5.9"
  leanOptions := #[⟨`autoImplicit, false⟩, ⟨`relaxedAutoImplicit, false⟩]

require «termcolor-terminal» from git
  "https://github.com/jonaprieto/lean-termcolor-terminal.git"
  @ "v0.3.6"

require «termcolor-widgets» from git
  "https://github.com/jonaprieto/lean-termcolor-widgets.git"
  @ "v0.1.13"

require «termcolor-diagnostics» from git
  "https://github.com/jonaprieto/lean-termcolor-diagnostics.git"
  @ "v0.1.16"

require «grip» from git
  "https://github.com/jonaprieto/lean-grip.git"
  @ "v0.3.4"

require «grip-diagnostics» from git
  "https://github.com/jonaprieto/lean-grip-diagnostics.git"
  @ "v0.2.7"

require «termcolor-repl» from git
  "https://github.com/jonaprieto/lean-termcolor-repl.git"
  @ "v0.8.6"

require argus from git
  "https://github.com/jonaprieto/lean-argus.git"
  @ "v0.5.3"

@[default_target]
lean_lib «Calc» where
  roots := #[`Calc]
  globs := #[.andSubmodules `Calc]

lean_exe «calc» where
  root := `Main
  srcDir := "app"
