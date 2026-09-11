# Indigo

This is a work-in-progress terminal-based modal text editor, chiefly inspired
by Kakoune and CodeMirror.

## Crates

- `indigo-core`: functional core, most code lives here.
- `indigo-kernel`: part of the functional core, in flux. A more ambitious and
  theoretically sound rethinking of the architecture embracing eventual
  consistency.
- `indigo`: interactive TUI / primary imperative shell.
- `indigo-cli`: non-interactive CLI / tiny imperative shell, currently for
  debugging and headless driving by coding agents.
- `indigo-wrap`: higher-kinded types emulation / mutability generics, powers the
  `{Cursor,Window,etc}View` pattern pervasive in `indigo-core`.
- `indigo-term`: low-level terminal manipulation library, currently unused and
  unfinished.
- `indigo-tui`: high-level terminal UI library, currently unused and unfinished.

## Code style

- The `indigo-kernel` crate prefers GHC-style note comments, albeit sparingly.
  See its `edit` and `grapheme` modules for examples.
- Prefer `std::{cmp::{min,max}, iter::zip}` functions over method variants.

## Checking your work

- Always run `cargo clippy --all-targets` to check if code compiles. Never run
  `cargo check` or `cargo build`.
- Prefer writing property tests with Hegel over unit tests. Read the tests in
  `indigo_kernel`'s `edit` and `merge` modules for good examples.
- Never run `indigo`; you cannot control it. Run `indigo-cli` instead to test
  editor behavior. Read `crates/indigo-cli/README.md` for detailed instructions.
- Run the `bin/compare-kak-suite` integration suite to compare Kakoune and
  Indigo behavior when you finish your work (takes a while to run). If you feel
  strongly that a new test case would be highly valuable, you have permission to
  extend the suite.
