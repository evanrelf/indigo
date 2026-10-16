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

- Break complex changes into small, incremental steps that are easy to review
  and commit individually. For example, you could implement a feature in
  multiple passes: first something simple and self-evidently correct, then
  address edge cases or apply optimizations in later passes. The user wants to
  review and understand all the code in this project, and regular pauses allow
  for steering and asking questions.
- Keep the core crates (`indigo-kernel`, `indigo-core`) deterministic. For
  example, `Hash{Map,Set}` must use `indigo_kernel::IndigoBuildHasher`. If you
  notice existing sources of nondeterminism, call them out so we can fix it.
- The `indigo-kernel` crate prefers GHC-style note comments, albeit sparingly.
  See its `edit` and `grapheme` modules for examples.
- Prefer `std::{cmp::{min,max}, iter::zip}` functions over method variants.

## Checking your work

- Always run `cargo clippy --all-targets` to check if code compiles. Never run
  `cargo check` or `cargo build`.
- Some crates may disable tests when feature flags aren't enabled. Err towards
  running `cargo test` from the root of the repo to include as much as possible.
- Prefer writing property tests with Hegel over unit tests. Read the tests in
  `indigo_kernel`'s `edit` and `merge` modules for good examples.
- Run `./bin/run-agent` with `rex` (or `tmux` as fallback) to control the TUI.
- Run `./bin/run-cli` to control the headless CLI. Prefer this over the TUI if
  you don't need interactive control, or aren't making UI changes. Read
  `crates/indigo-cli/README.md` for detailed instructions.
- Run the `bin/compare-kak-suite` integration suite to compare Kakoune and
  Indigo behavior when you finish working on changes that affect editing
  behavior. If you feel strongly that a new test case would be highly valuable,
  you have permission to extend the suite.
- Don't mention if you elect not to run a test (e.g. `bin/compare-kak-suite`).
