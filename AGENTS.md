# AGENTS.md

Guidance for coding agents. Learn more: <https://agents.md/>.

## About

This repository contains the source code for [Indigo], a work-in-progress,
terminal-native modal code editor, inspired by [Kakoune] and others.

[Indigo]: https://github.com/evanrelf/indigo
[Kakoune]: https://github.com/mawww/kakoune

The current goal is to build a minimally viable text editor, such that I can use
it to do real work at my job. To that end, the focus is on implementing table
stakes features.

If the [Blue] project is ever mentioned, that is the most recent
rewrite/offshoot of Indigo. I was experimenting with implementing things more
simply. That experiment is now done, and I'm incorporating code and lessons from
that back into Indigo. I typically have the code checked out next to Indigo
(i.e. `cd ../blue`); you're encouraged to reference that code if I mention it.

[Blue]: https://github.com/evanrelf/blue

## Project Structure

Indigo is implemented in Rust, across multiple crates in a Cargo workspace.

- **indigo-core:** The core types and functions defining the behavior of the
  editor. A thick functional core performing no I/O.

- **indigo-tui:** A terminal user interface (TUI) is the primary, interactive
  frontend. Responsible for the appearance of the editor. Handles I/O as a
  relatively thin imperative shell.

- **indigo-cli:** A command-line interface (CLI) is the secondary,
  non-interactive/headless frontend. Handles I/O as a very thin imperative
  shell.

- **indigo-wrap:** Limited form of higher-kinded types providing generic wrapper
  types and generic reference mutability. Many "view types" in `indigo-core`
  make use of "mutability generics."

## Checking Your Work

- Run `cargo clippy` as you work to check whether your code compiles. Focus on
  addressing compilation errors first, then worry about warnings later. Do not
  run `cargo check` or `cargo build`; `cargo clippy` is faster and more
  comprehensive.

- Do not run `indigo-tui`; it is a TUI application that you will struggle to
  control and understand. Instead, check runtime behavior by writing tests, or
  writing new binaries (e.g. `src/bin/my_program.rs`) if you specifically need
  to inspect output prior to determining the desired behavior.

## Code Conventions

- Bias towards turning implicit assumptions into explicit assertions or comments
  in code.

- Use `camino::{Utf8Path, Utf8PathBuf}` instead of `std::path::{Path, PathBuf}`.

- When working with event handling code, try to keep each key or mouse mapping
  on a single line for readability. If a key maps to multiple lines of code
  rather than a single function call, that's a smell; most logic should be
  abstracted away by the editor core.
