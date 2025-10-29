# AGENTS.md

Guidance for coding agents. Learn more: <https://agents.md/>.

## About

This repository contains the source code for Indigo, a work-in-progress,
terminal-native modal code editor, inspired by [Kakoune] and others.

[Kakoune]: https://github.com/mawww/kakoune

## Project Structure

Indigo is implemented in Rust, across multiple crates in a Cargo workspace.

- **indigo-core:** The core types and functions defining the behavior of the
  editor. A thick functional core performing no I/O.

- **indigo-tui:** A terminal user interface (TUI) is the primary, interactive
  frontend. Responsible for the appearance of the editor. Handles I/O as a
  relatively thin imperative shell.

- **indigo-cli:** A command-line interface (CLI) is the secondary,
  non-interactive/headless frontend. Handles I/O as an very thin imperative
  shell.

- **indigo-event:** Incoming events and the actions they trigger, represented as
  plain data, abstracted from any one frontend.

- **indigo-wrap:** Limited form of higher-kinded types providing generic wrapper
  types and generic reference mutability. Many "view types" in `indigo-core`
  make use of "mutability generics."

## Code Conventions

- Bias towards turning implicit assumptions into explicit assertions or comments
  in code.
- Use `camino::{Utf8Path, Utf8PathBuf}` instead of `std::path::{Path, PathBuf}`.
