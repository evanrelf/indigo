# AGENTS.md

Guidance for coding agents. Learn more: <https://agents.md/>.

## About

This repository contains the source code for Indigo, a work-in-progress,
terminal-native modal code editor, inspired by [Kakoune] and others.

[Kakoune]: https://github.com/mawww/kakoune

## Values

See [docs/zen.md](docs/zen.md).

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

  I definitely want a non-interactive frontend for scripting purposes, but in
  the future I imagine this functionality would be provided in the same binary
  as the TUI. The separate crate is currently more useful as a design
  constraint: the core should strive not to implement anything specific to one
  frontend.

- **indigo-event:** Incoming events and the actions they trigger, represented as
  plain data, abstracted from any one frontend.

  The idea is that in the future, events and actions could be serialized and
  sent across processes, across a network boundary, persisted to disk, etc.

- **indigo-wrap:** Limited form of higher-kinded types providing generic wrapper
  types and generic reference mutability. Many "view types" in `indigo-core`
  make use of "mutability generics."

## Important Concepts

- **Extended grapheme clusters:** TODO use graphemes or die

- **Byte vs grapheme vs display width:** TODO custom `DisplayWidth`

- **Indexes vs offsets:** TODO index is on, offset is between, prefer offset

- **Ropes:** TODO discontiguous

- **Operational transformation:** TODO `Edit` `EditSeq` only way to edit `Text`

## Important Libraries

- **ropey:** TODO char indexes

- **ratatui:** TODO don't use `Widget` trait or most built-in widgets

- **crossterm:** TODO terminal events

## Code Conventions

- Bias towards turning implicit assumptions into explicit assertions in code.
- Use `camino::{Utf8Path, Utf8PathBuf}` instead of `std::path::{Path, PathBuf}`.
