# Guidance for coding agents

## About

[Indigo] is a work-in-progress, terminal-native modal code editor, inspired by
[Kakoune].

[Indigo]: https://github.com/evanrelf/indigo
[Kakoune]: https://github.com/mawww/kakoune

## Project structure

Indigo is written in Rust, across multiple crates in a Cargo workspace.

### indigo-core

Implements the core types and functions defining editor behavior. The goal is to
put the majority of logic in this crate, and not perform any direct I/O (i.e.
functional core).

- `cursor`: A gap between graphemes in text that you can move around and edit
  text with.
- `range`: Combines two cursors to select text (i.e. a single selection).
- `selection`: Holds many ranges (i.e. multiple selections).
- `unicode`: Identify and move between character and grapheme boundaries.
- `rope`: Extensions to the `ropey` crate's [rope](https://en.wikipedia.org/wiki/Rope_(data_structure)).
- `ot`: Operational transformation implementation. Today it's used to reconcile
  concurrent edits from multiple cursors and track editing history for
  undo/redo. In the future it could be used to apply changes from other editor
  instances, external code formatters, coding agents, or other humans.
- `history`: Generic transactional history of events. In practice it's used to
  store OT operations to implement undo/redo.
- `text`: Wrapper around `Rope` that only allows mutation via OT operations,
  so they can be logged for undo/redo history and catching up stale peers.
- `buffer`: Represents an in-memory or on-disk file, combining text with a
  selection, and maybe a path, modification state, etc depending on the kind.
- `window`: A viewport into a buffer, currently only tracking scroll position.
- `editor`: Top-level editor state, owns everything.
- `mode`: Modal editing, like Vim/Kakoune/Helix.
  - `command`: Command prompt for Vim-style commands like `:wq` and such.
  - `goto`: Jump around to the start/end of the line, buffer, etc while
    optionally dragging your selection.
  - `insert`: Inserts the keys you type into the text.
  - `normal`: Vim-style normal mode where keys are commands that move you
    around, perform high-level editing operations, etc.
  - `prompt`: Prompt for input, currently used for narrowing a selection to a
    regex.
  - `seek`: `f`- and `t`-style one-dimensional movement.
- `display_width`: The width of text when displayed in a terminal, in terms of
  cells.
- `key`: Types representing keys, and parsers from a textual representation.
  Currently the types are used for input events, and the parsers are used by the
  CLI.
- `event`: Things that have happened. Currently just represents keyboard input
  events, provided by frontends (either by parsing strings or converting
  `crossterm` types).
- `fs`: Abstract filesystem interface. Keeps the core morally pure, and allows
  swapping out the implementation for testing.
- `attributes`: Work in progress. Arbitrary annotations for ranges inspired by
  Emacs. Plan is to use this for syntax highlighting and other stuff (tags?
  read-only text? who knows).

### indigo-tui

Implements an interactive TUI with the `ratatui` and `crossterm` crates. The
goal is to keep this relatively lightweight, keeping all non-trivial logic in
`indigo-core` (i.e. imperative shell).

- `terminal`: Application-agnostic TUI setup and teardown, including enabling
  modern terminal features like mouse support and richer key input events.
- `key`: Conversion between `crossterm` and `indigo-core` keys.
- `event`: Event handling. Most are passed directly to the core, but some are
  handled partially or completely by the TUI. For example, scrolling is depenent
  on terminal size, mouse input requires conversion from terminal cells to
  buffer byte offsets, etc.
- `areas`: Constraints dividing the terminal into separate user interface areas,
  as well as mappings between terminal cell positions and byte offsets in text,
  used for rendering and converting mouse clicks into cursor movement.
- `main`: Entrypoint for the program. Includes parsing command-line arguments,
  setting up XDG directories, setting up tracing, setting up the terminal, an
  event loop, and rendering state to the terminal. More or less follows The Elm
  Architecture.

### indigo-cli

Implements a headless CLI executing keys passed as command-line arguments. Read
more about how this works and its utility for testing in `docs/cli.md`.

### indigo-wrap

Implements a limited form of higher-kinded types / generic wrapper types /
"mutability generics". This is an important primitive powering the "view types"
you'll find throughout `indigo-core` (e.g. `CursorView`, `WindowView`, etc).

## Terminology

- **Index**: An unsigned integer identifying a specific element in a sequence.
  For example: identify a `\n` byte in some text.
- **Offset**: An unsigned integer identifying a gap between elements in a
  sequence. For example: position a cursor after a `\n`.
- **Bias**: Either "before" or "after", attaching an offset to an element in a
  sequence. For example: determine whether a cursor is at the end of one line or
  at the start of the next (<https://lord.io/text-editing-hates-you-too/#affinity>).

## Checking your work

- Always run `cargo clippy` to check if code compiles. Never run `cargo check`
  or `cargo build`.

- Never run `indigo-tui`; you cannot control it. Always check runtime behavior
  by writing tests or executables at `crates/<crate>/src/bin/<name>.rs`. Always
  run these ad-hoc executables with `cargo run --bin <name>`, never with `rustc`
  (no dependencies outside of Cargo workspace).

- Run `indigo-cli` to test editor behavior. With the `--debug` flag, the `<c-l>`
  key prints information and a diff. Try running this command as an example:

  ```
  $ echo -ne "hello\nworld\n" | ./bin/cli --debug 'fod<c-l>iHELLO<c-l><esc>jgla!<c-l>'
  ```
