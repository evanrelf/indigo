# indigo

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
