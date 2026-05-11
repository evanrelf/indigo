# indigo-core

Implements the core types and functions defining editor behavior. The goal is to
put the majority of logic in this crate, and not perform any direct I/O (i.e.
functional core).

- `cursor`: A gap between graphemes in text that you can move around and edit
  text with.
- `range`: Combines two cursors to select text (i.e. a single selection).
- `selection`: Holds many ranges (i.e. multiple selections).
- `rope`: Extensions to the `ropey` crate's [rope](https://en.wikipedia.org/wiki/Rope_(data_structure)),
  including identifying and moving between character and grapheme boundaries,
  and computing the width of text when displayed in a terminal.
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
  - `normal`: Vim-style normal mode where keys are commands that move you
    around, perform high-level editing operations, etc.
  - `insert`: Inserts the keys you type into the text.
  - `command`: Command prompt for Vim-style commands like `:wq` and such.
  - `prompt`: Prompt for input, currently used for narrowing a selection to a
    regex and running commands.
  - `seek`: `f`- and `t`-style one-dimensional movement.
- `key`: Types representing keys, and parsers from a textual representation.
  Currently the types are used for input events, and the parsers are used by the
  CLI.
- `keymap`: Trie-based mapping from key sequences to values, for multi-key
  bindings.
- `fs`: Abstract filesystem interface. Keeps the core morally pure, and allows
  swapping out the implementation for testing.
- `attributes`: Work in progress. Arbitrary annotations for ranges inspired by
  Emacs. Plan is to use this for syntax highlighting and other stuff (tags?
  read-only text? who knows).
- `btree`: Copy-on-write B+ tree-backed ordered map.

## Terminology

- **Index**: An unsigned integer identifying a specific element in a sequence.
  For example: identify a `\n` byte in some text.
- **Offset**: An unsigned integer identifying a gap between elements in a
  sequence. For example: position a cursor after a `\n`.
- **Bias**: Either "before" or "after", attaching an offset to an element in a
  sequence. For example: determine whether a cursor is at the end of one line or
  at the start of the next (<https://lord.io/text-editing-hates-you-too/#affinity>).
