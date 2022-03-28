# Indigo

A text editor

## Notes

### Inspiring Projects

#### [Kakoune][kakoune]

- Text editor written in C++ with no dependencies
- Like
  - Selection before action
  - Multiple cursors
  - Interactive help
  - UNIX philosophy (no windowing, no `:sort` command, etc.)
- Dislike
  - Configuration language isn't expressive or ergonomic enough to write
    non-trivial plugins
  - Regex-based syntax highlighting
  - UI freezes when piping text to/from other processes
  - Not interested in forking or contributing upstream because I don't like
    using C++

#### [Helix](https://github.com/helix-editor/helix)

- Text editor written in Rust
- Core text editing logic inspired by [CodeMirror 6][codemirror6]
- Like
  - Copies features I like from [Kakoune][kakoune] and [Neovim](https://neovim.io/)
  - Uses Tree-sitter for syntax highlighting
- Dislike
  - Too many IDE-like features for my taste
  - Avoids using Alt, but I don't mind how Kakoune uses it
  - Copies how Vim handles uppercase/shifted letters, but I prefer how Kakoune
    handles it (extends the selection)
  - Copies Vim's visual mode, but I like Kakoune's lack of visual mode
  - Not sure its vision is focused (or exists); the work of many contributors
    without a benevolent dictator with a vision (I could be wrong)

#### [Xi](https://github.com/xi-editor/xi-editor)

- Text editor written in Rust
- Like
  - Conflict resolution logic using [CRDTs][crdt] (for rectifying disparate
    changes from external processes, such as the UI and plugins)
  - Rope data structure
- Dislike
  - Exceeded its novelty budget 
  - Designed to be spread across multiple processes, and communicate over RPC
  - Took on lots of distributed system problems

#### [CodeMirror 6][codemirror6]

- Text editor written in JavaScript
- Like
  - Conflict resolution logic using [OT][ot]
  - Functional core, imperative shell

#### [XMonad](https://xmonad.org/)

- Window manager written in Haskell
- Like
  - Provided as a Haskell library, instead of a binary + external configuration
  - Configuration is done in Haskell; you're building the WM instead of
    configuring it
  - Small [`xmonad`](https://hackage.haskell.org/package/xmonad) core library
  - Monolithic [`xmonad-contrib`](https://hackage.haskell.org/package/xmonad-contrib) "userspace" library

#### [Bevy](https://bevyengine.org/)

- Game engine written in Rust
- Like
  - "Turtles all the way down": the game engine is written in the same language
    the games are, so there's no translation or bad abstractions from crossing
    a language boundary
  - [`bevy_ecs`](https://lib.rs/crates/bevy_ecs) crate makes great use of
    existential types and dynamic dispatch, to provide ergonomic extensibility,
    and reduce compile times by avoiding monomorphization

### Interesting Topics

(a.k.a. how to spend my novelty budget)

- Event sourcing
  - [Wikipedia article](https://en.wikipedia.org/wiki/Domain-driven_design#Event_sourcing)
  - Representing the editor and its subsystems' operations as data
  - Save operations to disk to be replayed later
  - Fuzz testing by generating arbitrary sequences of editor operations
  - Easy regression testing by replaying events to reach a desired state
  - Plugins could hook on specific operations
  - Plugins could act like middleware transforming streams of operations
  - If operations can be inverted, they could be undone 

- Operational transformation (OT)
  - [Wikipedia article][ot]
  - [CodeMirror 6 System Guide](https://codemirror.net/6/docs/guide/) (architecture and design)
  - [CodeMirror 6 Reference Manual](https://codemirror.net/6/docs/ref) (implementation details)
  - [`helix-core::transaction`](https://github.com/helix-editor/helix/blob/master/helix-core/src/transaction.rs)
  - [`operational-transform` crate](https://github.com/spebern/operational-transform-rs)
  - When not used in a distributed or peer-to-peer situation, the implementation
    is surprisingly manageable
  - Probably a higher power to weight ratio for Indigo than a CRDT

- Conflict-free replicated data types (CRDTs)
  - [Wikipedia article][crdt]
  - [Xi Editor Docs: CRDT - An approach to async plugins and undo](https://xi-editor.io/docs/crdt.html)
  - Probably an unnecessarily complicated solution to conflict resolution for
    Indigo, considering there will only be a single process, and it would
    coordinate any actors (not a distributed or peer-to-peer thing)

- Category theoretical perspective to conflict resolution in text editing
  - [On editing text](https://bosker.wordpress.com/2012/05/10/on-editing-text/)
  - [Revisiting “On editing text”](https://bosker.wordpress.com/2014/06/19/revisiting-on-editing-text/)


[codemirror6]: https://codemirror.net/6/
[crdt]: https://en.wikipedia.org/wiki/Conflict-free_replicated_data_type
[kakoune]: https://github.com/mawww/kakoune
[ot]: https://en.wikipedia.org/wiki/Operational_transformation
