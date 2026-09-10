# indigo-kernel

A transactional substrate for concurrent editing. Agents (human, plugin, code
formatter, etc) edit snapshots independently, and the kernel reconciles their
changes, converging on a single canonical state.

---

Indigo is designed to be single-threaded, synchronous, and avoid interior
mutability. Working with these constraints yields several benefits:

- **Low latency:** A text editor is an interactive application that has
  relatively low CPU and I/O demands; humans can only input text so fast, or
  keep track of so many files. Optimization effort should be spent on lowering
  latency and delivering a responsive experience. If something _can_ be
  instataneous, it _should_ be.
- **Simplicity:** Sync Rust is polished and fearless, async Rust has rough edges
  and footguns. Using a single thread means less synchronization required.
- **Compile-time guarantees:** It feels nicer when you're working with the
  compiler, not against it. Using references (e.g. `&T` and `&mut T`) instead of
  reference counting and locking and interior mutability (e.g. `Arc<Mutex<T>>`,
  `std::sync::mpsc`, etc) means the compiler prevents bugs, generates more
  efficient code, and helps you understand your program's semantics better.

These constraints are great and I want to keep them! But they cause problems
when you want to express more complex dependencies between pieces of state.

## Problem

Consider types representing the state of a simple text editor:

```rust
struct Editor {
    // Multiple buffers in memory, only one shown at a time
    buffers: Vec<Buffer>,
    foreground_buffer: usize,
}

struct Buffer {
    path: PathBuf,
    text: String,
    cursor: usize,
    vertical_scroll: usize,
}
```

When you insert text into a buffer, you move the cursor forward at the same
time. When you move the cursor around, you have exclusive access to the buffer
to ensure it stays in bounds.

The cursor's validity is a function of the text it's placed in. Because the two
are attached, `Buffer` trivially keeps them synchronized.

This is working well! Unfortunately, the moment you want to step outside this
nice tree-shaped ownership structure, and you need to separate pieces of state
that care about each other, you run into trouble.

Imagine we want to display multiple buffers on-screen at once, including the
potential for multiple windows viewing the same buffer but in different places.
That might look something like this:

```rust
struct Editor {
    buffers: Vec<Buffer>,
    windows: Vec<Window>,
    // Pretend this is more sophisticated, representing multiple
    // windows/splits/panes visible simultaneously in the UI
    visible_windows: Vec<usize>,
}

struct Buffer {
    path: PathBuf,
    text: String,
}

struct Window {
    buffer_index: usize,
    cursor: usize,
    vertical_scroll: usize,
}
```

Already we have so many issues!

- When a buffer is truncated, windows' vertical scroll positions may become
  invalid (i.e. beyond the last line).
- When a buffer is edited, windows' cursors don't adjust to the text shifting
  underneath them.
- Windows' buffer indexes may refer to deleted buffers (to be fair, this was a
  problem `Editor` had earlier with `foreground_buffer`).

State that is valid in the context of other state is separated, which means
maintaining invariants is no longer possible at lower levels of the ownership
tree.

There are many solutions to these problems, but these obvious ones are
unsatisfying to me:

- **Push responsibilities to parents:** Technically the `Editor` sits high
  enough in this ownership tree (in this case it's the root, but that's not a
  requirement) to maintain all the invariants and keep the pieces of state valid
  relative to each other. But that's so messy; good luck trying to abstract away
  any subcomponents!
- **Eschew the tree-shaped ownership structure:** You could make the editor
  state self-referential, but in Rust that's hacky at best, unsafe at worst. You
  could share ownership with reference-counting pointers. Either way, you're
  giving up static aliasing xor mutability checks in favor of runtime checks and
  locks and such. This totally works, but `Arc<Mutex<T>>` is a blunt hammer.

## Solution

Instead of struggling to keep all our state in sync, coupling and complicating
our various subsystems and/or going against the grain of Rust's ownership rules,
what if we could just... stop caring about synchronization? What if "is the
state synchronized" wasn't even a valid question anymore? Maybe that's a
farfetched way of describing it, but stick with me and I'll explain.

Much of our trouble comes from state becoming invalidated, and eagerly working
to keep it up-to-date. If we could come up with a representation that allows for
stale data, maybe we could lazily update state only when we demand its
freshness?

You could call this **"[eventual consistency]" or "[optimistic replication]" or
"delayed consensus"** but the idea is the same: design the system such that it
converges to the same state, eventually, and allow temporary inconsistencies.

[eventual consistency]: https://en.wikipedia.org/wiki/Eventual_consistency
[optimistic replication]: https://en.wikipedia.org/wiki/Optimistic_replication

Typically I hear about eventual consistency in the context of distributed
systems: database clusters distributed across multiple nodes, perhaps in
different data centers, talking over a network. But I've found this concept can
be applied at the opposite end of the spectrum too! It can help us solve these
problems of state distributed in our single-process, single-threaded
application.

Furthermore, once you embrace eventual consistency, the constraint becomes
liberating! We could fork a copy of our editor state, send it to a background
thread doing code formatting (I/O-bound task, takes a while), and let the user
continue editing their copy while we wait for the formatting changes to come
back. When that background thread finishes, we can join its state with the main
thread, integrating its remote edits with our local copy, and proceed forward.

And if we have the tech to make cheap copies of our state, we could use that for
undo/redo or full time travel debugging or whatever!

## Technology

The main technology I'm interested in for merging/convergence/reconciliation of
concurrent changes is **Conflict-free Replicated Data Types (CRDTs)**. But you
could implement this with other technologies such as Operational Transformation
(OT) or plain 2- or 3-way merge. Even if I have a fancy sequence CRDT
representing text, I might still use a basic merge function for background code
formatting, because that's an operation I can retry on conflicts (and CRDT
merging might semantically garble text).

I also really like the way the **"[Concurrent Programming with Revisions and
Isolation Types](https://dl.acm.org/doi/epdf/10.1145/1932682.1869515)"** paper
represents these concepts. I'll quote a little to give you an idea:

> Consider an application where many tasks are executing in parallel. For
> example, an office application may concurrently run tasks that (1) save a
> snapshot of the document to disk, (2) react to keyboard input by the user who
> is editing the document, (3) perform a spellcheck in the background, and (4)
> exchange document updates with collaborating remote users. Some of these tasks
> are CPU-bound, others are IO-bound; some only read the shared data, others may
> modify it. However, all of them need to potentially access the same data at
> the same time; thus, they must avoid, negotiate, or resolve conflicts.
>
> Ensuring consistency of shared data while allowing tasks to execute
> concurrently is often challenging, as it may require not only complex locking
> protocols, but also some form of data replication.

> We introduce a mechanism that simplifies the parallel execution of different
> application tasks. Programmers declare what data they wish to share between
> tasks by using isolation types, and execute tasks concurrently by forking and
> joining revisions. These revisions are isolated: they read and modify their
> own private copy of the shared data only. A runtime creates and merges copies
> automatically, and resolves conflicts deterministically, in a manner declared
> by the chosen isolation type.
