# Lenient selection: handling text/selection desync across windows

## Background

Indigo recently moved `SelectionState` from `Buffer` to `WindowState`. Previously,
a buffer owned both its text and its selection, so they were always in sync — any
mutation to the text could immediately update the selection in the same method
call. Now, text lives in `Buffer` and selection lives in `WindowState`, which
means multiple windows can share the same buffer but each has its own selection.

This mirrors Kakoune's architecture: the server owns buffers, each client has a
window with its own selection. Buffer changes propagate, but selections are
per-client.

## The problem

When window A edits the buffer's text (e.g. inserting or deleting characters),
window B's selection — which contains byte offsets into that same text — becomes
stale. The byte offsets may now:

1. **Exceed the text length.** If window A deletes text near the end of the
   buffer, window B's cursors might point past the end.
2. **Land mid-grapheme.** If window A inserts a multi-byte character at a
   position before window B's cursor, the cursor's byte offset might now fall
   in the middle of a grapheme cluster.
3. **Select a meaningless region.** Even if byte offsets are technically valid,
   the semantic meaning of a selection (e.g. "the word `foo`") is lost after
   the underlying text changes.

Problem (3) is inherent and acceptable — Kakoune has the same behavior. Problems
(1) and (2) currently cause **panics** and must be fixed.

## Where the panics happen

### Panic path 1: View constructors

The primary panic chain goes through the view type constructors:

1. `WindowView::selection()` and `WindowView::selection_mut()` call
   `SelectionView::new()`, which calls `self.assert_invariants()`.
   (`crates/indigo-core/src/selection.rs:75`)

2. `SelectionView::get()` and `SelectionView::for_each()` call `Range::new()`,
   which calls `self.assert_invariants()`.
   (`crates/indigo-core/src/selection.rs:89-90, 101-102`)

3. `Range::assert_invariants()` delegates to `Cursor::assert_invariants()` for
   both tail and head.
   (`crates/indigo-core/src/range.rs:261-263`)

4. `Cursor::assert_invariants()` checks two things and bails on failure:
   - `byte_offset > text.len()` → `Error::ExceedsEnd`
   - `!text.is_grapheme_boundary(byte_offset)` → `Error::NotOnGraphemeBoundary`
   (`crates/indigo-core/src/cursor.rs:176-189`)

5. The callers of these constructors use `.expect(...)` or `.unwrap()`, turning
   the `Err` into a panic. For example:
   - `WindowView::selection()`: `.expect("Window text and selection state are always kept valid")`
   - `SelectionView::get()`: `.expect("Selection text and range state are always kept valid")`
   - `SelectionMut`'s `on_drop` callback: `.unwrap()`

The `SelectionView::assert_invariants()` method itself is currently a no-op
(`Ok(())`), so the selection-level check doesn't catch anything. But the
**inner** `Range::new()` and `Cursor::new()` calls do real validation and will
panic on stale byte offsets.

Additionally, `RangeMut::tail_mut()` and `RangeMut::head_mut()` (range.rs:282-291)
attach their own `on_drop` callbacks that call `cursor.assert_invariants().unwrap()`.
These fire when a `CursorMut` is dropped after any cursor movement or edit
operation. If the view layer were made lenient at construction but these `on_drop`
checks remained strict, they could still panic. However, these inner `on_drop`
checks validate state *after* an operation within a single window's mutation —
they should always pass because the operation itself maintains consistency. The
cross-window desync only matters at the boundary where stale state meets a new
text version, which is the view construction step. Once a view is constructed with
snapped state, all operations within that view's lifetime are self-consistent.

### Panic path 2: Raw state access in `scroll_to_selection`

`WindowView::scroll_to_selection()` (window.rs:102-116) reads from
`self.state.selection` directly without constructing a `SelectionView`:

```rust
pub fn scroll_to_selection(&mut self) {
    let state = &self.state.selection;
    let head_byte_offset = state.ranges[state.primary_range].head.byte_offset;
    let line = self.buffer.text.rope().byte_to_line_idx(head_byte_offset, LINE_TYPE);
    // ...
}
```

`Rope::byte_to_line_idx` panics if `byte_idx > self.len()` (it does
`assert!(byte_idx <= self.len())`). So a stale `head.byte_offset` that exceeds
the text length will panic here too, bypassing the view constructor entirely.
This code path must also be made safe — either by snapping the selection before
use, or by clamping the byte offset inline.

### Panic path 3: `Editor::assert_invariants`

`Editor::assert_invariants()` (editor.rs:156-167) constructs `Selection::new()`
for each window, which would trigger panic path 1. However, this method is
currently `#[expect(dead_code)]` and never called, so it's not a live panic site.
It should still be updated for correctness, but it's not urgent.

## The existing pattern: eventual consistency for scroll position

`WindowState` already has a field that can go stale: `prev_vertical_scroll`. The
window handles this with a lazy-correction pattern:

```rust
// WindowState stores the raw value:
pub prev_vertical_scroll: usize,

// The accessor clamps it to valid bounds on read:
pub fn vertical_scroll(&self) -> usize {
    let last_line = self.buffer.text.rope().len_lines_indigo().saturating_sub(1);
    min(self.state.prev_vertical_scroll, last_line)
}
```

The stored value is never proactively updated when the buffer changes — it's
corrected on access. This is the model that selection should follow.

## What "lenient" means concretely

A `SelectionState` contains:
- `ranges: Vector<RangeState>` — each range has a `tail: CursorState` and
  `head: CursorState`, each of which is a `byte_offset: usize`.
- `primary_range: usize` — index into `ranges`.
- Each range also has `goal_column: usize`.

After another window mutates the buffer, any of these byte offsets might be
invalid. "Lenient" means: **instead of panicking, snap them to the nearest valid
position.**

### How snapping works

The existing snap helpers handle all the edge cases, including out-of-bounds:

- `floor_grapheme_boundary(rope, byte_index)` (rope.rs:475): If
  `byte_index > rope.len()`, returns `rope.len()`. Otherwise rounds down to the
  nearest grapheme boundary.
- `ceil_grapheme_boundary(rope, byte_index)` (rope.rs:489): If
  `byte_index > rope.len()`, returns `rope.len()`. Otherwise rounds up to the
  nearest grapheme boundary.

This means the snap functions implicitly handle the "exceeds text length" case.
When a byte offset is past the end, both floor and ceil clamp it to `text.len()`,
which is always a valid grapheme boundary (it's the end-of-text position).

For ranges, `RangeState::snap_to_grapheme_boundaries` (range.rs:36-57) snaps
the tail and head **outward** — if `tail < head` (forward range), tail floors
and head ceils; if `tail > head` (backward range), tail ceils and head floors.
When both offsets exceed `text.len()`, they both clamp to `text.len()`, producing
an empty range at the end of the buffer. This is acceptable — it's equivalent to
"cursor at end of file" which is the most reasonable fallback when the text the
selection pointed to no longer exists.

These helpers exist on the **state** types (not the view types), so they can be
called without constructing a view — which is important because the view
construction is exactly what panics.

## Where to make changes

There are two strategies, and they serve different purposes:

### Strategy A: Snap on access (safety net)

Make the view constructors themselves lenient — snap stale state into validity
instead of panicking. This prevents crashes regardless of how the desync
occurred.

The cleanest approach is to make `CursorView::new()`, `RangeView::new()`, and
`SelectionView::new()` infallible by snapping instead of validating:

```rust
// cursor.rs — before (panics on stale state):
pub fn new(...) -> anyhow::Result<Self> {
    cursor_view.assert_invariants()?;
    Ok(cursor_view)
}

// cursor.rs — after (snaps stale state):
pub fn new(...) -> Self {
    cursor_view.snap_to_grapheme_boundary();
    cursor_view
}
```

Making the constructors infallible at the lowest level (cursor/range) means every
code path that constructs views — `WindowView::selection()`,
`SelectionView::get()`, `SelectionView::for_each()`, etc. — becomes safe
automatically. This avoids having to audit and fix each call site individually.

This also resolves the `&self` vs `&mut self` tension for `WindowView::selection()`.
If the view constructors snap on construction, `selection()` doesn't need to
mutate `WindowState` — the snapping happens transiently inside the view. The
stored `SelectionState` remains stale until `selection_mut()` is called (which
can snap and persist the correction), but that's fine because every read path
goes through a view constructor that snaps.

The downside is losing the ability to catch logic bugs where state is unexpectedly
invalid. A middle ground: keep strict validation in debug builds
(`debug_assert!`) but snap silently in release builds. Or: log a warning when
snapping occurs, so desync events are visible in traces without crashing.

`scroll_to_selection()` also needs fixing since it reads raw state. It should
either go through the snapping view constructors, or clamp the byte offset
inline before passing it to `byte_to_line_idx`.

### Strategy B: OT transform on edit (correctness)

Whenever a window mutates the buffer, use operational transformation to update
all *other* windows' selections. This preserves the semantic meaning of
selections rather than just snapping to a nearby valid position.

This is what already happens *within* a single window:
- `SelectionView::for_each_mut()` (selection.rs:123-139) transforms sibling
  ranges after each mutation using `ops_since(version)`.
- `WindowView::undo()` and `WindowView::redo()` (window.rs:129-167) transform
  the window's own selection after text changes.

The OT log (`Text::log`) is append-only and lives on `Text` (which is shared via
`Buffer`). Any window can call `text.ops_since(version)` to retrieve operations
applied since a given version, regardless of which window produced them. This
means Strategy B is mechanically feasible: each window would store a `version`
marker, and before accessing its selection, it would catch up by transforming
through any operations it hasn't seen yet.

This could be implemented in `Editor`-level methods that wrap buffer mutations, or
lazily in `WindowView` methods.

### Recommendation

Do both, in layers:
1. **First**, implement Strategy A (snap on access) as a safety net. This
   prevents panics unconditionally.
2. **Later**, implement Strategy B (OT transform on edit) for better selection
   preservation. Without this, a stale selection just snaps to the nearest valid
   position, which might be semantically wrong (e.g. a selection on line 50 might
   jump to a completely different part of the buffer if text was inserted before
   it). With OT transforms, the selection would track the text movement correctly.

Strategy A is sufficient for single-window usage (the current state of the
editor). Strategy B becomes important when multiple windows on the same buffer
are actually implemented.

## Files to modify

- `crates/indigo-core/src/cursor.rs` — Make `CursorView::new()` infallible
  (snap instead of error). Update `assert_invariants` to be a debug-only check
  or remove it from the constructor path.
- `crates/indigo-core/src/range.rs` — Make `RangeView::new()` infallible,
  matching cursor changes.
- `crates/indigo-core/src/selection.rs` — Make `SelectionView::new()` infallible.
  Update `get()` / `for_each()` which construct inner views.
- `crates/indigo-core/src/window.rs` — Update `selection()` / `selection_mut()`
  signatures (no longer `Result`). Fix `scroll_to_selection()` to handle stale
  byte offsets. Update callers of `SelectionView::new()` to remove `.expect()`.

## Key types and their locations

| Type | File | Role |
|------|------|------|
| `CursorState` | `cursor.rs:30` | Raw byte offset |
| `CursorState::snap_to_grapheme_boundary` | `cursor.rs:42` | Snaps to `ceil_grapheme_boundary` (handles out-of-bounds) |
| `CursorView::new` | `cursor.rs:79` | Constructs cursor view, currently validates |
| `CursorView::assert_invariants` | `cursor.rs:176` | Checks offset ≤ len and on grapheme boundary |
| `RangeState` | `range.rs:27` | Two cursors (tail, head) + goal_column |
| `RangeState::snap_to_grapheme_boundaries` | `range.rs:39` | Snaps tail/head outward (handles out-of-bounds) |
| `RangeView::new` | `range.rs:127` | Constructs range view, currently validates |
| `RangeView::assert_invariants` | `range.rs:261` | Delegates to cursor invariants |
| `RangeMut::tail_mut` / `head_mut` | `range.rs:282,288` | Construct `CursorMut` with `on_drop` invariant check |
| `SelectionState` | `selection.rs:24` | Vector of ranges + primary index |
| `SelectionState::transform` | `selection.rs:30` | OT transform all ranges |
| `SelectionView::new` | `selection.rs:66` | Constructs selection view, currently validates |
| `SelectionView::assert_invariants` | `selection.rs:109` | Currently a no-op |
| `WindowView::selection` | `window.rs:82` | Constructs `Selection` from buffer text + window selection |
| `WindowView::selection_mut` | `window.rs:123` | Constructs `SelectionMut` from buffer text + window selection |
| `WindowView::scroll_to_selection` | `window.rs:102` | Reads raw selection state, calls `byte_to_line_idx` (panics on OOB) |
| `Text::version` | `text.rs:115` | Returns current log length (for OT catch-up) |
| `Text::ops_since` | `text.rs:120` | Returns ops since a version (append-only log, shared across windows) |
| `floor_grapheme_boundary` | `rope.rs:475` | Clamps to len, then floors to grapheme boundary |
| `ceil_grapheme_boundary` | `rope.rs:489` | Clamps to len, then ceils to grapheme boundary |
