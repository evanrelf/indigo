# Selection ownership: window vs buffer

Research into the tradeoffs of where `SelectionState` should live.

## Current state

The codebase recently moved `SelectionState` from `Buffer` to `WindowState`,
mirroring Kakoune's architecture. The `lenient-selection.md` design doc details
the immediate consequences and two mitigation strategies (snap-on-access + OT
transform).

## Approach A: Selection owned by the Window (current design)

### Pros

1. **Multiple independent selections per buffer.** Each window into the same
   buffer gets its own selection — essential for multiplayer/split-view. This is
   how Kakoune works.

2. **Selection is a view concern, not a data concern.** Conceptually, "where I'm
   looking and what I've selected" is UI state, not document state. Keeping it in
   the window respects this boundary.

3. **Undo/redo semantics are cleaner per-window.** Each window can have its own
   undo/redo interaction with selections. Window A undoing shouldn't move Window
   B's cursor. The current code already handles this — `WindowView::undo()`
   transforms only its own selection via OT.

4. **No contention on buffer mutations.** When modifying buffer text, you don't
   need to also lock/update a selection that lives alongside it. The buffer stays
   a pure data container.

5. **Extensible to remote/async scenarios.** If you ever add LSP-driven edits,
   code formatters, or AI-assisted editing that modify the buffer externally,
   window-owned selections can catch up lazily via the OT log. The buffer doesn't
   need to know about selections at all.

6. **Natural fit for the view-type pattern.** The `CursorView`/`RangeView`/
   `SelectionView` types already pair text (from buffer) with state (from window)
   at construction time. This pattern works naturally when the two halves live in
   separate owners.

### Cons

1. **Drift/staleness.** The core problem documented in `lenient-selection.md`.
   Byte offsets in `SelectionState` can become invalid (past end of text,
   mid-grapheme) when another window edits the buffer. Currently causes panics in
   view constructors and `scroll_to_selection()`.

2. **Two-phase fix required.** You need *both* Strategy A (snap-on-access as a
   safety net) *and* Strategy B (OT transform for correctness). Snap-on-access
   alone loses semantic meaning — a selection on line 50 might silently jump
   elsewhere.

3. **Version tracking overhead.** Each window needs to store a version number and
   catch up through the OT log before accessing its selection. This adds a
   `version: usize` field to `WindowState` and a transform step on every
   selection access (or at least on every focus switch).

4. **Harder to reason about invariants.** The `assert_invariants()` pattern that
   catches logic bugs becomes weaker — you can't tell whether a bad offset is
   from a real bug or just staleness from another window's edit. The design doc
   suggests debug-mode warnings as a compromise.

5. **`scroll_to_selection()` and similar raw-access sites.** Any code that reads
   `SelectionState` directly (bypassing view constructors) is a latent panic
   site. Every new piece of code touching raw selection state must remember to
   snap or clamp.

6. **Complexity in editor-level operations.** Operations like "close window" need
   to decide what happens to the selection. Operations like "move buffer to new
   window" need to decide whether to carry the selection or create a fresh one.

## Approach B: Selection owned by the Buffer

### Pros

1. **Text and selection are always in sync.** Since they're co-located, every
   text mutation can immediately update the selection in the same method call. No
   drift, no staleness, no version tracking.

2. **Single source of truth.** No need to reconcile multiple potentially-stale
   selection states.

3. **Simpler invariant enforcement.** `assert_invariants()` can be strict — if a
   selection is invalid, it's a real bug, not a staleness artifact.

4. **No OT catch-up needed for selections.** The OT log is only needed for text
   undo/redo, not for keeping selections valid across windows.

5. **Fewer panic sites.** Raw access to selection state is safe because the
   selection is always valid relative to its text.

### Cons

1. **Single selection per buffer.** All windows into the same buffer share one
   selection. This breaks the Kakoune model and makes split-view much less
   useful — moving the cursor in one view moves it in all views.

2. **Multiplayer is impossible without workarounds.** Multiple users editing the
   same buffer would need separate selection tracking anyway.

3. **Buffer becomes a God object.** It owns text + history + selection +
   potentially mode state. The boundary between "document data" and "view state"
   gets blurred.

4. **Undo/redo affects all windows.** Window A undoing a change also undoes
   Window B's perception of where the cursor was. No per-window undo behavior.

5. **External mutations still need selection awareness.** LSP auto-format or AI
   agent edits still need to update the buffer's selection. The sync problem
   moves inside the buffer rather than being eliminated.

6. **Switching buffers in a window is awkward.** The selection "follows" the
   buffer, not the window. If you switch away and come back, you resume the
   buffer's last selection — which might have been set by a different window.

## Hybrid approaches

### C: Buffer stores per-window selections

The buffer stores a `HashMap<WindowKey, SelectionState>`. Text mutations
immediately transform all selections. Windows get independent cursors and the
buffer maintains sync.

Downside: buffer now knows about windows (coupling), and dead windows leave
orphan selections that need cleanup.

### D: Window-owned with eager push-based updates

Instead of lazy OT catch-up, the editor's buffer-mutation methods immediately
transform all other windows' selections. The `Editor` already knows all windows
and their buffer associations.

This avoids per-window version tracking and makes sync deterministic.
Downside: O(windows * ranges * ops) on every edit.

## Assessment

The current design (window-owned, Approach A) is the right long-term choice for
a Kakoune-inspired editor. The cons are real but solvable:

- **Strategy A** (snap-on-access) is a small change that eliminates all panics.
  Sufficient for current single-window usage.
- **Strategy B** (OT transform) is needed later for multi-window correctness.
  The infrastructure already exists (`Text::ops_since`,
  `SelectionState::transform`).
- **Approach D** (eager push at editor level) might be the cleanest way to
  implement Strategy B — centralizes sync logic, keeps windows simple.

Moving selection back to the buffer would solve the immediate staleness problem
but paint the architecture into a corner. The sync problem doesn't go away — it
just changes shape.
