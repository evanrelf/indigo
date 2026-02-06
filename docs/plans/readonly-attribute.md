# Plan: Range-Specific Readonly Attribute

## Summary

Extend the existing `Attributes` system to support protecting specific ranges of text from modification, rather than the current binary readonly flag on the entire buffer.

## Background

The codebase already has:

1. **`Attributes<A>`** (`crates/indigo-core/src/attributes.rs`): A generic system mapping attributes to byte index ranges using `RoaringBitmap`. Already supports `transform()` to update ranges when operations are applied.

2. **`Text::apply()`** (`crates/indigo-core/src/text.rs`): The chokepoint where all text mutations flow through. Currently has a simple `readonly: bool` check.

3. **`OperationSeq`** (`crates/indigo-core/src/ot.rs`): Operations are sequences of `Retain(n)`, `Insert(String)`, and `Delete(n)`.

### Terminology

- **Index**: Unsigned integer identifying a specific byte in text (e.g., byte index 5 is the 6th byte).
- **Offset**: Unsigned integer identifying a gap between bytes (e.g., offset 5 is the gap before byte index 5).

The `Attributes` system uses byte indexes (which bytes have the attribute). OT operations work with byte offsets (where to insert/delete).

## Design Decisions

### Semantics

1. **Insertions at boundaries**: Inserting at an offset immediately before or after a readonly range is allowed. Inserting at an offset *within* a readonly range (between two readonly byte indexes) is blocked.

2. **Deletions**: Any deletion that overlaps readonly byte indexes is blocked, even partially.

3. **Partial rejection**: If an `OperationSeq` contains any operation that would touch a readonly range, reject the entire sequence. This maintains atomicity for multi-cursor edits.

4. **Range stability**: Readonly ranges cannot expand or contract (since internal edits are blocked). They shift when surrounding text is edited, handled by `Attributes::transform()`.

### Storage

Add an `attributes: Attributes<&'static str>` field to `Text` for general-purpose attributes. The `"readonly"` attribute marks byte index ranges as protected. This allows future attributes to use the same infrastructure.

```rust
pub struct Text {
    rope: Rope,
    history: History<BidiOperationSeq, BidiOperationSeq>,
    log: Vec<OperationSeq>,
    readonly: bool,  // existing binary flag
    attributes: Attributes<&'static str>,  // general-purpose attributes
}
```

The `"readonly"` attribute is one of potentially many attributes stored here.

## Implementation Steps

### Step 1: Add operation inspection helper

**File**: `crates/indigo-core/src/ot.rs`

Add a method to check if an `OperationSeq` touches any byte indexes in a bitmap:

```rust
impl OperationSeq {
    /// Returns true if any delete overlaps the given byte indexes, or any
    /// insert falls strictly inside (not at boundary offsets of) the given ranges.
    pub fn touches_indexes(&self, indexes: &RoaringBitmap) -> bool {
        // Walk operations, tracking byte offset in source text
        // - Retain(n): advance offset by n
        // - Delete(n): check if byte indexes [offset, offset+n) intersect, advance by n
        // - Insert: check if offset is strictly inside a range (not at boundary)
    }
}
```

For the "strictly inside" check on inserts: an insert at byte offset `o` is inside a readonly range if both byte index `o-1` and byte index `o` are readonly (meaning the offset falls between two protected bytes). An insert at offset `o` where only one side is readonly is a boundary insert and is allowed.

### Step 2: Add attributes to Text

**File**: `crates/indigo-core/src/text.rs`

- Add `attributes: Attributes<&'static str>` field (default empty)
- Add methods:
  - `add_attribute(range: Range<usize>, attr: &'static str)`
  - `remove_attribute(range: Range<usize>, attr: &'static str)`
  - `has_attribute(range: Range<usize>, attr: &'static str) -> bool`
  - `attribute_ranges(attr: &'static str) -> Option<impl Iterator<Item = RangeInclusive<u32>>>`

### Step 3: Enforce readonly in Text::apply()

**File**: `crates/indigo-core/src/text.rs`

Before applying operations, check if they touch readonly byte indexes:

```rust
pub fn apply(&mut self, ops: &OperationSeq) -> anyhow::Result<()> {
    anyhow::ensure!(!self.readonly, "Cannot modify readonly text");

    if let Some(readonly_ranges) = self.attributes.ranges("readonly") {
        let indexes: RoaringBitmap = readonly_ranges
            .flat_map(|r| *r.start()..=*r.end())
            .collect();
        anyhow::ensure!(
            !ops.touches_indexes(&indexes),
            "Cannot modify readonly range"
        );
    }

    // ... existing implementation ...

    // After applying, transform all attributes
    self.attributes.transform(ops);

    Ok(())
}
```

### Step 4: Expose through Buffer

**File**: `crates/indigo-core/src/buffer.rs`

Add delegation methods matching those added to `Text`.

### Step 5: Add command to mark ranges readonly

**File**: `crates/indigo-core/src/mode/command.rs`

Add commands like:
- `:readonly-add` - Mark current selection's byte indexes as readonly
- `:readonly-remove` - Remove readonly from current selection
- `:readonly-clear` - Remove all readonly ranges

## Verification

1. Run `cargo clippy` after each change

2. Unit tests for `OperationSeq::touches_indexes()`:
   - Delete fully inside readonly byte indexes -> true
   - Delete partially overlapping readonly byte indexes -> true
   - Delete fully outside readonly byte indexes -> false
   - Insert at offset inside readonly range (both adjacent bytes readonly) -> true
   - Insert at offset at readonly range boundary (one side readonly) -> false
   - Insert at offset outside readonly range -> false

3. Unit tests for `Text` with readonly attribute:
   - Apply succeeds when not touching readonly
   - Apply fails when delete overlaps readonly byte indexes
   - Apply fails when insert offset is inside readonly range
   - Readonly byte index ranges shift correctly after allowed edits

4. CLI tests:
   - Mark range readonly, try to delete it -> blocked
   - Mark range readonly, edit surrounding text -> allowed, range shifts
   - Remove readonly from range, edit it -> allowed

## Future Considerations

- Visual indication of readonly ranges in the TUI (different background color?)
- Readonly ranges could be persisted as file metadata
- Could be used for "folded" regions that are collapsed and non-editable
- Could protect auto-generated code sections
- Other attributes like syntax highlighting tokens, diagnostic underlines, etc.
