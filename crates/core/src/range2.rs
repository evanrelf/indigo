use crate::cursor2::{Cursor, CursorMut};
use ropey::Rope;

// TODO:
// - Use trait to avoid duplicated code between immutable and mutable types.

// CAUTION:
// - Unsafe to construct a `Range{,Mut}` from two user-provided `Cursor`s. They could point to
//   separate ropes that happen to share the same lifetime.
// - Unsafe to construct a `{Cursor,Range}Mut` from an existing rope holding structure without
//   consuming self. The new cursor/range could delete the whole rope and your indices would be
//   invalidated.

pub struct Range<'a> {
    rope: &'a Rope,
    anchor: usize, // char gap index
    head: usize,   // char gap index
}

impl Range<'_> {
    #[must_use]
    pub fn rope(&self) -> &Rope {
        self.rope
    }

    #[must_use]
    pub fn anchor(&self) -> Cursor<'_> {
        Cursor::new(self.rope, self.anchor).expect("Anchor index is valid in rope")
    }

    #[must_use]
    pub fn head(&self) -> Cursor<'_> {
        Cursor::new(self.rope, self.head).expect("Head index is valid in rope")
    }

    // TODO: Add `set_{head, anchor}`
}

impl<'a> From<Cursor<'a>> for Range<'a> {
    fn from(cursor: Cursor<'a>) -> Self {
        let index = cursor.index();
        let rope = cursor.rope();
        Self {
            rope,
            anchor: index,
            head: index,
        }
    }
}

impl<'a> From<CursorMut<'a>> for Range<'a> {
    fn from(cursor: CursorMut<'a>) -> Self {
        let index = cursor.index();
        let rope = cursor.into_rope_mut();
        Self {
            rope,
            anchor: index,
            head: index,
        }
    }
}

pub struct RangeMut<'a> {
    rope: &'a mut Rope,
    anchor: usize, // char gap index
    head: usize,   // char gap index
}

impl<'a> RangeMut<'a> {
    #[must_use]
    pub fn rope(&self) -> &Rope {
        self.rope
    }

    /// Must trade in `RangeMut` for `&mut Rope`. Upholding range invariants depends on coordinating
    /// rope and state mutations.
    #[must_use]
    pub fn into_rope_mut(self) -> &'a mut Rope {
        self.rope
    }

    #[must_use]
    pub fn anchor(&self) -> Cursor<'_> {
        Cursor::new(self.rope, self.anchor).expect("Anchor index is valid in rope")
    }

    /// Must trade in `RangeMut` for `CursorMut`. Upholding range invariants depends on coordinating
    /// rope and state mutations.
    #[must_use]
    pub fn into_anchor_mut(self) -> CursorMut<'a> {
        CursorMut::new(self.rope, self.anchor).expect("Anchor index is valid in rope")
    }

    #[must_use]
    pub fn head(&self) -> Cursor<'_> {
        Cursor::new(self.rope, self.head).expect("Head index is valid in rope")
    }

    /// Must trade in `RangeMut` for `CursorMut`. Upholding range invariants depends on coordinating
    /// rope and state mutations.
    #[must_use]
    pub fn into_head_mut(self) -> CursorMut<'a> {
        CursorMut::new(self.rope, self.head).expect("Head index is valid in rope")
    }

    // TODO: Add `set_{head, anchor}`
}

impl<'a> From<CursorMut<'a>> for RangeMut<'a> {
    fn from(cursor: CursorMut<'a>) -> Self {
        let index = cursor.index();
        let rope = cursor.into_rope_mut();
        Self {
            rope,
            anchor: index,
            head: index,
        }
    }
}
