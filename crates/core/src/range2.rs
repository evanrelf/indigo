use crate::RopeExt as _;
use ropey::Rope;

// TODO:
// - Use trait to avoid duplicated code between immutable and mutable types.

// CAUTION:
// - Unsafe to construct a `Range{,Mut}` from two user-provided `Cursor`s. They could point to
//   separate ropes that happen to share the same lifetime.
// - Unsafe to construct a `{Cursor,Range}Mut` from an existing rope holding structure without
//   consuming self. The new cursor/range could delete the whole rope and your indices would be
//   invalidated.

pub struct Cursor<'a> {
    rope: &'a Rope,
    index: usize, // char gap index
}

impl<'a> Cursor<'a> {
    pub fn new(rope: &'a Rope, mut index: usize) -> Result<Self, Self> {
        // Gap at end of file counts as grapheme boundary
        if let Ok(true) = rope.try_is_grapheme_boundary(index) {
            Ok(Self { rope, index })
        } else {
            index = rope.len_chars();
            Err(Self { rope, index })
        }
    }

    #[must_use]
    pub fn rope(&self) -> &Rope {
        self.rope
    }

    #[must_use]
    pub fn index(&self) -> usize {
        self.index
    }

    // TODO: Add `set_index`. Write `new` in terms of starting with a 0 index (always valid) and
    // then trying to set the provided index?
}

pub struct CursorMut<'a> {
    rope: &'a mut Rope,
    index: usize, // char gap index
}

impl<'a> CursorMut<'a> {
    pub fn new(rope: &'a mut Rope, mut index: usize) -> Result<Self, Self> {
        // Gap at end of file counts as grapheme boundary
        if let Ok(true) = rope.try_is_grapheme_boundary(index) {
            Ok(Self { rope, index })
        } else {
            index = rope.len_chars();
            Err(Self { rope, index })
        }
    }

    #[must_use]
    pub fn rope(&self) -> &Rope {
        self.rope
    }

    /// Must trade in `CursorMut` for `&mut Rope`. Upholding cursor invariants depends on
    /// coordinating rope and state mutations.
    #[must_use]
    pub fn into_rope_mut(self) -> &'a mut Rope {
        self.rope
    }

    #[must_use]
    pub fn index(&self) -> usize {
        self.index
    }

    // TODO: Add `set_index`. Write `new` in terms of starting with a 0 index (always valid) and
    // then trying to set the provided index?
}

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
        Cursor {
            rope: self.rope,
            index: self.anchor,
        }
    }

    #[must_use]
    pub fn head(&self) -> Cursor<'_> {
        Cursor {
            rope: self.rope,
            index: self.head,
        }
    }

    // TODO: Add `set_{head, anchor}`
}

impl<'a> From<Cursor<'a>> for Range<'a> {
    fn from(cursor: Cursor<'a>) -> Self {
        Self {
            rope: cursor.rope,
            anchor: cursor.index,
            head: cursor.index,
        }
    }
}

impl<'a> From<CursorMut<'a>> for Range<'a> {
    fn from(cursor: CursorMut<'a>) -> Self {
        Self {
            rope: cursor.rope,
            anchor: cursor.index,
            head: cursor.index,
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
        Cursor {
            rope: self.rope,
            index: self.anchor,
        }
    }

    /// Must trade in `RangeMut` for `CursorMut`. Upholding range invariants depends on coordinating
    /// rope and state mutations.
    #[must_use]
    pub fn into_anchor_mut(self) -> CursorMut<'a> {
        CursorMut {
            rope: self.rope,
            index: self.anchor,
        }
    }

    #[must_use]
    pub fn head(&self) -> Cursor<'_> {
        Cursor {
            rope: self.rope,
            index: self.head,
        }
    }

    /// Must trade in `RangeMut` for `CursorMut`. Upholding range invariants depends on coordinating
    /// rope and state mutations.
    #[must_use]
    pub fn into_head_mut(self) -> CursorMut<'a> {
        CursorMut {
            rope: self.rope,
            index: self.head,
        }
    }

    // TODO: Add `set_{head, anchor}`
}

impl<'a> From<CursorMut<'a>> for RangeMut<'a> {
    fn from(cursor: CursorMut<'a>) -> Self {
        Self {
            rope: cursor.rope,
            anchor: cursor.index,
            head: cursor.index,
        }
    }
}
