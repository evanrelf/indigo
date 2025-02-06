use crate::RopeExt as _;
use ropey::Rope;

// IS A CURSOR JUST A RANGE THAT ALWAYS REDUCES AFTER????

// TODO:
// - Use trait to avoid duplicated code between immutable and mutable types.

// CAUTION:
// - Unsafe to construct a `Range{,Mut}` from two user-provided `Cursor`s. They could point to
//   separate ropes that happen to share the same lifetime.
// - Unsafe to construct a `{Cursor,Range}Mut` from an existing rope holding structure without
//   consuming self. The new cursor/range could delete the whole rope and your indices would be
//   invalidated.

#[derive(Debug)]
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
    pub fn rope(&self) -> &'a Rope {
        self.rope
    }

    #[must_use]
    pub fn index(&self) -> usize {
        self.index
    }

    // TODO: Add `set_index`. Write `new` in terms of starting with a 0 index (always valid) and
    // then trying to set the provided index?
}

#[derive(Debug)]
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
