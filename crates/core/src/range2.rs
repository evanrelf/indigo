use crate::cursor2::{Cursor, CursorMut};
use ropey::Rope;

// TODO:
// - Use trait to avoid duplicated code between immutable and mutable types.
// - Put common stuff in `RawRange` impl instead of "parts"?

// CAUTION:
// - Unsafe to construct a `Range{,Mut}` from two user-provided `Cursor`s. They could point to
//   separate ropes that happen to share the same lifetime.
// - Unsafe to construct a `{Cursor,Range}Mut` from an existing rope holding structure without
//   consuming self. The new cursor/range could delete the whole rope and your indices would be
//   invalidated.

#[derive(Debug, Default)]
pub struct RawRange {
    pub anchor: usize, // char gap index
    pub head: usize,   // char gap index
}

impl RawRange {
    pub fn new(rope: &Rope, mut anchor: usize, mut head: usize) -> Result<Self, Self> {
        let mut corrected = false;

        anchor = match Cursor::new(rope, anchor) {
            Err(cursor) => {
                corrected = true;
                cursor.index()
            }
            Ok(cursor) => cursor.index(),
        };

        head = match Cursor::new(rope, head) {
            Err(cursor) => {
                corrected = true;
                cursor.index()
            }
            Ok(cursor) => cursor.index(),
        };

        if corrected {
            Err(Self { anchor, head })
        } else {
            Ok(Self { anchor, head })
        }
    }

    pub fn move_left(&mut self, rope: &Rope, distance: usize) {
        todo!()
    }

    pub fn move_right(&mut self, rope: &Rope, distance: usize) {
        todo!()
    }

    pub fn extend_left(&mut self, rope: &Rope, distance: usize) {
        todo!()
    }

    pub fn extend_right(&mut self, rope: &Rope, distance: usize) {
        todo!()
    }

    pub fn reduce(&mut self) {
        self.anchor = self.head;
    }

    pub fn insert_char(&mut self, rope: &mut Rope, char: char) {
        self.insert(rope, &char.to_string());
    }

    pub fn insert(&mut self, rope: &mut Rope, string: &str) {
        todo!()
    }

    pub fn backspace(&mut self, rope: &mut Rope, count: usize) {
        todo!()
    }

    pub fn delete(&mut self, rope: &mut Rope, count: usize) {
        todo!()
    }
}

#[derive(Debug)]
pub struct Range<'a> {
    rope: &'a Rope,
    range: RawRange,
}

impl<'a> Range<'a> {
    pub fn new(rope: &'a Rope, anchor: usize, head: usize) -> Result<Self, Self> {
        match RawRange::new(rope, anchor, head) {
            Err(range) => Err(Self { rope, range }),
            Ok(range) => Ok(Self { rope, range }),
        }
    }

    #[must_use]
    pub fn rope(&self) -> &Rope {
        self.rope
    }

    #[must_use]
    pub fn anchor(&self) -> Cursor<'_> {
        Cursor::new(self.rope, self.range.anchor).expect("Anchor index is valid in rope")
    }

    #[must_use]
    pub fn head(&self) -> Cursor<'_> {
        Cursor::new(self.rope, self.range.head).expect("Head index is valid in rope")
    }

    // TODO: Add `set_{head, anchor}`
}

impl<'a> From<Cursor<'a>> for Range<'a> {
    fn from(cursor: Cursor<'a>) -> Self {
        let index = cursor.index();
        let rope = cursor.rope();
        Self::new(rope, index, index).expect("Cursor index is valid in rope")
    }
}

impl<'a> From<CursorMut<'a>> for Range<'a> {
    fn from(cursor: CursorMut<'a>) -> Self {
        let index = cursor.index();
        let rope = cursor.into_rope_mut();
        Self::new(rope, index, index).expect("Cursor index is valid in rope")
    }
}

#[derive(Debug)]
pub struct RangeMut<'a> {
    rope: &'a mut Rope,
    range: RawRange,
}

impl<'a> RangeMut<'a> {
    pub fn new(rope: &'a mut Rope, anchor: usize, head: usize) -> Result<Self, Self> {
        match RawRange::new(rope, anchor, head) {
            Err(range) => Err(Self { rope, range }),
            Ok(range) => Ok(Self { rope, range }),
        }
    }

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
        Cursor::new(self.rope, self.range.anchor).expect("Anchor index is valid in rope")
    }

    /// Must trade in `RangeMut` for `CursorMut`. Upholding range invariants depends on coordinating
    /// rope and state mutations.
    #[must_use]
    pub fn into_anchor_mut(self) -> CursorMut<'a> {
        CursorMut::new(self.rope, self.range.anchor).expect("Anchor index is valid in rope")
    }

    #[must_use]
    pub fn head(&self) -> Cursor<'_> {
        Cursor::new(self.rope, self.range.head).expect("Head index is valid in rope")
    }

    /// Must trade in `RangeMut` for `CursorMut`. Upholding range invariants depends on coordinating
    /// rope and state mutations.
    #[must_use]
    pub fn into_head_mut(self) -> CursorMut<'a> {
        CursorMut::new(self.rope, self.range.head).expect("Head index is valid in rope")
    }

    // TODO: Add `set_{head, anchor}`
}

impl<'a> From<CursorMut<'a>> for RangeMut<'a> {
    fn from(cursor: CursorMut<'a>) -> Self {
        let index = cursor.index();
        let rope = cursor.into_rope_mut();
        Self::new(rope, index, index).expect("Cursor index is valid in rope")
    }
}
