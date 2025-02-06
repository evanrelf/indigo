use crate::rope::RopeExt;
use ropey::Rope;

// TODO:
// - Use trait to avoid duplicated code between immutable and mutable types.

#[derive(Debug, Default)]
pub struct RawRange {
    pub anchor: usize, // char gap index
    pub head: usize,   // char gap index
}

impl RawRange {
    pub fn new(rope: &Rope, mut anchor: usize, mut head: usize) -> Result<Self, Self> {
        let mut corrected = false;

        // Gap at end of file counts as grapheme boundary
        if let Ok(true) = rope.try_is_grapheme_boundary(anchor) {
            //
        } else {
            corrected = true;
            anchor = rope.len_chars();
        }

        // Gap at end of file counts as grapheme boundary
        if let Ok(true) = rope.try_is_grapheme_boundary(head) {
            //
        } else {
            corrected = true;
            head = rope.len_chars();
        }

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
    pub fn anchor(&self) -> usize {
        self.range.anchor
    }

    #[must_use]
    pub fn head(&self) -> usize {
        self.range.head
    }

    // TODO: Add `set_{head, anchor}`
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
    pub fn anchor(&self) -> usize {
        self.range.anchor
    }

    #[must_use]
    pub fn head(&self) -> usize {
        self.range.head
    }

    // TODO: Add `set_{head, anchor}`
}
