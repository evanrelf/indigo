#![allow(unused_variables)] // TODO

use crate::rope::RopeExt;
use ropey::Rope;

#[derive(Debug, Default)]
pub struct RawRange {
    pub anchor: usize, // char gap index
    pub head: usize,   // char gap index
}

#[derive(Debug)]
pub struct Range<'a> {
    rope: &'a Rope,
    range: RawRange,
}

impl<'a> Range<'a> {
    pub fn new(rope: &'a Rope, anchor: usize, head: usize) -> Result<Self, Self> {
        match new_impl(rope, anchor, head) {
            Err(range) => Err(Self { rope, range }),
            Ok(range) => Ok(Self { rope, range }),
        }
    }

    // TODO: Add `set_{head, anchor}`?
}

#[derive(Debug)]
pub struct RangeMut<'a> {
    rope: &'a mut Rope,
    range: RawRange,
}

impl<'a> RangeMut<'a> {
    pub fn new(rope: &'a mut Rope, anchor: usize, head: usize) -> Result<Self, Self> {
        match new_impl(rope, anchor, head) {
            Err(range) => Err(Self { rope, range }),
            Ok(range) => Ok(Self { rope, range }),
        }
    }

    /// Must trade in `RangeMut` for `&mut Rope`. Upholding range invariants depends on coordinating
    /// rope and state mutations.
    #[must_use]
    pub fn into_rope_mut(self) -> &'a mut Rope {
        self.rope
    }

    pub fn insert_char(&mut self, char: char) {
        self.insert(&char.to_string());
    }

    pub fn insert(&mut self, string: &str) {
        todo!()
    }

    pub fn backspace(&mut self, count: usize) {
        todo!()
    }

    pub fn delete(&mut self, count: usize) {
        todo!()
    }

    // TODO: Add `set_{head, anchor}`?
}

fn new_impl(rope: &Rope, mut anchor: usize, mut head: usize) -> Result<RawRange, RawRange> {
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
        Err(RawRange { anchor, head })
    } else {
        Ok(RawRange { anchor, head })
    }
}

trait RangeParts {
    fn range_parts(&self) -> (&Rope, &RawRange);

    fn range_parts_mut(&mut self) -> (&Rope, &mut RawRange);
}

impl RangeParts for Range<'_> {
    fn range_parts(&self) -> (&Rope, &RawRange) {
        (self.rope, &self.range)
    }

    fn range_parts_mut(&mut self) -> (&Rope, &mut RawRange) {
        (self.rope, &mut self.range)
    }
}

impl RangeParts for RangeMut<'_> {
    fn range_parts(&self) -> (&Rope, &RawRange) {
        (self.rope, &self.range)
    }

    fn range_parts_mut(&mut self) -> (&Rope, &mut RawRange) {
        (self.rope, &mut self.range)
    }
}

#[allow(private_bounds)]
pub trait RangeExt: RangeParts {
    #[must_use]
    fn rope(&self) -> &Rope {
        todo!()
    }

    #[must_use]
    fn anchor(&self) -> usize {
        todo!()
    }

    #[must_use]
    fn head(&self) -> usize {
        todo!()
    }

    fn move_left(&mut self, distance: usize) -> bool {
        todo!()
    }

    fn move_right(&mut self, distance: usize) -> bool {
        todo!()
    }

    fn extend_left(&mut self, distance: usize) -> bool {
        todo!()
    }

    fn extend_right(&mut self, distance: usize) -> bool {
        todo!()
    }

    fn reduce(&mut self) {
        todo!()
    }
}
