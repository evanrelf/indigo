use crate::{ot::EditSeq, rope::RopeExt};
use ropey::Rope;
use std::cmp::{max, min};

// TODO: It's a GAP index! If anchor == head, then the width is 0! This should only be legal at EOF.
// Gap index is the right way to go, don't back out on that, just make it work.
// TODO: Implement `Clone` and `Copy` for all the types in this module?

#[derive(Debug, Default)]
pub(crate) struct RawRange {
    pub(crate) anchor: usize, // char gap index
    pub(crate) head: usize,   // char gap index
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

    #[must_use]
    pub(crate) fn into_raw(self) -> RawRange {
        self.range
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

    #[must_use]
    pub(crate) fn into_raw(self) -> RawRange {
        self.range
    }

    pub fn insert_char(&mut self, char: char) {
        self.insert(&char.to_string());
    }

    pub fn insert(&mut self, string: &str) {
        let mut edits = EditSeq::new();
        edits.retain(self.range.head);
        edits.insert(string);
        edits.retain(self.rope.len_chars() - self.range.head);
        edits.apply(self.rope).unwrap();
        self.range.anchor = edits.transform_index(self.range.anchor);
        self.range.head = edits.transform_index(self.range.head);
    }

    // TODO: wtf this always crashes
    // TODO: Don't crash when backspacing with a non-reduced range.
    // TODO: Accept count. Can't naively write `edits.delete(count)`, otherwise you're implying
    // there exist that many characters to delete, and you'll get a length mismatch error.
    pub fn backspace(&mut self) {
        if self.range.head == 0 {
            return;
        }
        let mut edits = EditSeq::new();
        edits.retain(self.range.head.saturating_sub(1));
        edits.delete(1);
        // TODO: `EditSeq::retain_rest` (or implicitly do this) would be really nice.
        edits.retain(self.rope.len_chars().saturating_sub(self.range.head + 1));
        edits.apply(self.rope).unwrap();
        self.range.anchor = edits.transform_index(self.range.anchor);
        self.range.head = edits.transform_index(self.range.head);
    }

    // TODO: Don't crash when range contains EOF.
    // TODO: Accept count. Can't naively write `edits.delete(count)`, otherwise you're implying
    // there exist that many characters to delete, and you'll get a length mismatch error.
    pub fn delete(&mut self) {
        if self.rope.len_chars() == 0 {
            return;
        }
        let mut edits = EditSeq::new();
        edits.retain(self.start());
        edits.delete((self.end() - self.start()) + 1);
        edits.retain(self.rope.len_chars().saturating_sub(self.end() + 1));
        edits.apply(self.rope).unwrap();
        self.range.anchor = self.start();
        self.range.head = self.start();
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

trait AsRangeParts {
    fn as_range_parts(&self) -> (&Rope, &RawRange);

    fn as_range_parts_mut(&mut self) -> (&Rope, &mut RawRange);
}

impl AsRangeParts for Range<'_> {
    fn as_range_parts(&self) -> (&Rope, &RawRange) {
        (self.rope, &self.range)
    }

    fn as_range_parts_mut(&mut self) -> (&Rope, &mut RawRange) {
        (self.rope, &mut self.range)
    }
}

impl AsRangeParts for RangeMut<'_> {
    fn as_range_parts(&self) -> (&Rope, &RawRange) {
        (self.rope, &self.range)
    }

    fn as_range_parts_mut(&mut self) -> (&Rope, &mut RawRange) {
        (self.rope, &mut self.range)
    }
}

#[allow(private_bounds)]
pub trait RangeExt: AsRangeParts {
    #[must_use]
    fn rope(&self) -> &Rope {
        let (rope, _range) = self.as_range_parts();
        rope
    }

    #[must_use]
    fn anchor(&self) -> usize {
        let (_rope, range) = self.as_range_parts();
        range.anchor
    }

    #[must_use]
    fn head(&self) -> usize {
        let (_rope, range) = self.as_range_parts();
        range.head
    }

    #[must_use]
    fn start(&self) -> usize {
        min(self.anchor(), self.head())
    }

    #[must_use]
    fn end(&self) -> usize {
        max(self.anchor(), self.head())
    }

    #[must_use]
    fn is_forward(&self) -> bool {
        self.anchor() <= self.head()
    }

    #[must_use]
    fn is_backward(&self) -> bool {
        self.anchor() > self.head()
    }

    #[must_use]
    fn is_reduced(&self) -> bool {
        self.anchor() == self.head()
    }

    fn move_left(&mut self, distance: usize) {
        self.extend_left(distance);
        self.reduce();
    }

    fn move_right(&mut self, distance: usize) {
        self.extend_right(distance);
        self.reduce();
    }

    fn extend_left(&mut self, distance: usize) {
        let (rope, range) = self.as_range_parts_mut();
        for _ in 1..=distance {
            match rope.get_prev_grapheme_boundary(range.head) {
                Ok(head) if range.head != head => range.head = head,
                _ => break,
            }
        }
    }

    fn flip(&mut self) {
        if !self.is_reduced() {
            let (_rope, range) = self.as_range_parts_mut();
            std::mem::swap(&mut range.anchor, &mut range.head);
        }
    }

    fn flip_forward(&mut self) {
        if self.is_backward() {
            self.flip();
        }
    }

    fn flip_backward(&mut self) {
        if self.is_forward() {
            self.flip();
        }
    }

    fn extend_right(&mut self, distance: usize) {
        let (rope, range) = self.as_range_parts_mut();
        for _ in 1..=distance {
            match rope.get_next_grapheme_boundary(range.head) {
                Ok(head) if range.head != head => range.head = head,
                _ => break,
            }
        }
    }

    fn reduce(&mut self) {
        let (_rope, range) = self.as_range_parts_mut();
        range.anchor = range.head;
    }
}

impl<T> RangeExt for T where T: AsRangeParts {}
