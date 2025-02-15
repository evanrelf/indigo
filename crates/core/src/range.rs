use crate::{
    cursor::{Bias, RawCursor},
    ot::EditSeq,
    rope::RopeExt as _,
};
use ropey::Rope;
use std::cmp::{max, min, Ordering};

// TODO: It's a GAP index! If anchor == head, then the width is 0! This should only be legal at EOF.
// Gap index is the right way to go, don't back out on that, just make it work.
// TODO: Implement `Clone` and `Copy` for all the types in this module?

// Fields should be public because there are no invariants to enforce. Correctness is only
// meaningful relative to a rope, which this type does not control.
#[derive(Debug, Default, PartialEq)]
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
    #[must_use]
    pub fn new(rope: &'a Rope, anchor: usize, head: usize) -> Self {
        let range = new_impl(rope, anchor, head);
        Self { rope, range }
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
    #[must_use]
    pub fn new(rope: &'a mut Rope, anchor: usize, head: usize) -> Self {
        let range = new_impl(rope, anchor, head);
        Self { rope, range }
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
        edits.retain_rest(self.rope);
        edits.apply(self.rope).unwrap();
        self.range.anchor = edits.transform_index(self.range.anchor);
        self.range.head = edits.transform_index(self.range.head);
    }

    // TODO: Accept count. Can't naively write `edits.delete(count)`, otherwise you're implying
    // there exist that many characters to delete, and you'll get a length mismatch error.
    // TODO: Implement backspace in terms of `move_left` (on a copy of the range) + `delete`.
    pub fn backspace(&mut self) {
        if let Ok(index) = self.rope.get_prev_grapheme_boundary(self.range.head) {
            let mut edits = EditSeq::new();
            edits.retain(index);
            edits.delete(self.range.head - index);
            edits.retain_rest(self.rope);
            edits.apply(self.rope).unwrap();
            self.range.anchor = edits.transform_index(self.range.anchor);
            self.range.head = edits.transform_index(self.range.head);
        }
    }

    // TODO: Add grapheme awareness
    // TODO: Don't crash when range contains EOF.
    // TODO: Accept count. Can't naively write `edits.delete(count)`, otherwise you're implying
    // there exist that many characters to delete, and you'll get a length mismatch error.
    pub fn delete(&mut self) {
        if self.rope.len_chars() == 0 {
            return;
        }
        let mut edits = EditSeq::new();
        edits.retain(self.start());
        // TODO: Remove `+ 1` in favor of the end being the gap after the last grapheme, not the gap
        // before the last grapheme.
        edits.delete((self.end() - self.start()) + 1);
        edits.retain_rest(self.rope);
        edits.apply(self.rope).unwrap();
        self.range.anchor = self.start();
        self.range.head = self.start();
    }

    // TODO: Add `set_{head, anchor}`?
}

fn new_impl(rope: &Rope, mut anchor: usize, mut head: usize) -> RawRange {
    // Gap at end of file counts as grapheme boundary
    if let Ok(true) = rope.try_is_grapheme_boundary(anchor) {
        //
    } else {
        anchor = rope.len_chars();
    }

    // Gap at end of file counts as grapheme boundary
    if let Ok(true) = rope.try_is_grapheme_boundary(head) {
        //
    } else {
        head = rope.len_chars();
    }

    RawRange { anchor, head }
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

    fn extend_right(&mut self, distance: usize) {
        let (rope, range) = self.as_range_parts_mut();
        for _ in 1..=distance {
            match rope.get_next_grapheme_boundary(range.head) {
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

    fn reduce(&mut self) {
        let (_rope, range) = self.as_range_parts_mut();
        range.anchor = range.head;
    }
}

impl<T> RangeExt for T where T: AsRangeParts {}

fn snap_to_gaps(rope: &Rope, mut anchor: usize, mut head: usize) -> RawRange {
    if rope.len_chars() == 0 {
        return RawRange { anchor: 0, head: 0 };
    }

    match anchor.cmp(&head) {
        Ordering::Less | Ordering::Equal => {
            anchor = RawCursor::new(rope, anchor, Bias::Before).index;
            head = RawCursor::new(rope, head, Bias::After).index;
        }
        Ordering::Greater => {
            head = RawCursor::new(rope, head, Bias::Before).index;
            anchor = RawCursor::new(rope, anchor, Bias::After).index;
        }
    }

    if anchor == head {
        match rope.get_next_grapheme_boundary(head) {
            Ok(after) if head != after => head = after,
            _ => match rope.get_prev_grapheme_boundary(anchor) {
                Ok(before) if anchor != before => anchor = before,
                _ => unreachable!(),
            },
        }
    }

    RawRange { anchor, head }
}

#[cfg(test)]
mod tests {
    use super::*;
    use arbtest::arbtest;

    // Adjust budget with `ARBTEST_BUDGET_MS` environment variable

    #[test]
    fn snapping() {
        let r = |anchor, head| RawRange { anchor, head };
        let rope = Rope::new();
        assert_eq!(snap_to_gaps(&rope, 42, 42), r(0, 0));
        assert_eq!(snap_to_gaps(&rope, 42, 69), r(0, 0));
        let rope = Rope::from_str("👨🏻‍❤️‍💋‍👨🏻");
        assert_eq!(snap_to_gaps(&rope, 0, 0), r(0, 10));
        assert_eq!(snap_to_gaps(&rope, 1, 9), r(0, 10));
        assert_eq!(snap_to_gaps(&rope, 42, 42), r(0, 10));
        assert_eq!(snap_to_gaps(&rope, 42, 69), r(0, 10));
    }

    #[test]
    fn snapping_fuzz() {
        arbtest(|u| {
            let rope = Rope::from_str(u.arbitrary()?);
            let anchor = u.arbitrary()?;
            let head = u.arbitrary()?;
            let result = std::panic::catch_unwind(|| snap_to_gaps(&rope, anchor, head));
            assert!(result.is_ok());
            Ok(())
        });
    }
}
