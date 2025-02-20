use crate::{
    cursor::{snap, Bias, RawCursor},
    ot::EditSeq,
    rope::RopeExt as _,
};
use ropey::{Rope, RopeSlice};
use std::cmp::{max, min};

// TODO: It's a GAP index! If anchor == head, then the width is 0! This should only be legal at EOF.
// Gap index is the right way to go, don't back out on that, just make it work.

#[derive(Clone, Copy, Debug, Default, PartialEq)]
pub struct RawRange {
    pub anchor: RawCursor,
    pub head: RawCursor,
}

impl RawRange {
    #[must_use]
    pub fn new(rope: &Rope, anchor_gap_index: usize, head_gap_index: usize) -> Self {
        let mut anchor = RawCursor {
            gap_index: anchor_gap_index,
        };
        let mut head = RawCursor {
            gap_index: head_gap_index,
        };

        if !rope.is_grapheme_boundary(anchor.gap_index) {
            anchor.gap_index = rope.len_chars();
        }

        if !rope.is_grapheme_boundary(head.gap_index) {
            head.gap_index = rope.len_chars();
        }

        Self { anchor, head }
    }

    #[must_use]
    pub fn rope_slice<'a>(&self, rope: &'a Rope) -> RopeSlice<'a> {
        rope.slice(self.start()..=self.end())
    }

    // TODO: Return `RawCursor`s

    #[must_use]
    pub fn anchor(&self) -> usize {
        self.anchor.gap_index
    }

    #[must_use]
    pub fn head(&self) -> usize {
        self.head.gap_index
    }

    #[must_use]
    pub fn start(&self) -> usize {
        min(self.anchor.gap_index, self.head.gap_index)
    }

    #[must_use]
    pub fn end(&self) -> usize {
        max(self.anchor.gap_index, self.head.gap_index)
    }

    #[must_use]
    pub fn is_forward(&self) -> bool {
        self.anchor <= self.head
    }

    #[must_use]
    pub fn is_backward(&self) -> bool {
        self.anchor > self.head
    }

    #[must_use]
    pub fn is_reduced(&self) -> bool {
        self.anchor == self.head
    }

    pub fn move_left(&mut self, rope: &Rope, count: usize) {
        self.extend_left(rope, count);
        self.reduce();
    }

    pub fn move_right(&mut self, rope: &Rope, count: usize) {
        self.extend_right(rope, count);
        self.reduce();
    }

    pub fn extend_left(&mut self, rope: &Rope, count: usize) {
        self.head.move_left(rope, count);
    }

    pub fn extend_right(&mut self, rope: &Rope, count: usize) {
        self.head.move_right(rope, count);
    }

    pub fn flip(&mut self) {
        if !self.is_reduced() {
            std::mem::swap(&mut self.anchor, &mut self.head);
        }
    }

    pub fn flip_forward(&mut self) {
        if self.is_backward() {
            self.flip();
        }
    }

    pub fn flip_backward(&mut self) {
        if self.is_forward() {
            self.flip();
        }
    }

    pub fn reduce(&mut self) {
        self.anchor.gap_index = self.head.gap_index;
    }

    pub fn insert_char(&mut self, rope: &mut Rope, char: char) {
        self.insert(rope, &char.to_string());
    }

    pub fn insert(&mut self, rope: &mut Rope, string: &str) {
        let edits = self.head.insert_impl(rope, string);
        self.anchor.gap_index = snap(
            rope,
            edits.transform_index(self.anchor.gap_index),
            Bias::After,
        );
    }

    pub fn delete_before(&mut self, rope: &mut Rope, count: usize) {
        let edits = self.head.delete_before_impl(rope, count);
        self.anchor.gap_index = edits.transform_index(self.anchor.gap_index);
    }

    // TODO: Add grapheme awareness
    // TODO: Don't crash when range contains EOF.
    // TODO: Accept count. Can't naively write `edits.delete(count)`, otherwise you're implying
    // there exist that many characters to delete, and you'll get a length mismatch error.
    pub fn delete(&mut self, rope: &mut Rope) {
        if rope.len_chars() == 0 {
            return;
        }
        let mut edits = EditSeq::new();
        edits.retain(self.start());
        // TODO: Remove `+ 1` in favor of the end being the gap after the last grapheme, not the gap
        // before the last grapheme.
        edits.delete((self.end() - self.start()) + 1);
        edits.retain_rest(rope);
        edits.apply(rope).unwrap();
        self.anchor.gap_index = self.start();
        self.head.gap_index = self.start();
    }

    pub fn delete_after(&mut self, rope: &mut Rope, count: usize) {
        let edits = self.head.delete_after_impl(rope, count);
        self.anchor.gap_index = edits.transform_index(self.anchor.gap_index);
    }

    pub(crate) fn assert_valid(&self, rope: &Rope) {
        self.anchor.assert_valid(rope);
        self.head.assert_valid(rope);
    }
}

#[derive(Debug)]
pub struct Range<'a> {
    rope: &'a Rope,
    range: RawRange,
}

impl<'a> Range<'a> {
    #[must_use]
    pub fn new(rope: &'a Rope, anchor_gap_index: usize, head_gap_index: usize) -> Self {
        let range = RawRange::new(rope, anchor_gap_index, head_gap_index);
        Self { rope, range }
    }

    #[must_use]
    pub fn rope(self) -> &'a Rope {
        self.rope
    }

    #[must_use]
    pub fn rope_slice(self) -> RopeSlice<'a> {
        self.range.rope_slice(self.rope)
    }

    #[must_use]
    pub(crate) fn into_raw(self) -> RawRange {
        self.range
    }

    #[must_use]
    pub fn anchor(&self) -> usize {
        self.range.anchor()
    }

    #[must_use]
    pub fn head(&self) -> usize {
        self.range.head()
    }

    #[must_use]
    pub fn start(&self) -> usize {
        self.range.start()
    }

    #[must_use]
    pub fn end(&self) -> usize {
        self.range.end()
    }

    #[must_use]
    pub fn is_forward(&self) -> bool {
        self.range.is_forward()
    }

    #[must_use]
    pub fn is_backward(&self) -> bool {
        self.range.is_backward()
    }

    #[must_use]
    pub fn is_reduced(&self) -> bool {
        self.range.is_reduced()
    }

    pub fn move_left(&mut self, count: usize) {
        self.range.move_left(self.rope, count);
    }

    pub fn move_right(&mut self, count: usize) {
        self.range.move_right(self.rope, count);
    }

    pub fn extend_left(&mut self, count: usize) {
        self.range.extend_left(self.rope, count);
    }

    pub fn extend_right(&mut self, count: usize) {
        self.range.extend_right(self.rope, count);
    }

    pub fn flip(&mut self) {
        self.range.flip();
    }

    pub fn flip_forward(&mut self) {
        self.range.flip_forward();
    }

    pub fn flip_backward(&mut self) {
        self.range.flip_backward();
    }

    pub fn reduce(&mut self) {
        self.range.reduce();
    }

    pub(crate) fn assert_valid(&self) {
        self.range.assert_valid(self.rope);
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
    pub fn new(rope: &'a mut Rope, anchor_gap_index: usize, head_gap_index: usize) -> Self {
        let range = RawRange::new(rope, anchor_gap_index, head_gap_index);
        Self { rope, range }
    }

    #[must_use]
    pub fn rope(self) -> &'a Rope {
        self.rope
    }

    /// Must trade in `RangeMut` for `&mut Rope`. Upholding range invariants depends on coordinating
    /// rope and state mutations.
    #[must_use]
    pub fn into_rope_mut(self) -> &'a mut Rope {
        self.rope
    }

    #[must_use]
    pub fn rope_slice(self) -> RopeSlice<'a> {
        self.range.rope_slice(self.rope)
    }

    #[must_use]
    pub(crate) fn into_raw(self) -> RawRange {
        self.range
    }

    #[must_use]
    pub fn anchor(&self) -> usize {
        self.range.anchor()
    }

    #[must_use]
    pub fn head(&self) -> usize {
        self.range.head()
    }

    #[must_use]
    pub fn start(&self) -> usize {
        self.range.start()
    }

    #[must_use]
    pub fn end(&self) -> usize {
        self.range.end()
    }

    #[must_use]
    pub fn is_forward(&self) -> bool {
        self.range.is_forward()
    }

    #[must_use]
    pub fn is_backward(&self) -> bool {
        self.range.is_backward()
    }

    #[must_use]
    pub fn is_reduced(&self) -> bool {
        self.range.is_reduced()
    }

    pub fn move_left(&mut self, count: usize) {
        self.range.move_left(self.rope, count);
    }

    pub fn move_right(&mut self, count: usize) {
        self.range.move_right(self.rope, count);
    }

    pub fn extend_left(&mut self, count: usize) {
        self.range.extend_left(self.rope, count);
    }

    pub fn extend_right(&mut self, count: usize) {
        self.range.extend_right(self.rope, count);
    }

    pub fn flip(&mut self) {
        self.range.flip();
    }

    pub fn flip_forward(&mut self) {
        self.range.flip_forward();
    }

    pub fn flip_backward(&mut self) {
        self.range.flip_backward();
    }

    pub fn reduce(&mut self) {
        self.range.reduce();
    }

    pub fn insert_char(&mut self, char: char) {
        self.range.insert_char(self.rope, char);
    }

    pub fn insert(&mut self, string: &str) {
        self.range.insert(self.rope, string);
    }

    pub fn delete_before(&mut self, count: usize) {
        self.range.delete_before(self.rope, count);
    }

    pub fn delete(&mut self) {
        self.range.delete(self.rope);
    }

    pub fn delete_after(&mut self, count: usize) {
        self.range.delete_after(self.rope, count);
    }

    pub(crate) fn assert_valid(&self) {
        self.range.assert_valid(self.rope);
    }

    // TODO: Add `set_{head, anchor}`?
}

fn snap_to_gaps(rope: &Rope, range: RawRange) -> RawRange {
    if rope.len_chars() == 0 {
        return RawRange::default();
    }

    let (anchor_snap_bias, head_snap_bias) = if range.anchor <= range.head {
        (Bias::Before, Bias::After)
    } else {
        (Bias::After, Bias::Before)
    };

    let mut anchor = RawCursor::new(rope, range.anchor.gap_index, anchor_snap_bias);
    let mut head = RawCursor::new(rope, range.head.gap_index, head_snap_bias);

    if anchor == head {
        match rope.next_grapheme_boundary(head.gap_index) {
            Some(after) if head.gap_index != after => head.gap_index = after,
            _ => match rope.prev_grapheme_boundary(anchor.gap_index) {
                Some(before) if anchor.gap_index != before => anchor.gap_index = before,
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

    fn r(anchor_gap_index: usize, head_gap_index: usize) -> RawRange {
        RawRange {
            anchor: RawCursor {
                gap_index: anchor_gap_index,
            },
            head: RawCursor {
                gap_index: head_gap_index,
            },
        }
    }

    #[test]
    fn snapping() {
        let rope = Rope::new();
        assert_eq!(snap_to_gaps(&rope, r(42, 42)), r(0, 0));
        assert_eq!(snap_to_gaps(&rope, r(42, 69)), r(0, 0));
        let rope = Rope::from_str("👨🏻‍❤️‍💋‍👨🏻");
        assert_eq!(snap_to_gaps(&rope, r(0, 0)), r(0, 10));
        assert_eq!(snap_to_gaps(&rope, r(1, 9)), r(0, 10));
        assert_eq!(snap_to_gaps(&rope, r(42, 42)), r(0, 10));
        assert_eq!(snap_to_gaps(&rope, r(42, 69)), r(0, 10));
    }

    #[test]
    fn insert_changes_grapheme_boundary() {
        let mut rope = Rope::from_str("\u{0301}"); // combining acute accent (´)
        let mut range = RangeMut::new(&mut rope, 0, 0);
        range.insert("e");
        range.assert_valid();
    }

    #[test]
    fn snapping_fuzz() {
        arbtest(|u| {
            let rope = Rope::from_str(u.arbitrary()?);
            let range = r(u.arbitrary()?, u.arbitrary()?);
            let result = std::panic::catch_unwind(|| snap_to_gaps(&rope, range));
            assert!(result.is_ok());
            Ok(())
        });
    }
}
