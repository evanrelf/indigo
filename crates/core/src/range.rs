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
pub struct RawRange {
    pub anchor: usize, // char gap index
    pub head: usize,   // char gap index
}

impl RawRange {
    #[must_use]
    pub fn new(rope: &Rope, mut anchor: usize, mut head: usize) -> Self {
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

        Self { anchor, head }
    }

    #[must_use]
    pub fn start(&self) -> usize {
        min(self.anchor, self.head)
    }

    #[must_use]
    pub fn end(&self) -> usize {
        max(self.anchor, self.head)
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

    pub fn move_left(&mut self, rope: &Rope, distance: usize) {
        self.extend_left(rope, distance);
        self.reduce();
    }

    pub fn move_right(&mut self, rope: &Rope, distance: usize) {
        self.extend_right(rope, distance);
        self.reduce();
    }

    pub fn extend_left(&mut self, rope: &Rope, distance: usize) {
        for _ in 1..=distance {
            match rope.get_prev_grapheme_boundary(self.head) {
                Ok(head) if self.head != head => self.head = head,
                _ => break,
            }
        }
    }

    pub fn extend_right(&mut self, rope: &Rope, distance: usize) {
        for _ in 1..=distance {
            match rope.get_next_grapheme_boundary(self.head) {
                Ok(head) if self.head != head => self.head = head,
                _ => break,
            }
        }
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
        self.anchor = self.head;
    }

    pub fn insert_char(&mut self, rope: &mut Rope, char: char) {
        self.insert(rope, &char.to_string());
    }

    pub fn insert(&mut self, rope: &mut Rope, string: &str) {
        let mut head = RawCursor {
            gap_index: self.head,
        };
        let edits = head.insert_impl(rope, string);
        let mut anchor = RawCursor {
            gap_index: edits.transform_index(self.anchor),
        };
        anchor.snap(rope, Bias::After);
        self.anchor = anchor.gap_index;
        self.head = head.gap_index;
    }

    pub fn delete_before(&mut self, rope: &mut Rope, count: usize) {
        let mut head = RawCursor {
            gap_index: self.head,
        };
        let edits = head.delete_before_impl(rope, count);
        self.anchor = edits.transform_index(self.anchor);
        self.head = head.gap_index;
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
        self.anchor = self.start();
        self.head = self.start();
    }

    pub fn delete_after(&mut self, rope: &mut Rope, count: usize) {
        let mut head = RawCursor {
            gap_index: self.head,
        };
        let edits = head.delete_after_impl(rope, count);
        self.anchor = edits.transform_index(self.anchor);
        self.head = head.gap_index;
    }

    pub(crate) fn is_valid(&self, rope: &Rope) -> bool {
        let anchor = RawCursor {
            gap_index: self.anchor,
        };
        let head = RawCursor {
            gap_index: self.head,
        };
        anchor.is_valid(rope) && head.is_valid(rope)
    }
}

#[derive(Debug)]
pub struct Range<'a> {
    rope: &'a Rope,
    range: RawRange,
}

impl<'a> Range<'a> {
    #[must_use]
    pub fn new(rope: &'a Rope, anchor: usize, head: usize) -> Self {
        let range = RawRange::new(rope, anchor, head);
        Self { rope, range }
    }

    #[must_use]
    pub(crate) fn into_raw(self) -> RawRange {
        self.range
    }

    #[must_use]
    pub fn anchor(&self) -> usize {
        self.range.anchor
    }

    #[must_use]
    pub fn head(&self) -> usize {
        self.range.head
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

    pub fn move_left(&mut self, distance: usize) {
        self.range.move_left(self.rope, distance);
    }

    pub fn move_right(&mut self, distance: usize) {
        self.range.move_right(self.rope, distance);
    }

    pub fn extend_left(&mut self, distance: usize) {
        self.range.extend_left(self.rope, distance);
    }

    pub fn extend_right(&mut self, distance: usize) {
        self.range.extend_right(self.rope, distance);
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

    pub(crate) fn is_valid(&self) -> bool {
        self.range.is_valid(self.rope)
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
        let range = RawRange::new(rope, anchor, head);
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

    #[must_use]
    pub fn anchor(&self) -> usize {
        self.range.anchor
    }

    #[must_use]
    pub fn head(&self) -> usize {
        self.range.head
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

    pub fn move_left(&mut self, distance: usize) {
        self.range.move_left(self.rope, distance);
    }

    pub fn move_right(&mut self, distance: usize) {
        self.range.move_right(self.rope, distance);
    }

    pub fn extend_left(&mut self, distance: usize) {
        self.range.extend_left(self.rope, distance);
    }

    pub fn extend_right(&mut self, distance: usize) {
        self.range.extend_right(self.rope, distance);
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

    pub(crate) fn is_valid(&self) -> bool {
        self.range.is_valid(self.rope)
    }

    // TODO: Add `set_{head, anchor}`?
}

fn snap_to_gaps(rope: &Rope, mut anchor: usize, mut head: usize) -> RawRange {
    if rope.len_chars() == 0 {
        return RawRange { anchor: 0, head: 0 };
    }

    match anchor.cmp(&head) {
        Ordering::Less | Ordering::Equal => {
            anchor = RawCursor::new(rope, anchor, Bias::Before).gap_index;
            head = RawCursor::new(rope, head, Bias::After).gap_index;
        }
        Ordering::Greater => {
            head = RawCursor::new(rope, head, Bias::Before).gap_index;
            anchor = RawCursor::new(rope, anchor, Bias::After).gap_index;
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
    fn insert_changes_grapheme_boundary() {
        let mut rope = Rope::from_str("\u{0301}"); // combining acute accent (´)
        let mut range = RangeMut::new(&mut rope, 0, 0);
        range.insert("e");
        let anchor = range.anchor();
        let head = range.head();
        assert!(
            range.is_valid(),
            "range not on grapheme boundary\nrope = {rope:?}\nanchor = {anchor}\nhead = {head}"
        );
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
