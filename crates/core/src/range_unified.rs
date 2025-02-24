use crate::{
    cursor_unified::Cursor,
    ot::EditSeq,
    rope::{Bias, RopeExt as _},
};
use ropey::{Rope, RopeSlice};
use std::{
    borrow::{Borrow, BorrowMut},
    cmp::{max, min},
    num::NonZeroUsize,
};

// TODO: Movement with counts is broken.

#[derive(Clone, Copy, Debug, Default, PartialEq)]
pub struct Range<R> {
    rope: R,
    anchor: Cursor<()>,
    head: Cursor<()>,
}

impl Range<()> {
    #[must_use]
    pub fn new(anchor_byte_gap_index: usize, head_byte_gap_index: usize) -> Self {
        Self {
            rope: (),
            anchor: Cursor::new(anchor_byte_gap_index),
            head: Cursor::new(head_byte_gap_index),
        }
    }
}

impl<R> Range<R> {
    #[must_use]
    pub fn with_rope<R2>(mut self, rope: R2) -> Range<R2>
    where
        R2: Borrow<Rope>,
    {
        if self.anchor() == self.head() && rope.borrow().len_bytes() > 0 {
            self.head.set_byte_gap_index(self.head() + 1);
        }
        let (anchor_snap_bias, head_snap_bias) = if self.anchor() < self.head() {
            (Bias::Before, Bias::After)
        } else {
            (Bias::After, Bias::Before)
        };
        let anchor = Cursor::new(self.anchor())
            .with_rope(rope.borrow(), anchor_snap_bias)
            .without_rope();
        let head = Cursor::new(self.head())
            .with_rope(rope.borrow(), head_snap_bias)
            .without_rope();
        let range = Range { rope, anchor, head };
        range.assert_valid();
        range
    }

    #[must_use]
    pub fn try_with_rope<R2>(self, rope: R2) -> Option<Range<R2>>
    where
        R2: Borrow<Rope>,
    {
        let anchor = Cursor::new(self.anchor())
            .try_with_rope(rope.borrow())?
            .without_rope();
        let head = Cursor::new(self.head())
            .try_with_rope(rope.borrow())?
            .without_rope();
        let range = Range { rope, anchor, head };
        range.assert_valid();
        Some(range)
    }

    #[must_use]
    pub fn without_rope(self) -> Range<()> {
        Range {
            rope: (),
            anchor: self.anchor,
            head: self.head,
        }
    }

    #[must_use]
    pub fn anchor(&self) -> usize {
        self.anchor.byte_gap_index()
    }

    #[must_use]
    pub fn head(&self) -> usize {
        self.head.byte_gap_index()
    }

    #[must_use]
    pub fn start(&self) -> usize {
        min(self.anchor(), self.head())
    }

    #[must_use]
    pub fn end(&self) -> usize {
        max(self.anchor(), self.head())
    }

    #[must_use]
    pub fn byte_length(&self) -> usize {
        self.end() - self.start()
    }

    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.byte_length() == 0
    }

    #[must_use]
    pub fn is_forward(&self) -> bool {
        self.anchor <= self.head
    }

    #[must_use]
    pub fn is_backward(&self) -> bool {
        self.anchor > self.head
    }
}

impl<R> Range<R>
where
    R: Borrow<Rope>,
{
    #[must_use]
    pub fn rope(&self) -> &Rope {
        self.rope.borrow()
    }

    #[must_use]
    pub fn rope_slice<'a>(&self) -> RopeSlice<'a> {
        self.anchor.assert_valid();
        self.head.assert_valid();
        self.rope().slice(self.start()..self.end())
    }

    #[must_use]
    pub fn grapheme_length(&self) -> usize {
        match self.byte_length() {
            0 => 0,
            1 => 1,
            _ => self.rope_slice().graphemes().count(),
        }
    }

    #[must_use]
    pub fn is_eof(&self) -> bool {
        self.is_empty() && self.head.is_eof()
    }

    #[tracing::instrument(skip_all)]
    pub fn extend_left(&mut self, count: NonZeroUsize) {
        self.head.move_left(count);
        if self.anchor() == 0 && self.head() == 0 {
            self.head.move_right(NonZeroUsize::MIN);
            return;
        }
        if self.is_empty() {
            self.anchor.move_right(NonZeroUsize::MIN);
            self.head.move_left(NonZeroUsize::MIN);
            let grapheme_length = self.grapheme_length();
            if grapheme_length != 2 {
                tracing::warn!(
                    grapheme_length,
                    text = ?self.rope_slice(),
                    "Unexpected grapheme length != 2 after crossover",
                );
            }
            return;
        }
        if self.grapheme_length() == 1 {
            self.flip_forward();
        }
    }

    // TODO: Allow moving both cursors to EOF
    #[tracing::instrument(skip_all)]
    pub fn extend_right(&mut self, count: NonZeroUsize) {
        self.head.move_right(count);
        if self.is_empty() && !self.is_eof() {
            self.anchor.move_left(NonZeroUsize::MIN);
            self.head.move_right(NonZeroUsize::MIN);
            let grapheme_length = self.grapheme_length();
            if grapheme_length != 2 {
                tracing::warn!(
                    grapheme_length,
                    text = ?self.rope_slice(),
                    "Unexpected grapheme length != 2 after crossover",
                );
            }
            return;
        }
        if self.grapheme_length() == 1 {
            self.flip_forward();
        }
    }

    pub fn flip(&mut self) {
        if self.is_forward() && self.grapheme_length() == 1 {
            return;
        }
        std::mem::swap(&mut self.anchor, &mut self.head);
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

    #[tracing::instrument(skip_all)]
    pub fn reduce(&mut self) {
        if self.is_eof() {
            return;
        }
        if self.is_forward() {
            self.anchor = self.head;
            self.anchor.move_left(NonZeroUsize::MIN);
        } else {
            self.anchor = self.head;
            self.head.move_right(NonZeroUsize::MIN);
        }
        let grapheme_length = self.grapheme_length();
        if grapheme_length != 1 {
            tracing::warn!(
                grapheme_length,
                text = ?self.rope_slice(),
                "Unexpected grapheme length != 1 after reduce",
            );
        }
    }

    pub(crate) fn assert_valid(&self) {
        self.anchor.assert_valid();
        self.head.assert_valid();
        assert!(
            !self.is_empty() || self.is_eof(),
            "Range empty but not at EOF (anchor={}, head={})",
            self.anchor(),
            self.head(),
        );
        assert!(
            self.is_forward() || self.grapheme_length() > 1,
            "Range reduced but not facing forward (anchor={}, head={})",
            self.anchor(),
            self.head(),
        );
    }
}

impl<R> Range<R>
where
    R: BorrowMut<Rope>,
{
    #[must_use]
    fn rope_mut(&mut self) -> &mut Rope {
        self.rope.borrow_mut()
    }

    pub fn insert_char(&mut self, char: char) {
        self.insert(&char.to_string());
    }

    pub fn insert(&mut self, string: &str) {
        let mut range = *self;
        range.reduce();
        let edits = range.anchor.insert_impl(string);
        *self = Range::new(
            edits.transform_index(self.anchor()),
            edits.transform_index(self.head()),
        )
        .with_rope(self.rope);
    }

    pub fn delete_before(&mut self, count: NonZeroUsize) {
        let mut range = *self;
        range.reduce();
        let edits = range.anchor.delete_before_impl(count);
        self.anchor
            .set_byte_gap_index(edits.transform_index(self.anchor()));
        self.head
            .set_byte_gap_index(edits.transform_index(self.head()));
    }

    pub fn delete(&mut self) {
        if self.is_empty() {
            return;
        }
        let mut edits = EditSeq::new();
        edits.retain(self.start());
        edits.delete(self.byte_length());
        edits.retain_rest(self.rope());
        edits.apply(self.rope_mut()).unwrap();
        self.anchor
            .set_byte_gap_index(edits.transform_index(self.anchor()));
        self.head
            .set_byte_gap_index(edits.transform_index(self.head()));
        assert_eq!(self.anchor, self.head);
        self.extend_right(NonZeroUsize::MIN);
    }

    pub fn delete_after(&mut self, count: NonZeroUsize) {
        let mut range = *self;
        range.reduce();
        let edits = range
            .head
            .try_with_rope(self.rope_mut())
            .unwrap()
            .delete_after_impl(count);
        self.anchor
            .set_byte_gap_index(edits.transform_index(self.anchor()));
        self.head
            .set_byte_gap_index(edits.transform_index(self.head()));
    }
}

/*

impl<'a> RangeMut<'a> {

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
    pub(crate) fn raw(&self) -> Range {
        self.range
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn insert_changes_grapheme_boundary() {
        let mut rope = Rope::from_str("\u{0301}"); // combining acute accent (´)
        let mut range = RangeMut::new_snapped(&mut rope, 0, 0);
        range.insert("e");
        range.assert_valid();
    }
}

*/
