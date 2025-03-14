use crate::{
    cursor::Cursor,
    ot::EditSeq,
    rope::{RopeExt as _, SnapBias},
};
use ropey::{Rope, RopeSlice};
use std::{
    cmp::{max, min},
    num::NonZeroUsize,
};

// TODO: Movement with counts is broken.

#[derive(Clone, Debug, Default, PartialEq)]
pub struct RawRange {
    pub anchor: Cursor<()>,
    pub head: Cursor<()>,
}

impl RawRange {
    #[must_use]
    pub fn new(rope: &Rope, anchor_char_offset: usize, head_char_offset: usize) -> Option<Self> {
        let anchor = Cursor::new(anchor_char_offset)
            .clone()
            .try_with(rope)?
            .without();
        let head = Cursor::new(head_char_offset)
            .clone()
            .try_with(rope)?
            .without();
        let range = Self { anchor, head };
        range.assert_valid(rope);
        Some(range)
    }

    #[must_use]
    pub fn new_snapped(
        rope: &Rope,
        anchor_char_offset: usize,
        mut head_char_offset: usize,
    ) -> Self {
        if anchor_char_offset == head_char_offset && rope.len_chars() > 0 {
            head_char_offset += 1;
        }
        let (anchor_snap_bias, head_snap_bias) = if anchor_char_offset < head_char_offset {
            (SnapBias::Before, SnapBias::After)
        } else {
            (SnapBias::After, SnapBias::Before)
        };
        let anchor = Cursor::new(anchor_char_offset)
            .with(rope, anchor_snap_bias)
            .without();
        let head = Cursor::new(head_char_offset)
            .with(rope, head_snap_bias)
            .without();
        let range = Self { anchor, head };
        range.assert_valid(rope);
        range
    }

    #[must_use]
    pub fn rope_slice<'a>(&self, rope: &'a Rope) -> RopeSlice<'a> {
        self.anchor.clone().try_with(rope).unwrap().assert_valid();
        self.head.clone().try_with(rope).unwrap().assert_valid();
        rope.slice(self.start()..self.end())
    }

    #[must_use]
    pub fn anchor(&self) -> usize {
        self.anchor.char_offset()
    }

    #[must_use]
    pub fn head(&self) -> usize {
        self.head.char_offset()
    }

    #[must_use]
    pub fn start(&self) -> usize {
        min(&self.anchor, &self.head).char_offset()
    }

    #[must_use]
    pub fn end(&self) -> usize {
        max(&self.anchor, &self.head).char_offset()
    }

    #[must_use]
    pub fn char_length(&self) -> usize {
        self.end() - self.start()
    }

    #[must_use]
    pub fn grapheme_length(&self, rope: &Rope) -> usize {
        match self.char_length() {
            0 => 0,
            1 => 1,
            _ => self.rope_slice(rope).graphemes().count(),
        }
    }

    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.char_length() == 0
    }

    #[must_use]
    pub fn is_eof(&self, rope: &Rope) -> bool {
        self.is_empty() && self.head.clone().try_with(rope).unwrap().is_eof()
    }

    #[must_use]
    pub fn is_forward(&self) -> bool {
        self.anchor <= self.head
    }

    #[must_use]
    pub fn is_backward(&self) -> bool {
        self.anchor > self.head
    }

    #[tracing::instrument(skip_all)]
    pub fn extend_left(&mut self, rope: &Rope, count: NonZeroUsize) {
        {
            let mut head = self.head.clone().try_with(rope).unwrap();
            head.move_left(count);
            self.head = head.without();
        }
        if self.anchor() == 0 && self.head() == 0 {
            {
                let mut head = self.head.clone().try_with(rope).unwrap();
                head.move_right(NonZeroUsize::MIN);
                self.head = head.without();
            }
            return;
        }
        if self.is_empty() {
            {
                let mut anchor = self.anchor.clone().try_with(rope).unwrap();
                anchor.move_right(NonZeroUsize::MIN);
                self.anchor = anchor.without();
            }
            {
                let mut head = self.head.clone().try_with(rope).unwrap();
                head.move_left(NonZeroUsize::MIN);
                self.head = head.without();
            }
            let grapheme_length = self.grapheme_length(rope);
            if grapheme_length != 2 {
                tracing::warn!(
                    grapheme_length,
                    text = ?self.rope_slice(rope),
                    "Unexpected grapheme length != 2 after crossover",
                );
            }
            return;
        }
        if self.grapheme_length(rope) == 1 {
            self.flip_forward(rope);
        }
    }

    // TODO: Allow moving both cursors to EOF
    #[tracing::instrument(skip_all)]
    pub fn extend_right(&mut self, rope: &Rope, count: NonZeroUsize) {
        {
            let mut head = self.head.clone().try_with(rope).unwrap();
            head.move_right(count);
            self.head = head.without();
        }
        if self.is_empty() && !self.is_eof(rope) {
            {
                let mut anchor = self.anchor.clone().try_with(rope).unwrap();
                anchor.move_left(NonZeroUsize::MIN);
                self.anchor = anchor.without();
            }
            {
                let mut head = self.head.clone().try_with(rope).unwrap();
                head.move_right(NonZeroUsize::MIN);
                self.head = head.without();
            }
            let grapheme_length = self.grapheme_length(rope);
            if grapheme_length != 2 {
                tracing::warn!(
                    grapheme_length,
                    text = ?self.rope_slice(rope),
                    "Unexpected grapheme length != 2 after crossover",
                );
            }
            return;
        }
        if self.grapheme_length(rope) == 1 {
            self.flip_forward(rope);
        }
    }

    pub fn flip(&mut self, rope: &Rope) {
        if self.is_forward() && self.grapheme_length(rope) == 1 {
            return;
        }
        std::mem::swap(&mut self.anchor, &mut self.head);
    }

    pub fn flip_forward(&mut self, rope: &Rope) {
        if self.is_backward() {
            self.flip(rope);
        }
    }

    pub fn flip_backward(&mut self, rope: &Rope) {
        if self.is_forward() {
            self.flip(rope);
        }
    }

    #[tracing::instrument(skip_all)]
    pub fn reduce(&mut self, rope: &Rope) {
        if self.is_eof(rope) {
            return;
        }
        if self.is_forward() {
            self.anchor = self.head.clone();
            {
                let mut anchor = self.anchor.clone().try_with(rope).unwrap();
                anchor.move_left(NonZeroUsize::MIN);
                self.anchor = anchor.without();
            }
        } else {
            self.anchor = self.head.clone();
            {
                let mut head = self.head.clone().try_with(rope).unwrap();
                head.move_right(NonZeroUsize::MIN);
                self.head = head.without();
            }
        }
        let grapheme_length = self.grapheme_length(rope);
        if grapheme_length != 1 {
            tracing::warn!(
                grapheme_length,
                text = ?self.rope_slice(rope),
                "Unexpected grapheme length != 1 after reduce",
            );
        }
    }

    pub fn insert_char(&mut self, rope: &mut Rope, char: char) {
        self.insert(rope, &char.to_string());
    }

    pub fn insert(&mut self, rope: &mut Rope, string: &str) {
        let mut range = self.clone();
        range.reduce(rope);
        let edits = range
            .anchor
            .clone()
            .try_with(&mut *rope)
            .unwrap()
            .insert_impl(string);
        *self = Self::new_snapped(
            rope,
            edits.transform_char_offset(self.anchor.char_offset()),
            edits.transform_char_offset(self.head.char_offset()),
        );
    }

    pub fn delete_before(&mut self, rope: &mut Rope, count: NonZeroUsize) {
        let mut range = self.clone();
        range.reduce(rope);
        let edits = range
            .anchor
            .clone()
            .try_with(rope)
            .unwrap()
            .delete_before_impl(count);
        self.anchor
            .set_char_offset(edits.transform_char_offset(self.anchor.char_offset()));
        self.head
            .set_char_offset(edits.transform_char_offset(self.head.char_offset()));
    }

    pub fn delete(&mut self, rope: &mut Rope) {
        if self.is_empty() {
            return;
        }
        let mut edits = EditSeq::new();
        edits.retain(self.start());
        edits.delete(self.char_length());
        edits.retain_rest(rope);
        edits.apply(rope).unwrap();
        self.anchor
            .set_char_offset(edits.transform_char_offset(self.anchor.char_offset()));
        self.head
            .set_char_offset(edits.transform_char_offset(self.head.char_offset()));
        debug_assert_eq!(self.anchor, self.head);
        self.extend_right(rope, NonZeroUsize::MIN);
    }

    pub fn delete_after(&mut self, rope: &mut Rope, count: NonZeroUsize) {
        let mut range = self.clone();
        range.reduce(rope);
        let edits = range
            .head
            .clone()
            .try_with(rope)
            .unwrap()
            .delete_after_impl(count);
        self.anchor
            .set_char_offset(edits.transform_char_offset(self.anchor.char_offset()));
        self.head
            .set_char_offset(edits.transform_char_offset(self.head.char_offset()));
    }

    pub(crate) fn assert_valid(&self, rope: &Rope) {
        self.anchor.clone().try_with(rope).unwrap().assert_valid();
        self.head.clone().try_with(rope).unwrap().assert_valid();
        debug_assert!(
            !self.is_empty() || self.is_eof(rope),
            "Range empty but not at EOF (anchor={}, head={})",
            self.anchor.char_offset(),
            self.head.char_offset(),
        );
        debug_assert!(
            self.is_forward() || self.grapheme_length(rope) > 1,
            "Range reduced but not facing forward (anchor={}, head={})",
            self.anchor.char_offset(),
            self.head.char_offset(),
        );
    }
}

#[derive(Debug)]
pub struct Range<'a> {
    rope: &'a Rope,
    range: RawRange,
}

impl<'a> Range<'a> {
    #[must_use]
    pub fn new(rope: &'a Rope, anchor_char_offset: usize, head_char_offset: usize) -> Option<Self> {
        let range = RawRange::new(rope, anchor_char_offset, head_char_offset)?;
        Some(Self { rope, range })
    }

    #[must_use]
    pub fn new_snapped(rope: &'a Rope, anchor_char_offset: usize, head_char_offset: usize) -> Self {
        let range = RawRange::new_snapped(rope, anchor_char_offset, head_char_offset);
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
    pub(crate) fn raw(&self) -> &RawRange {
        &self.range
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
    pub fn char_length(&self) -> usize {
        self.range.char_length()
    }

    #[must_use]
    pub fn grapheme_length(&self) -> usize {
        self.range.grapheme_length(self.rope)
    }

    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.range.is_empty()
    }

    #[must_use]
    pub fn is_eof(&self) -> bool {
        self.range.is_eof(self.rope)
    }

    #[must_use]
    pub fn is_forward(&self) -> bool {
        self.range.is_forward()
    }

    #[must_use]
    pub fn is_backward(&self) -> bool {
        self.range.is_backward()
    }

    pub fn extend_left(&mut self, count: NonZeroUsize) {
        self.range.extend_left(self.rope, count);
    }

    pub fn extend_right(&mut self, count: NonZeroUsize) {
        self.range.extend_right(self.rope, count);
    }

    pub fn flip(&mut self) {
        self.range.flip(self.rope);
    }

    pub fn flip_forward(&mut self) {
        self.range.flip_forward(self.rope);
    }

    pub fn flip_backward(&mut self) {
        self.range.flip_backward(self.rope);
    }

    pub fn reduce(&mut self) {
        self.range.reduce(self.rope);
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
    pub fn new(
        rope: &'a mut Rope,
        anchor_char_offset: usize,
        head_char_offset: usize,
    ) -> Option<Self> {
        let range = RawRange::new(rope, anchor_char_offset, head_char_offset)?;
        Some(Self { rope, range })
    }

    #[must_use]
    pub fn new_snapped(
        rope: &'a mut Rope,
        anchor_char_offset: usize,
        head_char_offset: usize,
    ) -> Self {
        let range = RawRange::new_snapped(rope, anchor_char_offset, head_char_offset);
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
    pub(crate) fn raw(&self) -> &RawRange {
        &self.range
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
    pub fn char_length(&self) -> usize {
        self.range.char_length()
    }

    #[must_use]
    pub fn grapheme_length(&self) -> usize {
        self.range.grapheme_length(self.rope)
    }

    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.range.is_empty()
    }

    #[must_use]
    pub fn is_eof(&self) -> bool {
        self.range.is_eof(self.rope)
    }

    #[must_use]
    pub fn is_forward(&self) -> bool {
        self.range.is_forward()
    }

    #[must_use]
    pub fn is_backward(&self) -> bool {
        self.range.is_backward()
    }

    pub fn extend_left(&mut self, count: NonZeroUsize) {
        self.range.extend_left(self.rope, count);
    }

    pub fn extend_right(&mut self, count: NonZeroUsize) {
        self.range.extend_right(self.rope, count);
    }

    pub fn flip(&mut self) {
        self.range.flip(self.rope);
    }

    pub fn flip_forward(&mut self) {
        self.range.flip_forward(self.rope);
    }

    pub fn flip_backward(&mut self) {
        self.range.flip_backward(self.rope);
    }

    pub fn reduce(&mut self) {
        self.range.reduce(self.rope);
    }

    pub fn insert_char(&mut self, char: char) {
        self.range.insert_char(self.rope, char);
    }

    pub fn insert(&mut self, string: &str) {
        self.range.insert(self.rope, string);
    }

    pub fn delete_before(&mut self, count: NonZeroUsize) {
        self.range.delete_before(self.rope, count);
    }

    pub fn delete(&mut self) {
        self.range.delete(self.rope);
    }

    pub fn delete_after(&mut self, count: NonZeroUsize) {
        self.range.delete_after(self.rope, count);
    }

    pub(crate) fn assert_valid(&self) {
        self.range.assert_valid(self.rope);
    }

    // TODO: Add `set_{head, anchor}`?
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
