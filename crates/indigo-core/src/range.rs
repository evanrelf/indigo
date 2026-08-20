use crate::{
    cursor::{Cursor, CursorMut, CursorSnapshot, CursorState},
    rope::RopeExt as _,
    text::Text,
};
use indigo_kernel::edit::Edit;
use indigo_wrap::{WBox, WMut, WRef, Wrap, WrapMut, WrapRef};
use ropey::{Rope, RopeSlice};
use std::{mem, thread};
use thiserror::Error;

#[cfg(feature = "arbitrary")]
use arbitrary::Arbitrary;

#[derive(Debug, Error)]
pub enum Error {
    #[error("Error from tail")]
    Tail(#[source] anyhow::Error),

    #[error("Error from head")]
    Head(#[source] anyhow::Error),
}

#[cfg_attr(feature = "arbitrary", derive(Arbitrary))]
#[derive(Debug)]
pub enum Action {
    UpdateGoalColumn,
    ExtendTo(usize),
    MoveTo(usize),
    ExtendLeft(u8),
    MoveLeft(u8),
    ExtendRight(u8),
    MoveRight(u8),
    ExtendUp(u8),
    MoveUp(u8),
    ExtendDown(u8),
    MoveDown(u8),
    ExtendUntilPrevByte { byte: u8, count: u8 },
    MoveUntilPrevByte { byte: u8, count: u8 },
    ExtendOntoPrevByte { byte: u8, count: u8 },
    MoveOntoPrevByte { byte: u8, count: u8 },
    ExtendUntilNextByte { byte: u8, count: u8 },
    MoveUntilNextByte { byte: u8, count: u8 },
    ExtendOntoNextByte { byte: u8, count: u8 },
    MoveOntoNextByte { byte: u8, count: u8 },
    ExtendToStart,
    MoveToStart,
    ExtendToEnd,
    MoveToEnd,
    ExtendToBottom,
    MoveToBottom,
    ExtendToLineStart,
    MoveToLineStart,
    ExtendToLineNonBlankStart,
    MoveToLineNonBlankStart,
    ExtendUntilLineEnd,
    MoveUntilLineEnd,
    ExtendOntoLineEnd,
    MoveOntoLineEnd,
    ExpandToFullLines,
    Flip,
    FlipForward,
    FlipBackward,
    Reduce,
    InsertChar(char),
    Insert(String),
    DeleteBefore,
    Delete,
    DeleteAfter,
}

#[derive(Clone, Default)]
pub struct RangeState {
    pub tail: CursorState,
    pub head: CursorState,
    pub goal_column: usize,
}

impl RangeState {
    pub fn transform(&mut self, ops: &Edit, text: &Rope) {
        self.tail.transform(ops, text);
        self.head.transform(ops, text);
    }

    #[must_use]
    pub fn start(&self) -> &CursorState {
        if self.tail.byte_index <= self.head.byte_index {
            &self.tail
        } else {
            &self.head
        }
    }

    #[must_use]
    pub fn end(&self) -> &CursorState {
        if self.tail.byte_index <= self.head.byte_index {
            &self.head
        } else {
            &self.tail
        }
    }

    /// A reduced range is considered forward.
    #[must_use]
    pub fn is_forward(&self) -> bool {
        self.tail.byte_index <= self.head.byte_index
    }

    #[must_use]
    pub fn with_bounds(&self, start: usize, end: usize) -> Self {
        if self.is_forward() {
            Self {
                tail: CursorState { byte_index: start },
                head: CursorState { byte_index: end },
                goal_column: self.goal_column,
            }
        } else {
            Self {
                tail: CursorState { byte_index: end },
                head: CursorState { byte_index: start },
                goal_column: self.goal_column,
            }
        }
    }

    #[must_use]
    pub fn save(&self, text: &Text) -> RangeSnapshot {
        let tail = self.tail.save(text);
        let head = self.head.save(text);
        RangeSnapshot {
            tail,
            head,
            goal_column: self.goal_column,
        }
    }
}

pub struct RangeSnapshot {
    pub tail: CursorSnapshot,
    pub head: CursorSnapshot,
    pub goal_column: usize,
}

impl RangeSnapshot {
    #[must_use]
    pub fn restore(&self, text: &Text) -> Option<RangeState> {
        let tail = self.tail.restore(text)?;
        let head = self.head.restore(text)?;
        Some(RangeState {
            tail,
            head,
            goal_column: self.goal_column,
        })
    }
}

#[must_use]
pub struct RangeView<'a, W: Wrap> {
    text: W::Wrap<'a, Text>,
    state: W::Wrap<'a, RangeState>,
    #[expect(clippy::type_complexity)]
    on_drop: Option<Box<dyn FnOnce(&mut Self) + 'a>>,
}

pub type Range<'a> = RangeView<'a, WRef>;

pub type RangeMut<'a> = RangeView<'a, WMut>;

impl<'a, W: Wrap> RangeView<'a, W> {
    pub fn on_drop(mut self, f: impl FnOnce(&mut Self) + 'a) -> Self {
        self.on_drop = Some(Box::new(f));
        self
    }
}

impl<'a, W: WrapRef> RangeView<'a, W> {
    pub fn new(
        text: W::WrapRef<'a, Text>,
        state: W::WrapRef<'a, RangeState>,
    ) -> anyhow::Result<Self> {
        let range_view = RangeView {
            text,
            state,
            on_drop: None,
        };
        range_view.assert_invariants()?;
        Ok(range_view)
    }

    pub fn text(&self) -> &Text {
        &self.text
    }

    pub fn state(&self) -> &RangeState {
        &self.state
    }

    #[must_use]
    pub fn byte_offsets(&self) -> (usize, usize) {
        let start = self.state.start().byte_index;
        let end = self
            .text
            .next_grapheme_boundary(self.state.end().byte_index)
            .expect("Range end is always on a grapheme");
        (start, end)
    }

    pub fn slice(&self) -> RopeSlice<'_> {
        let (start, end) = self.byte_offsets();
        self.text.slice(start..end)
    }

    pub fn tail(&self) -> Cursor<'_> {
        Cursor::new(&self.text, &self.state.tail)
            .expect("Range text and tail cursor state are always kept valid")
    }

    pub fn head(&self) -> Cursor<'_> {
        Cursor::new(&self.text, &self.state.head)
            .expect("Range text and head cursor state are always kept valid")
    }

    pub fn start(&self) -> Cursor<'_> {
        if self.is_forward() {
            self.tail()
        } else {
            self.head()
        }
    }

    pub fn end(&self) -> Cursor<'_> {
        if self.is_forward() {
            self.head()
        } else {
            self.tail()
        }
    }

    pub fn goal_column(&self) -> usize {
        self.state.goal_column
    }

    pub fn byte_length(&self) -> usize {
        let (start, end) = self.byte_offsets();
        end - start
    }

    pub fn grapheme_length(&self) -> usize {
        match self.byte_length() {
            0 => unreachable!(),
            1 => 1,
            _ => self.slice().graphemes().count(),
        }
    }

    /// A reduced range is considered forward.
    pub fn is_forward(&self) -> bool {
        self.state.is_forward()
    }

    pub fn is_backward(&self) -> bool {
        !self.is_forward()
    }

    pub fn is_touching<W2>(&self, other: &RangeView<'_, W2>) -> bool
    where
        W2: WrapRef,
    {
        let (self_start, self_end) = self.byte_offsets();
        let (other_start, other_end) = other.byte_offsets();
        self_end == other_start || other_end == self_start
    }

    pub fn is_overlapping<W2>(&self, other: &RangeView<'_, W2>) -> bool
    where
        W2: WrapRef,
    {
        self.state.start().byte_index <= other.state.end().byte_index
            && other.state.start().byte_index <= self.state.end().byte_index
    }

    #[must_use]
    pub fn save(&self) -> RangeSnapshot {
        self.state.save(&self.text)
    }

    pub fn assert_invariants(&self) -> anyhow::Result<()> {
        let _ = Cursor::new(&self.text, &self.state.tail).map_err(Error::Tail)?;
        let _ = Cursor::new(&self.text, &self.state.head).map_err(Error::Head)?;
        Ok(())
    }
}

impl<W: WrapMut> RangeView<'_, W> {
    fn tail_mut(&mut self) -> CursorMut<'_> {
        CursorMut::new(&mut self.text, &mut self.state.tail)
            .expect("Range text and tail cursor state are always kept valid")
            .on_drop(|cursor| cursor.assert_invariants().unwrap())
    }

    fn head_mut(&mut self) -> CursorMut<'_> {
        CursorMut::new(&mut self.text, &mut self.state.head)
            .expect("Range text and head cursor state are always kept valid")
            .on_drop(|cursor| cursor.assert_invariants().unwrap())
    }

    fn start_mut(&mut self) -> CursorMut<'_> {
        if self.is_forward() {
            self.tail_mut()
        } else {
            self.head_mut()
        }
    }

    fn end_mut(&mut self) -> CursorMut<'_> {
        if self.is_forward() {
            self.head_mut()
        } else {
            self.tail_mut()
        }
    }

    /// Should be called after performing any non-vertical movement.
    pub fn update_goal_column(&mut self) {
        let head_column = self.head().display_column();
        self.state.goal_column = head_column;
    }

    pub fn extend_to(&mut self, byte_index: usize) {
        self.head_mut().move_to(byte_index);
        self.update_goal_column();
    }

    pub fn move_to(&mut self, byte_index: usize) {
        self.extend_to(byte_index);
        self.reduce();
    }

    pub fn extend_left(&mut self, count: usize) {
        self.head_mut().move_left(count);
        self.update_goal_column();
    }

    pub fn move_left(&mut self, count: usize) {
        self.extend_left(count);
        self.reduce();
    }

    pub fn extend_right(&mut self, count: usize) {
        self.head_mut().move_right(count);
        self.update_goal_column();
    }

    pub fn move_right(&mut self, count: usize) {
        self.extend_right(count);
        self.reduce();
    }

    pub fn extend_up(&mut self, count: usize) {
        let goal_column = self.state.goal_column;
        self.head_mut().move_up(goal_column, count);
    }

    pub fn move_up(&mut self, count: usize) {
        self.extend_up(count);
        self.reduce();
    }

    pub fn extend_down(&mut self, count: usize) {
        let goal_column = self.state.goal_column;
        self.head_mut().move_down(goal_column, count);
    }

    pub fn move_down(&mut self, count: usize) {
        self.extend_down(count);
        self.reduce();
    }

    pub fn extend_until_prev_byte(&mut self, byte: u8, count: usize) {
        if self.head_mut().move_to_prev_byte(byte, count) {
            self.head_mut().move_right(1);
        }
        self.update_goal_column();
    }

    pub fn move_until_prev_byte(&mut self, byte: u8, count: usize) {
        self.reduce();
        self.extend_until_prev_byte(byte, count);
    }

    pub fn extend_onto_prev_byte(&mut self, byte: u8, count: usize) {
        self.head_mut().move_to_prev_byte(byte, count);
        self.update_goal_column();
    }

    pub fn move_onto_prev_byte(&mut self, byte: u8, count: usize) {
        self.reduce();
        self.extend_onto_prev_byte(byte, count);
    }

    pub fn extend_until_next_byte(&mut self, byte: u8, count: usize) {
        if self.head_mut().move_to_next_byte(byte, count) {
            self.head_mut().move_left(1);
        }
        self.update_goal_column();
    }

    pub fn move_until_next_byte(&mut self, byte: u8, count: usize) {
        self.reduce();
        self.extend_until_next_byte(byte, count);
    }

    pub fn extend_onto_next_byte(&mut self, byte: u8, count: usize) {
        self.head_mut().move_to_next_byte(byte, count);
        self.update_goal_column();
    }

    pub fn move_onto_next_byte(&mut self, byte: u8, count: usize) {
        self.reduce();
        self.extend_onto_next_byte(byte, count);
    }

    pub fn extend_to_start(&mut self) {
        self.head_mut().move_to_start();
        self.update_goal_column();
    }

    pub fn move_to_start(&mut self) {
        self.extend_to_start();
        self.reduce();
    }

    pub fn extend_to_end(&mut self) {
        self.head_mut().move_to_end();
        self.update_goal_column();
    }

    pub fn move_to_end(&mut self) {
        self.extend_to_end();
        self.reduce();
    }

    pub fn extend_to_bottom(&mut self) {
        self.head_mut().move_to_bottom();
        self.update_goal_column();
    }

    pub fn move_to_bottom(&mut self) {
        self.extend_to_bottom();
        self.reduce();
    }

    pub fn extend_to_line_start(&mut self) {
        self.head_mut().move_to_line_start();
        self.update_goal_column();
    }

    pub fn move_to_line_start(&mut self) {
        self.extend_to_line_start();
        self.reduce();
    }

    pub fn extend_to_line_non_blank_start(&mut self) {
        self.head_mut().move_to_line_non_blank_start();
        self.update_goal_column();
    }

    pub fn move_to_line_non_blank_start(&mut self) {
        self.extend_to_line_non_blank_start();
        self.reduce();
    }

    pub fn extend_until_line_end(&mut self) {
        self.head_mut().move_until_line_end();
        self.update_goal_column();
    }

    pub fn move_until_line_end(&mut self) {
        self.extend_until_line_end();
        self.reduce();
    }

    pub fn extend_onto_line_end(&mut self) {
        self.head_mut().move_to_line_end();
        self.update_goal_column();
    }

    pub fn move_onto_line_end(&mut self) {
        self.extend_onto_line_end();
        self.reduce();
    }

    pub fn expand_to_full_lines(&mut self) {
        self.start_mut().move_to_line_start();
        self.end_mut().move_to_line_end();
        self.update_goal_column();
    }

    pub fn flip(&mut self) {
        fn both(state: &mut RangeState) -> (&mut CursorState, &mut CursorState) {
            (&mut state.tail, &mut state.head)
        }
        let (tail, head) = both(&mut self.state);
        mem::swap(tail, head);
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
        self.state.tail = self.state.head.clone();
    }

    /// Reshape for appending (Kakoune's `a`): reduce to the head, then move onto the following
    /// grapheme so insertion lands after the original head. On the text's last grapheme there is
    /// no following grapheme, so a newline is appended first (as Kakoune does) and the cursor
    /// lands on it.
    pub fn prepare_append(&mut self) -> Option<Edit> {
        self.reduce();
        if self.head().is_at_end() {
            let mut ops = Edit::new();
            ops.retain(self.text.len());
            ops.insert("\n");
            self.text.apply(&ops).expect("Operations are well formed");
            self.state.transform(&ops, &self.text);
            let last = self
                .text
                .last_grapheme_start()
                .expect("Text is never empty");
            self.state.tail.byte_index = last;
            self.state.head.byte_index = last;
            self.update_goal_column();
            Some(ops)
        } else {
            self.head_mut().move_right(1);
            self.reduce();
            self.update_goal_column();
            None
        }
    }

    pub fn insert_char(&mut self, char: char) -> Edit {
        self.insert(&char.to_string())
    }

    /// Insert before the range's first grapheme. Both endpoints stay on their graphemes (i.e.
    /// end up after the inserted text).
    #[tracing::instrument(skip_all)]
    pub fn insert(&mut self, text: &str) -> Edit {
        debug_assert!(
            self.grapheme_length() <= 1,
            "Range reduced before entering insert mode"
        );
        let mut ops = Edit::new();
        ops.retain(self.state.start().byte_index);
        ops.insert(text);
        ops.retain_rest(&self.text)
            .expect("Operations fit within text");
        self.text.apply(&ops).expect("Operations are well formed");
        self.state.transform(&ops, &self.text);
        self.update_goal_column();
        ops
    }

    /// Delete the grapheme before the range's start.
    #[tracing::instrument(skip_all)]
    pub fn delete_before(&mut self) -> Option<Edit> {
        debug_assert!(
            self.grapheme_length() <= 1,
            "Range reduced before entering insert mode"
        );
        let start = self.state.start().byte_index;
        let delete_start = self.text.prev_grapheme_boundary(start)?;
        let mut ops = Edit::new();
        ops.retain(delete_start);
        ops.delete(&self.text.slice(delete_start..start).to_string());
        ops.retain_rest(&self.text)
            .expect("Operations fit within text");
        self.text.apply(&ops).expect("Operations are well formed");
        self.state.transform(&ops, &self.text);
        self.update_goal_column();
        Some(ops)
    }

    /// Delete the selected graphemes. Deleting through the end of the text re-inserts the
    /// invariant trailing newline.
    #[tracing::instrument(skip_all)]
    pub fn delete(&mut self) -> Edit {
        let (start, end) = self.byte_offsets();
        let mut ops = Edit::new();
        ops.retain(start);
        ops.delete(&self.text.slice(start..end).to_string());
        if end == self.text.len() && (start == 0 || self.text.byte(start - 1) != b'\n') {
            ops.insert("\n");
        }
        ops.retain_rest(&self.text)
            .expect("Operations fit within text");
        self.text.apply(&ops).expect("Operations are well formed");
        self.state.transform(&ops, &self.text);
        self.update_goal_column();
        ops
    }

    /// Delete the grapheme under the range's end cursor, unless it is the text's final newline.
    #[tracing::instrument(skip_all)]
    pub fn delete_after(&mut self) -> Option<Edit> {
        debug_assert!(
            self.grapheme_length() <= 1,
            "Range reduced before entering insert mode"
        );
        let end = self.state.end().byte_index;
        let delete_end = self
            .text
            .next_grapheme_boundary(end)
            .expect("Range end is always on a grapheme");
        if delete_end == self.text.len() {
            // Deleting the final newline would break the `Text` invariant.
            return None;
        }
        let mut ops = Edit::new();
        ops.retain(end);
        ops.delete(&self.text.slice(end..delete_end).to_string());
        ops.retain_rest(&self.text)
            .expect("Operations fit within text");
        self.text.apply(&ops).expect("Operations are well formed");
        self.state.transform(&ops, &self.text);
        self.update_goal_column();
        Some(ops)
    }

    pub fn restore(&mut self, snapshot: &RangeSnapshot) -> bool {
        if let Some(state) = snapshot.restore(&self.text) {
            *self.state = state;
            true
        } else {
            false
        }
    }
}

#[expect(clippy::too_many_lines)]
pub fn handle_action<W: WrapMut>(range: &mut RangeView<'_, W>, action: &Action) {
    match action {
        Action::UpdateGoalColumn => {
            range.update_goal_column();
        }
        Action::ExtendTo(byte_index) => {
            range.extend_to(*byte_index);
        }
        Action::MoveTo(byte_index) => {
            range.move_to(*byte_index);
        }
        Action::ExtendLeft(count) => {
            range.extend_left(usize::from(*count));
        }
        Action::MoveLeft(count) => {
            range.move_left(usize::from(*count));
        }
        Action::ExtendRight(count) => {
            range.extend_right(usize::from(*count));
        }
        Action::MoveRight(count) => {
            range.move_right(usize::from(*count));
        }
        Action::ExtendUp(count) => {
            range.extend_up(usize::from(*count));
        }
        Action::MoveUp(count) => {
            range.move_up(usize::from(*count));
        }
        Action::ExtendDown(count) => {
            range.extend_down(usize::from(*count));
        }
        Action::MoveDown(count) => {
            range.move_down(usize::from(*count));
        }
        Action::ExtendUntilPrevByte { byte, count } => {
            range.extend_until_prev_byte(*byte, usize::from(*count));
        }
        Action::MoveUntilPrevByte { byte, count } => {
            range.move_until_prev_byte(*byte, usize::from(*count));
        }
        Action::ExtendOntoPrevByte { byte, count } => {
            range.extend_onto_prev_byte(*byte, usize::from(*count));
        }
        Action::MoveOntoPrevByte { byte, count } => {
            range.move_onto_prev_byte(*byte, usize::from(*count));
        }
        Action::ExtendUntilNextByte { byte, count } => {
            range.extend_until_next_byte(*byte, usize::from(*count));
        }
        Action::MoveUntilNextByte { byte, count } => {
            range.move_until_next_byte(*byte, usize::from(*count));
        }
        Action::ExtendOntoNextByte { byte, count } => {
            range.extend_onto_next_byte(*byte, usize::from(*count));
        }
        Action::MoveOntoNextByte { byte, count } => {
            range.move_onto_next_byte(*byte, usize::from(*count));
        }
        Action::ExtendToStart => {
            range.extend_to_start();
        }
        Action::MoveToStart => {
            range.move_to_start();
        }
        Action::ExtendToEnd => {
            range.extend_to_end();
        }
        Action::MoveToEnd => {
            range.move_to_end();
        }
        Action::ExtendToBottom => {
            range.extend_to_bottom();
        }
        Action::MoveToBottom => {
            range.move_to_bottom();
        }
        Action::ExtendToLineStart => {
            range.extend_to_line_start();
        }
        Action::MoveToLineStart => {
            range.move_to_line_start();
        }
        Action::ExtendToLineNonBlankStart => {
            range.extend_to_line_non_blank_start();
        }
        Action::MoveToLineNonBlankStart => {
            range.move_to_line_non_blank_start();
        }
        Action::ExtendUntilLineEnd => {
            range.extend_until_line_end();
        }
        Action::MoveUntilLineEnd => {
            range.move_until_line_end();
        }
        Action::ExtendOntoLineEnd => {
            range.extend_onto_line_end();
        }
        Action::MoveOntoLineEnd => {
            range.move_onto_line_end();
        }
        Action::ExpandToFullLines => {
            range.expand_to_full_lines();
        }
        Action::Flip => {
            range.flip();
        }
        Action::FlipForward => {
            range.flip_forward();
        }
        Action::FlipBackward => {
            range.flip_backward();
        }
        Action::Reduce => {
            range.reduce();
        }
        Action::InsertChar(c) => {
            range.reduce();
            range.insert_char(*c);
        }
        Action::Insert(text) => {
            range.reduce();
            range.insert(text);
        }
        Action::DeleteBefore => {
            range.reduce();
            range.delete_before();
        }
        Action::Delete => {
            range.delete();
        }
        Action::DeleteAfter => {
            range.reduce();
            range.delete_after();
        }
    }
}

impl<R> TryFrom<(R, usize, usize)> for RangeView<'_, WBox>
where
    R: Into<Text>,
{
    type Error = anyhow::Error;
    fn try_from((text, tail, head): (R, usize, usize)) -> anyhow::Result<Self> {
        let text = Box::new(text.into());
        let state = Box::new(RangeState {
            tail: CursorState { byte_index: tail },
            head: CursorState { byte_index: head },
            goal_column: 0,
        });
        Self::new(text, state).map(|mut range| {
            range.update_goal_column();
            range
        })
    }
}

impl<W: Wrap> Drop for RangeView<'_, W> {
    fn drop(&mut self) {
        if !thread::panicking()
            && let Some(f) = self.on_drop.take()
        {
            f(self);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn insert_changes_grapheme_boundary() {
        // combining acute accent (´)
        let mut text = Text::from("\u{0301}");
        let mut state = RangeState::default();
        let mut range = RangeMut::new(&mut text, &mut state).unwrap();
        range.insert("e");
        range.assert_invariants().unwrap();
    }

    #[test]
    fn invalid_range_constructor_returns_error() {
        let range = RangeView::try_from(("~", usize::MAX, 0));
        assert!(range.is_err());
    }

    #[test]
    fn one_grapheme_range_is_minimum() {
        let range = RangeView::try_from(("xy\n", 1, 1)).unwrap();
        assert_eq!(range.grapheme_length(), 1);
        assert_eq!(range.byte_length(), 1);
        assert_eq!(&range.slice().to_string(), "y");
        assert!(range.is_forward());
    }

    #[test]
    fn flip_is_direction_swap() {
        let mut range = RangeView::try_from(("abc\n", 0, 2)).unwrap();
        assert!(range.is_forward());
        range.flip();
        assert!(range.is_backward());
        assert_eq!(range.tail().byte_index(), 2);
        assert_eq!(range.head().byte_index(), 0);
        range.reduce();
        assert!(range.is_forward());
    }

    #[test]
    fn delete_one_grapheme_range_deletes_a_character() {
        let mut range = RangeView::try_from(("abc\n", 1, 1)).unwrap();
        range.delete();
        assert_eq!(&range.text().to_string(), "ac\n");
        assert_eq!(range.head().byte_index(), 1);
    }

    #[test]
    fn delete_through_end_keeps_trailing_newline() {
        // Select "bc\n" (inclusive of the final newline) and delete.
        let mut range = RangeView::try_from(("abc\n", 1, 3)).unwrap();
        range.delete();
        assert_eq!(&range.text().to_string(), "a\n");
        range.assert_invariants().unwrap();

        // Deleting everything leaves the empty text "\n".
        let mut range = RangeView::try_from(("abc\n", 0, 3)).unwrap();
        range.delete();
        assert_eq!(&range.text().to_string(), "\n");
        assert_eq!(range.head().byte_index(), 0);
    }

    #[test]
    fn prepare_append_at_end_materializes_newline() {
        // Cursor on the final newline: Kakoune appends a real newline so the insertion point
        // exists, and the cursor lands on it.
        let mut range = RangeView::try_from(("ab\n", 2, 2)).unwrap();
        range.prepare_append();
        assert_eq!(&range.text().to_string(), "ab\n\n");
        assert_eq!(range.head().byte_index(), 3);
        range.insert("x");
        assert_eq!(&range.text().to_string(), "ab\nx\n");

        // Mid-text `a` just moves onto the next grapheme.
        let mut range = RangeView::try_from(("ab\n", 0, 0)).unwrap();
        range.prepare_append();
        assert_eq!(&range.text().to_string(), "ab\n");
        assert_eq!(range.head().byte_index(), 1);
    }

    #[test]
    fn move_to_line_start_from_newline() {
        let mut text = Text::from("hello world\n");
        let mut state = RangeState::default();
        let mut range = RangeMut::new(&mut text, &mut state).unwrap();
        range.move_until_line_end();
        range.move_right(1);
        assert_eq!(&range.slice().to_string(), "\n");
        range.move_to_line_start();
        assert_eq!(&range.slice().to_string(), "h");
    }

    #[test]
    fn move_to_line_start_idempotent() {
        let mut text = Text::from("");
        let mut state = RangeState::default();
        let mut range = RangeMut::new(&mut text, &mut state).unwrap();
        range.insert("x\ny");

        range.move_to_line_start();
        let first_tail_index = range.tail().byte_index();
        let first_head_index = range.head().byte_index();
        assert_eq!(&range.slice().to_string(), "y");

        range.move_to_line_start();
        let second_tail_index = range.tail().byte_index();
        let second_head_index = range.head().byte_index();
        assert_eq!(first_tail_index, second_tail_index);
        assert_eq!(first_head_index, second_head_index);
        assert_eq!(&range.slice().to_string(), "y");
    }

    #[test]
    fn move_to_line_non_blank_start_idempotent() {
        let mut text = Text::from("");
        let mut state = RangeState::default();
        let mut range = RangeMut::new(&mut text, &mut state).unwrap();
        range.insert(" x");

        range.move_to_line_non_blank_start();
        let first_tail_index = range.tail().byte_index();
        let first_head_index = range.head().byte_index();
        assert_eq!(&range.slice().to_string(), "x");

        range.move_to_line_non_blank_start();
        let second_tail_index = range.tail().byte_index();
        let second_head_index = range.head().byte_index();
        assert_eq!(first_tail_index, second_tail_index);
        assert_eq!(first_head_index, second_head_index);
        assert_eq!(&range.slice().to_string(), "x");
    }

    #[test]
    fn move_to_line_non_blank_start_symmetric() {
        let mut text = Text::from("");
        let mut state = RangeState::default();
        let mut range = RangeMut::new(&mut text, &mut state).unwrap();
        range.insert("    foo");

        range.move_to_line_start();
        range.move_to_line_non_blank_start();
        let from_start = range.head().byte_index();

        range.move_until_line_end();
        range.move_to_line_non_blank_start();
        let from_end = range.head().byte_index();

        assert_eq!(from_start, from_end);
    }

    #[hegel::test(test_cases = 1000)]
    fn fuzz(tc: hegel::TestCase) {
        use hegel::{TestCase, generators as gs};

        struct StateMachine {
            text: Text,
            state: RangeState,
        }
        #[hegel::state_machine]
        #[expect(clippy::needless_pass_by_value)]
        impl StateMachine {
            fn range(&mut self) -> RangeMut<'_> {
                RangeMut::new(&mut self.text, &mut self.state).expect("Range state kept valid")
            }
            fn count(tc: &TestCase) -> usize {
                tc.draw(gs::integers::<usize>().min_value(1).max_value(100))
            }
            #[rule]
            fn extend_to(&mut self, tc: TestCase) {
                let byte_index = tc.draw(gs::integers::<usize>().max_value(self.text.len()));
                self.range().extend_to(byte_index);
            }
            #[rule]
            fn move_to(&mut self, tc: TestCase) {
                let byte_index = tc.draw(gs::integers::<usize>().max_value(self.text.len()));
                self.range().move_to(byte_index);
            }
            #[rule]
            fn extend_left(&mut self, tc: TestCase) {
                let count = Self::count(&tc);
                self.range().extend_left(count);
            }
            #[rule]
            fn move_left(&mut self, tc: TestCase) {
                let count = Self::count(&tc);
                self.range().move_left(count);
            }
            #[rule]
            fn extend_right(&mut self, tc: TestCase) {
                let count = Self::count(&tc);
                self.range().extend_right(count);
            }
            #[rule]
            fn move_right(&mut self, tc: TestCase) {
                let count = Self::count(&tc);
                self.range().move_right(count);
            }
            #[rule]
            fn extend_up(&mut self, tc: TestCase) {
                let count = Self::count(&tc);
                self.range().extend_up(count);
            }
            #[rule]
            fn move_down(&mut self, tc: TestCase) {
                let count = Self::count(&tc);
                self.range().move_down(count);
            }
            #[rule]
            fn seek(&mut self, tc: TestCase) {
                let byte = tc.draw(gs::integers::<u8>());
                let count = Self::count(&tc);
                let mut range = self.range();
                match tc.draw(gs::integers::<u8>().max_value(7)) {
                    0 => range.extend_until_prev_byte(byte, count),
                    1 => range.move_until_prev_byte(byte, count),
                    2 => range.extend_onto_prev_byte(byte, count),
                    3 => range.move_onto_prev_byte(byte, count),
                    4 => range.extend_until_next_byte(byte, count),
                    5 => range.move_until_next_byte(byte, count),
                    6 => range.extend_onto_next_byte(byte, count),
                    _ => range.move_onto_next_byte(byte, count),
                }
            }
            #[rule]
            fn line_ops(&mut self, tc: TestCase) {
                let mut range = self.range();
                match tc.draw(gs::integers::<u8>().max_value(7)) {
                    0 => range.extend_to_line_start(),
                    1 => range.move_to_line_start(),
                    2 => range.extend_to_line_non_blank_start(),
                    3 => range.move_to_line_non_blank_start(),
                    4 => range.extend_until_line_end(),
                    5 => range.move_until_line_end(),
                    6 => range.extend_onto_line_end(),
                    _ => range.move_onto_line_end(),
                }
            }
            #[rule]
            fn extremes(&mut self, tc: TestCase) {
                let mut range = self.range();
                match tc.draw(gs::integers::<u8>().max_value(5)) {
                    0 => range.extend_to_start(),
                    1 => range.move_to_start(),
                    2 => range.extend_to_end(),
                    3 => range.move_to_end(),
                    4 => range.extend_to_bottom(),
                    _ => range.move_to_bottom(),
                }
            }
            #[rule]
            fn expand_to_full_lines(&mut self, _: TestCase) {
                self.range().expand_to_full_lines();
            }
            #[rule]
            fn flip(&mut self, tc: TestCase) {
                let mut range = self.range();
                match tc.draw(gs::integers::<u8>().max_value(2)) {
                    0 => range.flip(),
                    1 => range.flip_forward(),
                    _ => range.flip_backward(),
                }
            }
            #[rule]
            fn reduce(&mut self, _: TestCase) {
                self.range().reduce();
            }
            #[rule]
            fn prepare_append(&mut self, _: TestCase) {
                self.range().prepare_append();
            }
            #[rule]
            fn insert(&mut self, tc: TestCase) {
                let string = tc.draw(gs::text());
                let mut range = self.range();
                range.reduce();
                range.insert(&string);
            }
            #[rule]
            fn delete_before(&mut self, _: TestCase) {
                let mut range = self.range();
                range.reduce();
                range.delete_before();
            }
            #[rule]
            fn delete(&mut self, _: TestCase) {
                self.range().delete();
            }
            #[rule]
            fn delete_after(&mut self, _: TestCase) {
                let mut range = self.range();
                range.reduce();
                range.delete_after();
            }
            #[rule]
            fn save_restore_roundtrip(&mut self, tc: TestCase) {
                let string = tc.draw(gs::text());
                let mut range = self.range();
                let snapshot = range.save();
                range.reduce();
                range.insert(&string);
                assert!(range.restore(&snapshot));
            }
            #[invariant]
            fn invariants(&self, _: TestCase) {
                let range =
                    Range::new(&self.text, &self.state).expect("Range valid after every operation");
                assert!(range.grapheme_length() >= 1, "Range is never empty");
                assert_eq!(range.byte_length(), range.slice().len());
                assert!(range.state().start().byte_index <= range.state().end().byte_index);
                let rope = self.text.rope();
                assert!(
                    rope.len() > 0 && rope.byte(rope.len() - 1) == b'\n',
                    "Text keeps its trailing newline"
                );
            }
        }
        let machine = StateMachine {
            text: Text::new(),
            state: RangeState::default(),
        };
        hegel::stateful::run(machine, tc);
    }
}
