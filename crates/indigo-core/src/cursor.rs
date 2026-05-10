use crate::{
    ot::OperationSeq,
    rope::{DisplayWidth as _, LINE_TYPE, RopeExt as _},
    text::{Anchor, Text},
};
use indigo_wrap::{WBox, WMut, WRef, Wrap, WrapMut, WrapRef};
use ropey::{Rope, RopeSlice};
use std::thread;
use thiserror::Error;

#[cfg(any(feature = "arbitrary", test))]
use arbitrary::Arbitrary;

#[derive(Debug, Error)]
pub enum Error {
    #[error("Byte offset {byte_offset} exceeds end of text at {end_offset}")]
    ExceedsEnd {
        byte_offset: usize,
        end_offset: usize,
    },

    #[error("Byte offset {byte_offset} does not lie on grapheme boundary")]
    NotOnGraphemeBoundary { byte_offset: usize },
}

#[cfg_attr(any(feature = "arbitrary", test), derive(Arbitrary))]
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Bias {
    Before,
    After,
}

#[cfg_attr(any(feature = "arbitrary", test), derive(Arbitrary))]
#[derive(Debug)]
pub enum Action {
    SnapToGraphemeBoundary,
    UncheckedMoveTo(usize),
    MoveLeft(u8),
    MoveRight(u8),
    MoveUp {
        goal_column: u16,
        bias: Bias,
        count: u8,
    },
    MoveDown {
        goal_column: u16,
        bias: Bias,
        count: u8,
    },
    MoveToPrevByte {
        byte: u8,
        count: u8,
    },
    MoveToNextByte {
        byte: u8,
        count: u8,
    },
    MoveToPrevBlank(u8),
    MoveToNextBlank(u8),
    MoveToStart,
    MoveToEnd,
    MoveToBottom(Bias),
    MoveToLineStart(Bias),
    MoveToLineNonBlankStart(Bias),
    MoveToLineEnd(Bias),
    InsertChar(char),
    Insert(String),
    DeleteBefore,
    DeleteAfter,
}

#[derive(Clone, Copy)]
pub enum Direction {
    Backward,
    Forward,
}

#[derive(Clone, Debug, Default)]
pub struct CursorState {
    pub byte_offset: usize,
}

impl CursorState {
    /// Snap to nearest grapheme boundary. This is a no-op if the cursor is already valid.
    pub fn snap_to_grapheme_boundary(&mut self, text: &Rope) -> bool {
        let prev_byte_offset = self.byte_offset;
        self.byte_offset = text.ceil_grapheme_boundary(self.byte_offset);
        self.byte_offset != prev_byte_offset
    }

    #[must_use]
    pub fn snapped_to_grapheme_boundary(mut self, text: &Rope) -> Self {
        self.snap_to_grapheme_boundary(text);
        self
    }

    pub fn transform(&mut self, ops: &OperationSeq) {
        self.byte_offset = ops.transform_byte_offset(self.byte_offset);
    }

    #[must_use]
    pub fn save(&self, text: &Text) -> CursorSnapshot {
        let byte_offset = text.create_anchor(self.byte_offset);
        CursorSnapshot { byte_offset }
    }
}

pub struct CursorSnapshot {
    pub byte_offset: Anchor,
}

impl CursorSnapshot {
    #[must_use]
    pub fn restore(&self, text: &Text) -> Option<CursorState> {
        let byte_offset = text.ceil_grapheme_boundary(text.resolve_anchor(&self.byte_offset)?);
        Some(CursorState { byte_offset })
    }
}

#[must_use]
pub struct CursorView<'a, W: Wrap> {
    text: W::Wrap<'a, Text>,
    state: W::Wrap<'a, CursorState>,
    #[expect(clippy::type_complexity)]
    on_drop: Option<Box<dyn FnOnce(&mut Self) + 'a>>,
}

pub type Cursor<'a> = CursorView<'a, WRef>;

pub type CursorMut<'a> = CursorView<'a, WMut>;

impl<'a, W: Wrap> CursorView<'a, W> {
    pub fn on_drop(mut self, f: impl FnOnce(&mut Self) + 'a) -> Self {
        self.on_drop = Some(Box::new(f));
        self
    }
}

impl<'a, W: WrapRef> CursorView<'a, W> {
    pub fn new(
        text: W::WrapRef<'a, Text>,
        state: W::WrapRef<'a, CursorState>,
    ) -> anyhow::Result<Self> {
        let cursor_view = CursorView {
            text,
            state,
            on_drop: None,
        };
        cursor_view.assert_invariants()?;
        Ok(cursor_view)
    }

    pub fn text(&self) -> &Text {
        &self.text
    }

    pub fn state(&self) -> &CursorState {
        &self.state
    }

    #[must_use]
    pub fn byte_offset(&self) -> usize {
        self.state.byte_offset
    }

    pub fn byte_index(&self, bias: Bias) -> Result<usize, Option<usize>> {
        let prev = || {
            self.text
                .prev_grapheme_boundary(self.state.byte_offset)
                .expect("Not at start so previous grapheme boundary exists")
        };
        match bias {
            _ if self.text.len() == 0 => Err(None),
            Bias::Before if self.is_at_start() => Err(Some(self.state.byte_offset)),
            Bias::After if self.is_at_end() => Err(Some(prev())),
            Bias::Before => Ok(prev()),
            Bias::After => Ok(self.state.byte_offset),
        }
    }

    pub fn line_index(&self, bias: Bias) -> Option<usize> {
        let byte_index = self.byte_index(bias).ok()?;
        let line_index = self.text.byte_to_line_idx(byte_index, LINE_TYPE);
        Some(line_index)
    }

    #[must_use]
    pub fn display_column(&self, bias: Bias) -> Option<usize> {
        let byte_index = self.byte_index(bias).ok()?;
        Some(self.text.display_column(byte_index))
    }

    #[must_use]
    pub fn line_number(&self, bias: Bias) -> usize {
        self.line_index(bias).map_or(1, |n| n + 1)
    }

    #[must_use]
    pub fn column_number(&self, bias: Bias) -> usize {
        let Ok(byte_index) = self.byte_index(bias) else {
            return 1;
        };
        let line_index = self.text.byte_to_line_idx(byte_index, LINE_TYPE);
        let line_byte_index = self.text.line_to_byte_idx(line_index, LINE_TYPE);
        (byte_index - line_byte_index) + 1
    }

    #[must_use]
    pub fn grapheme(&self, bias: Bias) -> Option<RopeSlice<'_>> {
        let byte_index = self.byte_index(bias).ok()?;
        let start = byte_index;
        let end = self.text.next_grapheme_boundary(start)?;
        Some(self.text.slice(start..end))
    }

    #[must_use]
    pub fn line(&self, bias: Bias) -> Option<RopeSlice<'_>> {
        let line_index = self.line_index(bias)?;
        self.text.get_line(line_index, LINE_TYPE)
    }

    #[must_use]
    pub fn is_at_start(&self) -> bool {
        self.state.byte_offset == 0
    }

    #[must_use]
    pub fn is_at_end(&self) -> bool {
        self.state.byte_offset == self.text.len()
    }

    #[must_use]
    pub fn save(&self) -> CursorSnapshot {
        self.state.save(&self.text)
    }

    pub fn assert_invariants(&self) -> anyhow::Result<()> {
        if self.state.byte_offset > self.text.len() {
            anyhow::bail!(Error::ExceedsEnd {
                byte_offset: self.state.byte_offset,
                end_offset: self.text.len(),
            });
        }
        if !self.text.is_grapheme_boundary(self.state.byte_offset) {
            anyhow::bail!(Error::NotOnGraphemeBoundary {
                byte_offset: self.state.byte_offset,
            });
        }
        Ok(())
    }
}

impl<W: WrapMut> CursorView<'_, W> {
    pub fn snap_to_grapheme_boundary(&mut self) -> bool {
        self.state.snap_to_grapheme_boundary(&self.text)
    }

    // TODO: Drop `unchecked_` prefix? Or change to something else? Since there are assertions.
    pub fn unchecked_move_to(&mut self, byte_offset: usize) {
        debug_assert!(byte_offset <= self.text.len());
        debug_assert!(self.text.is_grapheme_boundary(byte_offset));
        self.state.byte_offset = byte_offset;
    }

    pub fn move_left(&mut self, count: usize) -> bool {
        if count == 0 {
            return false;
        }
        let pre = self.state.byte_offset;
        for _ in 0..count {
            if let Some(prev) = self.text.prev_grapheme_boundary(self.state.byte_offset)
                && self.state.byte_offset != prev
            {
                self.state.byte_offset = prev;
            } else {
                debug_assert!(self.state.byte_offset <= pre);
                return false;
            }
        }
        debug_assert!(self.state.byte_offset < pre);
        true
    }

    pub fn move_right(&mut self, count: usize) -> bool {
        if count == 0 {
            return false;
        }
        let pre = self.state.byte_offset;
        for _ in 0..count {
            if let Some(next) = self.text.next_grapheme_boundary(self.state.byte_offset)
                && self.state.byte_offset != next
            {
                self.state.byte_offset = next;
            } else {
                debug_assert!(self.state.byte_offset >= pre);
                return false;
            }
        }
        debug_assert!(self.state.byte_offset > pre);
        true
    }

    pub fn move_up(&mut self, goal_column: usize, bias: Bias, count: usize) -> bool {
        self.move_vertical(Direction::Backward, goal_column, bias, count)
    }

    pub fn move_down(&mut self, goal_column: usize, bias: Bias, count: usize) -> bool {
        self.move_vertical(Direction::Forward, goal_column, bias, count)
    }

    fn move_vertical(
        &mut self,
        direction: Direction,
        goal_column: usize,
        bias: Bias,
        count: usize,
    ) -> bool {
        if count == 0 || self.text.len() == 0 {
            return false;
        }
        for _ in 0..count {
            #[expect(clippy::manual_let_else)]
            let current_byte_index = match self.byte_index(bias) {
                Ok(n) | Err(Some(n)) => n,
                Err(None) => unreachable!("Already checked rope length"),
            };
            let current_line_index = self.text.byte_to_line_idx(current_byte_index, LINE_TYPE);
            let target_line_index = match direction {
                Direction::Backward => {
                    if current_line_index == 0 {
                        return false;
                    }
                    current_line_index - 1
                }
                Direction::Forward => {
                    if self.state.byte_offset == self.text.len() {
                        return false;
                    }
                    let target = current_line_index + 1;
                    if target >= self.text.len_lines_indigo() {
                        return false;
                    }
                    target
                }
            };
            let target_line_byte_index = self.text.line_to_byte_idx(target_line_index, LINE_TYPE);
            let target_line_slice = self.text.line(target_line_index, LINE_TYPE);
            let mut target_line_prefix = 0;
            let mut target_byte_index = target_line_byte_index;
            for grapheme in target_line_slice.graphemes() {
                if grapheme.chars().any(|c| c == '\n' || c == '\r') {
                    break;
                }
                let grapheme_width = grapheme.display_width();
                if target_line_prefix + grapheme_width > goal_column {
                    break;
                }
                target_line_prefix += grapheme_width;
                target_byte_index += grapheme.len();
            }
            self.state.byte_offset = match bias {
                Bias::After => target_byte_index,
                Bias::Before => self
                    .text
                    .next_grapheme_boundary(target_byte_index)
                    .unwrap_or(target_byte_index),
            };
        }
        true
    }

    pub fn move_to_prev_byte(&mut self, byte: u8, count: usize) -> bool {
        if count == 0 {
            return false;
        }
        let pre = self.state.byte_offset;
        for _ in 0..count {
            if let Some(byte_offset) = self.text.find_prev_byte(..self.state.byte_offset, &[byte]) {
                self.state.byte_offset = byte_offset;
                self.move_right(1);
            } else {
                debug_assert!(self.state.byte_offset <= pre);
                return false;
            }
        }
        debug_assert!(self.state.byte_offset <= pre);
        true
    }

    pub fn move_to_next_byte(&mut self, byte: u8, count: usize) -> bool {
        if count == 0 {
            return false;
        }
        let pre = self.state.byte_offset;
        let mut start = self.state.byte_offset;
        for _ in 0..count {
            if let Some(byte_offset) = self.text.find_next_byte(start.., &[byte]) {
                self.state.byte_offset = self.text.ceil_grapheme_boundary(byte_offset);
                start = self.text.ceil_grapheme_boundary(byte_offset + 1);
            } else {
                debug_assert!(self.state.byte_offset >= pre);
                return false;
            }
        }
        debug_assert!(self.state.byte_offset >= pre);
        true
    }

    pub fn move_to_prev_blank(&mut self, count: usize) -> bool {
        const BYTES: &[u8] = b" \t\n\r";
        if count == 0 {
            return false;
        }
        let pre = self.state.byte_offset;
        for _ in 0..count {
            if let Some(byte_offset) = self.text.find_prev_byte(..self.state.byte_offset, BYTES) {
                self.state.byte_offset = byte_offset;
                self.move_right(1);
            } else {
                debug_assert!(self.state.byte_offset <= pre);
                return false;
            }
        }
        debug_assert!(self.state.byte_offset <= pre);
        true
    }

    pub fn move_to_next_blank(&mut self, count: usize) -> bool {
        const BYTES: &[u8] = b" \t\n\r";
        if count == 0 {
            return false;
        }
        let pre = self.state.byte_offset;
        let mut start = self.state.byte_offset;
        for _ in 0..count {
            if let Some(byte_offset) = self.text.find_next_byte(start.., BYTES) {
                self.state.byte_offset = self.text.ceil_grapheme_boundary(byte_offset);
                start = self.text.ceil_grapheme_boundary(byte_offset + 1);
            } else {
                debug_assert!(self.state.byte_offset >= pre);
                return false;
            }
        }
        debug_assert!(self.state.byte_offset >= pre);
        true
    }

    pub fn move_to_start(&mut self) {
        self.state.byte_offset = 0;
    }

    pub fn move_to_end(&mut self) {
        self.state.byte_offset = self.text.len();
    }

    pub fn move_to_bottom(&mut self, bias: Bias) {
        self.move_to_end();
        self.move_to_line_start(bias);
    }

    pub fn move_to_line_start(&mut self, bias: Bias) {
        if self.text.len() == 0 {
            return;
        }

        let pre = self.state.byte_offset;

        #[expect(clippy::manual_let_else)]
        let byte_index = match self.byte_index(bias) {
            Ok(n) | Err(Some(n)) => n,
            Err(None) => unreachable!("Already checked rope length"),
        };

        let line_index = self.text.byte_to_line_idx(byte_index, LINE_TYPE);
        let line_start_byte_offset = self.text.line_to_byte_idx(line_index, LINE_TYPE);
        self.state.byte_offset = line_start_byte_offset;
        debug_assert!(self.state.byte_offset <= pre);
    }

    pub fn move_to_line_non_blank_start(&mut self, bias: Bias) {
        if self.text.len() == 0 {
            return;
        }

        #[expect(clippy::manual_let_else)]
        let byte_index = match self.byte_index(bias) {
            Ok(n) | Err(Some(n)) => n,
            Err(None) => unreachable!("Already checked rope length"),
        };

        let line_index = self.text.byte_to_line_idx(byte_index, LINE_TYPE);
        let line_start_byte_offset = self.text.line_to_byte_idx(line_index, LINE_TYPE);
        let line_slice = self.text.line(line_index, LINE_TYPE);
        let mut byte_offset = line_start_byte_offset;
        for grapheme in line_slice.graphemes() {
            if grapheme.chars().any(|c| c == '\n' || c == '\r') {
                break;
            }
            if !grapheme.chars().all(|c| c.is_whitespace()) {
                self.state.byte_offset = byte_offset;
                return;
            }
            byte_offset += grapheme.len();
        }
        self.state.byte_offset = byte_offset;
    }

    pub fn move_to_line_end(&mut self, bias: Bias) -> bool {
        if self.text.len() == 0 || self.is_at_end() {
            return false;
        }

        let pre = self.state.byte_offset;

        #[expect(clippy::manual_let_else)]
        let byte_index = match self.byte_index(bias) {
            Ok(n) | Err(Some(n)) => n,
            Err(None) => unreachable!("Already checked rope length"),
        };

        let line_index = self.text.byte_to_line_idx(byte_index, LINE_TYPE);

        if bias == Bias::Before
            && self
                .text
                .byte_to_line_idx(self.state.byte_offset, LINE_TYPE)
                != line_index
        {
            // Already past the newline of the line we have bias for
            return false;
        }

        // Find last character before line ending
        let line_start_byte_offset = self.text.line_to_byte_idx(line_index, LINE_TYPE);
        let line_slice = self.text.line(line_index, LINE_TYPE);
        let mut byte_offset = line_start_byte_offset;
        for grapheme in line_slice.graphemes() {
            if grapheme.chars().any(|c| c == '\n' || c == '\r') {
                break;
            }
            byte_offset += grapheme.len();
        }
        self.state.byte_offset = byte_offset;
        debug_assert!(self.state.byte_offset >= pre);
        true
    }

    pub fn insert_char(&mut self, char: char) -> OperationSeq {
        self.insert(&char.to_string())
    }

    #[tracing::instrument(skip_all)]
    pub fn insert(&mut self, text: &str) -> OperationSeq {
        let pre_offset = self.state.byte_offset;
        let pre_text_len = self.text.len();
        let mut ops = OperationSeq::new();
        ops.retain(self.state.byte_offset);
        ops.insert(text);
        ops.retain_rest(&self.text);
        self.text.apply(&ops).expect("Operations are well formed");
        self.state.transform(&ops);
        if self.snap_to_grapheme_boundary() {
            tracing::warn!("wasn't on grapheme boundary after");
        }
        debug_assert_eq!(self.text.len(), pre_text_len + text.len());
        debug_assert!(self.state.byte_offset >= pre_offset + text.len());
        ops
    }

    // Behavior traditionally associated with the Backspace key.
    #[tracing::instrument(skip_all)]
    pub fn delete_before(&mut self) -> Option<OperationSeq> {
        let pre_offset = self.state.byte_offset;
        let pre_text_len = self.text.len();
        let mut byte_offset = self.state.byte_offset;
        if let Some(prev) = self.text.prev_grapheme_boundary(byte_offset)
            && byte_offset != prev
        {
            byte_offset = prev;
        } else {
            return None;
        }
        let mut ops = OperationSeq::new();
        ops.retain(byte_offset);
        ops.delete(self.state.byte_offset - byte_offset);
        ops.retain_rest(&self.text);
        self.text.apply(&ops).expect("Operations are well formed");
        self.state.transform(&ops);
        if self.snap_to_grapheme_boundary() {
            tracing::warn!("wasn't on grapheme boundary after");
        }
        debug_assert_eq!(self.text.len(), pre_text_len - (pre_offset - byte_offset));
        // Cursor lands at `byte_offset` after the transform; `snap_to_grapheme_boundary`
        // can ceil forward but never backward. It can ceil past `pre_offset` if the
        // deletion merges graphemes (e.g. removing a Control between a base and an
        // Extend), so we can only assert the lower bound.
        debug_assert!(self.state.byte_offset >= byte_offset);
        Some(ops)
    }

    // Behavior traditionally associated with the Delete key.
    #[tracing::instrument(skip_all)]
    pub fn delete_after(&mut self) -> Option<OperationSeq> {
        let pre_offset = self.state.byte_offset;
        let pre_text_len = self.text.len();
        let mut byte_offset = self.state.byte_offset;
        if let Some(next) = self.text.next_grapheme_boundary(byte_offset)
            && byte_offset != next
        {
            byte_offset = next;
        } else {
            return None;
        }
        let mut ops = OperationSeq::new();
        ops.retain(self.state.byte_offset);
        ops.delete(byte_offset - self.state.byte_offset);
        ops.retain_rest(&self.text);
        self.text.apply(&ops).expect("Operations are well formed");
        self.state.transform(&ops);
        if self.snap_to_grapheme_boundary() {
            tracing::warn!("wasn't on grapheme boundary after");
        }
        debug_assert_eq!(self.text.len(), pre_text_len - (byte_offset - pre_offset));
        debug_assert!(self.state.byte_offset >= pre_offset);
        Some(ops)
    }

    pub fn restore(&mut self, snapshot: &CursorSnapshot) -> bool {
        if let Some(state) = snapshot.restore(&self.text) {
            *self.state = state;
            true
        } else {
            false
        }
    }
}

pub fn handle_action<W: WrapMut>(cursor: &mut CursorView<'_, W>, action: &Action) {
    match action {
        Action::SnapToGraphemeBoundary => {
            cursor.snap_to_grapheme_boundary();
        }
        Action::UncheckedMoveTo(byte_offset) => {
            // Snapping to grapheme boundary because `unchecked_move_to` is otherwise unsafe.
            let byte_offset = cursor.text.ceil_grapheme_boundary(*byte_offset);
            cursor.unchecked_move_to(byte_offset);
        }
        Action::MoveLeft(count) => {
            cursor.move_left(usize::from(*count));
        }
        Action::MoveRight(count) => {
            cursor.move_right(usize::from(*count));
        }
        Action::MoveUp {
            goal_column,
            bias,
            count,
        } => {
            cursor.move_up(usize::from(*goal_column), *bias, usize::from(*count));
        }
        Action::MoveDown {
            goal_column,
            bias,
            count,
        } => {
            cursor.move_down(usize::from(*goal_column), *bias, usize::from(*count));
        }
        Action::MoveToPrevByte { byte, count } => {
            cursor.move_to_prev_byte(*byte, usize::from(*count));
        }
        Action::MoveToNextByte { byte, count } => {
            cursor.move_to_next_byte(*byte, usize::from(*count));
        }
        Action::MoveToPrevBlank(count) => {
            cursor.move_to_prev_blank(usize::from(*count));
        }
        Action::MoveToNextBlank(count) => {
            cursor.move_to_next_blank(usize::from(*count));
        }
        Action::MoveToStart => cursor.move_to_start(),
        Action::MoveToEnd => cursor.move_to_end(),
        Action::MoveToBottom(bias) => cursor.move_to_bottom(*bias),
        Action::MoveToLineStart(bias) => cursor.move_to_line_start(*bias),
        Action::MoveToLineNonBlankStart(bias) => cursor.move_to_line_non_blank_start(*bias),
        Action::MoveToLineEnd(bias) => {
            cursor.move_to_line_end(*bias);
        }
        Action::InsertChar(c) => {
            cursor.insert_char(*c);
        }
        Action::Insert(text) => {
            cursor.insert(text);
        }
        Action::DeleteBefore => {
            cursor.delete_before();
        }
        Action::DeleteAfter => {
            cursor.delete_after();
        }
    }
}

impl<R> TryFrom<(R, usize)> for CursorView<'_, WBox>
where
    R: Into<Text>,
{
    type Error = anyhow::Error;
    fn try_from((text, byte_offset): (R, usize)) -> anyhow::Result<Self> {
        let text = Box::new(text.into());
        let state = Box::new(CursorState { byte_offset });
        Self::new(text, state)
    }
}

impl<W: Wrap> Drop for CursorView<'_, W> {
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
    use arbtest::arbtest;
    use std::{cmp::max, sync::mpsc, thread};

    macro_rules! defer {
        ($f:expr) => {
            let __defer = {
                struct Defer<F: FnMut()>(F);
                impl<F: FnMut()> Drop for Defer<F> {
                    fn drop(&mut self) {
                        (self.0)();
                    }
                }
                Defer($f)
            };
        };
    }

    #[test]
    fn delete_before_can_merge_graphemes_past_cursor() {
        // Cursor sits between a Control (SUB, U+001A) and a combining mark
        // (U+036B). Deleting the Control causes '@' and the combining mark to
        // merge into a single grapheme cluster, so the cursor's post-transform
        // position is no longer on a boundary, and snap ceils it forward past
        // the original `pre_offset`.
        let mut cursor = CursorView::try_from(("@\u{1a}\u{36b}", 2)).unwrap();
        cursor.delete_before();
        cursor.assert_invariants().unwrap();
        // pre_offset was 2; cursor ends at 3 because '@' merged with the
        // combining mark into "@\u{36b}" (one 3-byte grapheme).
        assert_eq!(cursor.byte_offset(), 3);
    }

    #[test]
    fn insert_changes_grapheme_boundary() {
        // combining acute accent (´)
        let mut cursor = CursorView::try_from(("\u{0301}", 0)).unwrap();
        cursor.insert("e");
        cursor.assert_invariants().unwrap();
    }

    #[test]
    fn move_down_before_to_unterminated_final_line_end() {
        let mut cursor = CursorView::try_from(("\nx", 2)).unwrap();
        cursor.move_to_line_non_blank_start(Bias::Before);
        cursor.move_down(usize::MAX, Bias::Before, 1);
        assert_eq!(cursor.byte_offset(), 2);
        cursor.assert_invariants().unwrap();
    }

    #[test]
    fn move_to_next_byte_repeated_after_multibyte_match() {
        let mut cursor = CursorView::try_from(("\nۉ", 3)).unwrap();
        cursor.move_to_line_non_blank_start(Bias::Before);
        cursor.move_to_next_byte(219, 2);
        cursor.assert_invariants().unwrap();
    }

    #[test]
    fn move_to_next_blank_repeated_after_crlf_match() {
        let mut cursor = CursorView::try_from(("\r", 1)).unwrap();
        cursor.insert("\n");
        cursor.move_left(27);
        cursor.move_to_next_blank(10);
        cursor.delete_after();
        cursor.assert_invariants().unwrap();
    }

    #[test]
    fn restore_snaps_to_grapheme_boundary_after_insert() {
        let mut cursor = CursorView::try_from(("\u{0301}", 0)).unwrap();
        let snapshot = cursor.save();
        cursor.insert("e");
        assert!(cursor.restore(&snapshot));
        cursor.assert_invariants().unwrap();
    }

    #[test]
    fn expected_behavior_left_right() {
        let mut cursor = CursorView::try_from(("", 0)).unwrap();
        cursor.assert_invariants().unwrap();

        cursor.insert("hello");
        assert_eq!(**cursor.text, "hello");
        assert_eq!(cursor.byte_offset(), 5);

        cursor.delete_before();
        cursor.delete_before();
        assert_eq!(**cursor.text, "hel");
        assert_eq!(cursor.byte_offset(), 3);

        cursor.move_left(2);
        assert_eq!(cursor.byte_offset(), 1);

        cursor.delete_after();
        assert_eq!(cursor.byte_offset(), 1);
        assert_eq!(**cursor.text, "hl");

        cursor.move_right(1);
        assert_eq!(cursor.byte_offset(), 2);
        cursor.move_right(1);
        assert_eq!(cursor.byte_offset(), 2);

        cursor.delete_after();
        assert_eq!(**cursor.text, "hl");
        assert_eq!(cursor.byte_offset(), 2);

        cursor.delete_before();
        cursor.delete_before();
        assert_eq!(**cursor.text, "");
        assert_eq!(cursor.byte_offset(), 0);

        cursor.delete_before();
        assert_eq!(**cursor.text, "");
        assert_eq!(cursor.byte_offset(), 0);
    }

    #[test]
    fn expected_behavior_up_down() {
        let bias = Bias::After;

        let mut cursor = CursorView::try_from(("", 0)).unwrap();
        cursor.assert_invariants().unwrap();

        let text = "0\n234\n6789AB\n";
        cursor.insert(text);
        // At end of text.
        assert_eq!(cursor.grapheme(bias), None);
        assert_eq!(cursor.line(Bias::After), None);
        assert_eq!(
            cursor.line(Bias::Before).map(|rope| rope.to_string()),
            Some(String::from("6789AB\n"))
        );
        assert_eq!(cursor.byte_offset(), 13);
        assert_eq!(cursor.byte_offset(), text.len());
        assert_eq!(cursor.display_column(Bias::Before), Some(6));

        cursor.move_left(1);
        // At final newline on line 2.
        assert_eq!(cursor.grapheme(bias), Some(Rope::from("\n").slice(..)));
        assert_eq!(cursor.byte_offset(), 12);
        assert_eq!(cursor.display_column(Bias::After), Some(6));

        cursor.move_up(cursor.display_column(bias).unwrap_or(0), bias, 1);
        // At second newline on line 1, which is shorter than the goal column.
        assert_eq!(cursor.grapheme(bias), Some(Rope::from("\n").slice(..)));
        assert_eq!(cursor.byte_offset(), 5);
        // Goal column should remain the same through vertical movement.
        assert_eq!(cursor.display_column(Bias::After), Some(3));

        cursor.move_left(1);
        // At "4" on line 1.
        assert_eq!(cursor.grapheme(bias), Some(Rope::from("4").slice(..)));
        assert_eq!(
            cursor.line(Bias::After).map(|rope| rope.to_string()),
            Some(String::from("234\n"))
        );
        assert_eq!(
            cursor.line(Bias::Before).map(|rope| rope.to_string()),
            Some(String::from("234\n"))
        );
        assert_eq!(cursor.byte_offset(), 4);
        // Goal column should change through horizontal movement.
        assert_eq!(cursor.display_column(Bias::After), Some(2));
    }

    #[test]
    fn move_to_line_end() {
        let cases = [
            // When bias is for `\n` before cursor offset, should remain in place because it's
            // considered already at the end of line 1.
            (Bias::Before, 2),
            // When bias is for `y` after cursor offset, should move right to before following
            // newline, because it's considered at the start of line 2.
            (Bias::After, 3),
        ];
        for (bias, byte_offset) in cases {
            let mut cursor = CursorView::try_from(("x\ny\n", 0)).unwrap();
            assert_eq!(cursor.byte_offset(), 0);
            cursor.move_to_next_byte(b'\n', 1);
            cursor.move_right(1);
            assert_eq!(cursor.byte_offset(), 2);
            cursor.move_to_line_end(bias);
            assert_eq!(cursor.byte_offset(), byte_offset, "with bias={bias:?}");
        }
    }

    #[test]
    fn move_to_next_byte_count_skips_previous_matches() {
        let mut cursor = CursorView::try_from(("hello world\n", 0)).unwrap();

        assert!(cursor.move_to_next_byte(b'l', 1));
        assert_eq!(cursor.byte_offset(), 2);

        let mut cursor = CursorView::try_from(("hello world\n", 0)).unwrap();
        assert!(cursor.move_to_next_byte(b'l', 2));
        assert_eq!(cursor.byte_offset(), 3);

        let mut cursor = CursorView::try_from(("hello world\n", 0)).unwrap();
        assert!(cursor.move_to_next_byte(b'l', 3));
        assert_eq!(cursor.byte_offset(), 9);
    }

    #[test]
    fn move_to_next_blank_count_skips_previous_matches() {
        let mut cursor = CursorView::try_from(("a b\tc\n", 0)).unwrap();

        assert!(cursor.move_to_next_blank(1));
        assert_eq!(cursor.byte_offset(), 1);

        let mut cursor = CursorView::try_from(("a b\tc\n", 0)).unwrap();
        assert!(cursor.move_to_next_blank(2));
        assert_eq!(cursor.byte_offset(), 3);

        let mut cursor = CursorView::try_from(("a b\tc\n", 0)).unwrap();
        assert!(cursor.move_to_next_blank(3));
        assert_eq!(cursor.byte_offset(), 5);
    }

    #[test]
    fn byte_index() {
        let cursor = CursorView::try_from(("xy", 1)).unwrap();
        assert_eq!(cursor.byte_index(Bias::Before), Ok(0));
        assert_eq!(cursor.byte_index(Bias::After), Ok(1));
        assert_eq!(cursor.text().char(0), 'x');
        assert_eq!(cursor.text().char(1), 'y');

        let cursor = CursorView::try_from(("x", 0)).unwrap();
        assert_eq!(cursor.byte_index(Bias::Before), Err(Some(0)));

        let cursor = CursorView::try_from(("x", 1)).unwrap();
        assert_eq!(cursor.byte_index(Bias::After), Err(Some(0)));

        let cursor = CursorView::try_from(("", 0)).unwrap();
        assert_eq!(cursor.byte_index(Bias::Before), Err(None));
        assert_eq!(cursor.byte_index(Bias::After), Err(None));
    }

    #[test]
    fn fuzz() {
        arbtest(|u| {
            let text = Text::new();
            let byte_offset = if u.arbitrary::<bool>()? {
                text.floor_grapheme_boundary(u.arbitrary::<usize>()?)
            } else {
                text.ceil_grapheme_boundary(u.arbitrary::<usize>()?)
            };
            let bias = Bias::After;
            let mut cursor = CursorView::try_from((text, byte_offset)).unwrap();
            let (tx, rx) = mpsc::channel();
            defer!(|| {
                if thread::panicking() {
                    let actions: Vec<String> = rx.try_iter().collect();
                    if !actions.is_empty() {
                        eprintln!("Actions:\n  {}", actions.join("\n  "));
                    }
                }
            });
            for _ in 0..u.choose_index(100)? {
                match u.choose_index(7)? {
                    0 => {
                        let count = max(1, u.choose_index(99)?);
                        cursor.move_left(count);
                        tx.send(format!("move_left() x{count}")).unwrap();
                    }
                    1 => {
                        let count = max(1, u.choose_index(99)?);
                        cursor.move_right(count);
                        tx.send(format!("move_right() x{count}")).unwrap();
                    }

                    2 => {
                        let count = max(1, u.choose_index(99)?);
                        cursor.move_up(cursor.display_column(bias).unwrap_or(0), bias, count);
                        tx.send(format!("move_up() x{count}")).unwrap();
                    }
                    3 => {
                        let count = max(1, u.choose_index(99)?);
                        cursor.move_down(cursor.display_column(bias).unwrap_or(0), bias, count);
                        tx.send(format!("move_down() x{count}")).unwrap();
                    }
                    4 => {
                        let text = u.arbitrary()?;
                        cursor.insert(text);
                        tx.send(format!("insert({text:?})")).unwrap();
                    }
                    5 => {
                        let count = max(1, u.choose_index(99)?);
                        for _ in 1..=count {
                            cursor.delete_before();
                        }
                        tx.send(format!("delete_before() x{count}")).unwrap();
                    }
                    6 => {
                        let count = max(1, u.choose_index(99)?);
                        for _ in 1..=count {
                            cursor.delete_after();
                        }
                        tx.send(format!("delete_after() x{count}")).unwrap();
                    }
                    _ => unreachable!(),
                }
                cursor.assert_invariants().unwrap();
            }
            Ok(())
        });
    }
}
