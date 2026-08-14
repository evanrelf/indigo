use crate::{
    ot2::OperationSeq,
    rope::{DisplayWidth as _, LINE_TYPE, RopeExt as _},
    text::{Anchor, Text},
};
use indigo_wrap::{WBox, WMut, WRef, Wrap, WrapMut, WrapRef};
use ropey::{Rope, RopeSlice};
use std::thread;
use thiserror::Error;

#[cfg(feature = "arbitrary")]
use arbitrary::Arbitrary;

#[derive(Debug, Error)]
pub enum Error {
    #[error("Byte index {byte_index} is not within text of length {length}")]
    OutOfRange { byte_index: usize, length: usize },

    #[error("Byte index {byte_index} is not the start of a grapheme")]
    NotOnGrapheme { byte_index: usize },
}

#[cfg_attr(feature = "arbitrary", derive(Arbitrary))]
#[derive(Debug)]
pub enum Action {
    MoveTo { byte_index: usize },
    MoveLeft { count: u8 },
    MoveRight { count: u8 },
    MoveUp { goal_column: u16, count: u8 },
    MoveDown { goal_column: u16, count: u8 },
    MoveToPrevByte { byte: u8, count: u8 },
    MoveToNextByte { byte: u8, count: u8 },
    MoveToPrevBlank { count: u8 },
    MoveToNextBlank { count: u8 },
    MoveToStart,
    MoveToEnd,
    MoveToBottom,
    MoveToLineStart,
    MoveToLineNonBlankStart,
    MoveToLineEnd,
    MoveUntilLineEnd,
    InsertChar { char: char },
    Insert { text: String },
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
    pub byte_index: usize,
}

impl CursorState {
    pub fn transform(&mut self, ops: &OperationSeq, text: &Rope) {
        let byte_index = ops.transform_byte_offset(self.byte_index);
        self.byte_index = text
            .snap_to_grapheme_start(byte_index)
            .expect("Text is never empty");
    }

    #[must_use]
    pub fn save(&self, text: &Text) -> CursorSnapshot {
        let byte_index = text.create_anchor(self.byte_index);
        CursorSnapshot { byte_index }
    }
}

pub struct CursorSnapshot {
    pub byte_index: Anchor,
}

impl CursorSnapshot {
    #[must_use]
    pub fn restore(&self, text: &Text) -> Option<CursorState> {
        let byte_index = text
            .snap_to_grapheme_start(text.resolve_anchor(&self.byte_index)?)
            .expect("Text is never empty");
        Some(CursorState { byte_index })
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
    pub fn byte_index(&self) -> usize {
        self.state.byte_index
    }

    #[must_use]
    pub fn line_index(&self) -> usize {
        self.text.byte_to_line_idx(self.state.byte_index, LINE_TYPE)
    }

    #[must_use]
    pub fn line_number(&self) -> usize {
        self.line_index() + 1
    }

    #[must_use]
    pub fn column_number(&self) -> usize {
        let line_byte_index = self.text.line_to_byte_idx(self.line_index(), LINE_TYPE);
        (self.state.byte_index - line_byte_index) + 1
    }

    #[must_use]
    pub fn display_column(&self) -> usize {
        self.text.display_column(self.state.byte_index)
    }

    #[must_use]
    pub fn grapheme(&self) -> RopeSlice<'_> {
        self.text.grapheme(self.state.byte_index)
    }

    #[must_use]
    pub fn line(&self) -> RopeSlice<'_> {
        self.text
            .get_line(self.line_index(), LINE_TYPE)
            .expect("Cursor's line always exists")
    }

    #[must_use]
    pub fn is_at_start(&self) -> bool {
        self.state.byte_index == 0
    }

    #[must_use]
    pub fn is_at_end(&self) -> bool {
        let end_offset = self
            .text
            .next_grapheme_boundary(self.state.byte_index)
            .expect("Cursor is always on a grapheme");
        end_offset == self.text.len()
    }

    #[must_use]
    pub fn save(&self) -> CursorSnapshot {
        self.state.save(&self.text)
    }

    pub fn assert_invariants(&self) -> anyhow::Result<()> {
        if self.state.byte_index >= self.text.len() {
            anyhow::bail!(Error::OutOfRange {
                byte_index: self.state.byte_index,
                length: self.text.len(),
            });
        }
        if !self.text.is_grapheme_start(self.state.byte_index) {
            anyhow::bail!(Error::NotOnGrapheme {
                byte_index: self.state.byte_index,
            });
        }
        Ok(())
    }
}

impl<W: WrapMut> CursorView<'_, W> {
    pub fn move_to(&mut self, byte_index: usize) {
        let byte_index = self
            .text
            .snap_to_grapheme_start(byte_index)
            .expect("Text is never empty");
        self.state.byte_index = byte_index;
    }

    pub fn move_left(&mut self, count: usize) -> bool {
        if count == 0 {
            return false;
        }
        for _ in 0..count {
            if let Some(prev) = self.text.prev_grapheme_boundary(self.state.byte_index) {
                self.state.byte_index = prev;
            } else {
                return false;
            }
        }
        true
    }

    pub fn move_right(&mut self, count: usize) -> bool {
        if count == 0 {
            return false;
        }
        for _ in 0..count {
            match self.text.next_grapheme_boundary(self.state.byte_index) {
                Some(next) if next < self.text.len() => self.state.byte_index = next,
                _ => return false,
            }
        }
        true
    }

    pub fn move_up(&mut self, goal_column: usize, count: usize) -> bool {
        self.move_vertical(Direction::Backward, goal_column, count)
    }

    pub fn move_down(&mut self, goal_column: usize, count: usize) -> bool {
        self.move_vertical(Direction::Forward, goal_column, count)
    }

    fn move_vertical(&mut self, direction: Direction, goal_column: usize, count: usize) -> bool {
        if count == 0 {
            return false;
        }
        for _ in 0..count {
            let current_line_index = self.text.byte_to_line_idx(self.state.byte_index, LINE_TYPE);
            let target_line_index = match direction {
                Direction::Backward => {
                    if current_line_index == 0 {
                        return false;
                    }
                    current_line_index - 1
                }
                Direction::Forward => {
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
            self.state.byte_index = target_byte_index;
        }
        true
    }

    pub fn move_to_prev_byte(&mut self, byte: u8, count: usize) -> bool {
        if count == 0 {
            return false;
        }
        for _ in 0..count {
            if let Some(found) = self.text.find_prev_byte(..self.state.byte_index, &[byte]) {
                self.state.byte_index = self.text.floor_grapheme_boundary(found);
            } else {
                return false;
            }
        }
        true
    }

    pub fn move_to_next_byte(&mut self, byte: u8, count: usize) -> bool {
        self.seek_next(&[byte], count)
    }

    pub fn move_to_prev_blank(&mut self, count: usize) -> bool {
        const BYTES: &[u8] = b" \t\n\r";
        if count == 0 {
            return false;
        }
        for _ in 0..count {
            if let Some(found) = self.text.find_prev_byte(..self.state.byte_index, BYTES) {
                self.state.byte_index = self.text.floor_grapheme_boundary(found);
            } else {
                return false;
            }
        }
        true
    }

    pub fn move_to_next_blank(&mut self, count: usize) -> bool {
        const BYTES: &[u8] = b" \t\n\r";
        self.seek_next(BYTES, count)
    }

    /// Land on the grapheme containing the next matching byte, searching strictly after the
    /// grapheme the cursor occupies.
    fn seek_next(&mut self, bytes: &[u8], count: usize) -> bool {
        if count == 0 {
            return false;
        }
        let mut start = self
            .text
            .next_grapheme_boundary(self.state.byte_index)
            .expect("Cursor is always on a grapheme");
        for _ in 0..count {
            if let Some(found) = self.text.find_next_byte(start.., bytes) {
                let byte_index = self.text.floor_grapheme_boundary(found);
                self.state.byte_index = byte_index;
                start = self
                    .text
                    .next_grapheme_boundary(byte_index)
                    .expect("Found byte is within the text");
            } else {
                return false;
            }
        }
        true
    }

    pub fn move_to_start(&mut self) {
        self.state.byte_index = 0;
    }

    pub fn move_to_end(&mut self) {
        self.state.byte_index = self
            .text
            .last_grapheme_start()
            .expect("Text is never empty");
    }

    pub fn move_to_bottom(&mut self) {
        self.move_to_end();
        self.move_to_line_start();
    }

    pub fn move_to_line_start(&mut self) {
        let line_index = self.text.byte_to_line_idx(self.state.byte_index, LINE_TYPE);
        self.state.byte_index = self.text.line_to_byte_idx(line_index, LINE_TYPE);
    }

    pub fn move_to_line_non_blank_start(&mut self) {
        let line_index = self.text.byte_to_line_idx(self.state.byte_index, LINE_TYPE);
        let line_start_byte_index = self.text.line_to_byte_idx(line_index, LINE_TYPE);
        let line_slice = self.text.line(line_index, LINE_TYPE);
        let mut byte_index = line_start_byte_index;
        for grapheme in line_slice.graphemes() {
            if grapheme.chars().any(|c| c == '\n' || c == '\r') {
                break;
            }
            if !grapheme.chars().all(char::is_whitespace) {
                break;
            }
            byte_index += grapheme.len();
        }
        self.state.byte_index = byte_index;
    }

    /// Move onto the line's end: its `'\n'` (or `"\r\n"`) grapheme. Always succeeds because
    /// every line is terminated.
    pub fn move_to_line_end(&mut self) {
        let line_index = self.text.byte_to_line_idx(self.state.byte_index, LINE_TYPE);
        let line_start_byte_index = self.text.line_to_byte_idx(line_index, LINE_TYPE);
        let line_slice = self.text.line(line_index, LINE_TYPE);
        let mut byte_index = line_start_byte_index;
        for grapheme in line_slice.graphemes() {
            if grapheme.chars().any(|c| c == '\n' || c == '\r') {
                break;
            }
            byte_index += grapheme.len();
        }
        self.state.byte_index = byte_index;
    }

    /// Move onto the last grapheme before the line's end. Stays on the line end grapheme when
    /// the line is empty.
    pub fn move_until_line_end(&mut self) {
        self.move_to_line_end();
        let line_index = self.text.byte_to_line_idx(self.state.byte_index, LINE_TYPE);
        let line_start_byte_index = self.text.line_to_byte_idx(line_index, LINE_TYPE);
        if self.state.byte_index > line_start_byte_index {
            self.state.byte_index = self
                .text
                .prev_grapheme_boundary(self.state.byte_index)
                .expect("Not at start of text");
        }
    }

    pub fn insert_char(&mut self, char: char) -> OperationSeq {
        self.insert(&char.to_string())
    }

    /// Insert before the grapheme the cursor occupies. The cursor stays on its grapheme (i.e.
    /// ends up after the inserted text).
    #[tracing::instrument(skip_all)]
    pub fn insert(&mut self, text: &str) -> OperationSeq {
        let pre_text_len = self.text.len();
        let mut ops = OperationSeq::new();
        ops.retain(self.state.byte_index);
        ops.insert(text);
        ops.retain_rest(&self.text)
            .expect("Operations fit within text");
        self.text.apply(&ops).expect("Operations are well formed");
        self.state.transform(&ops, &self.text);
        debug_assert_eq!(self.text.len(), pre_text_len + text.len());
        ops
    }

    /// Delete the grapheme before the cursor. Behavior traditionally associated with the
    /// Backspace key.
    #[tracing::instrument(skip_all)]
    pub fn delete_before(&mut self) -> Option<OperationSeq> {
        let start = self.text.prev_grapheme_boundary(self.state.byte_index)?;
        let mut ops = OperationSeq::new();
        ops.retain(start);
        ops.delete(&self.text.slice(start..self.state.byte_index).to_string());
        ops.retain_rest(&self.text)
            .expect("Operations fit within text");
        self.text.apply(&ops).expect("Operations are well formed");
        self.state.transform(&ops, &self.text);
        Some(ops)
    }

    /// Delete the grapheme the cursor occupies, unless it is the text's final newline. Behavior
    /// traditionally associated with the Delete key.
    #[tracing::instrument(skip_all)]
    pub fn delete_after(&mut self) -> Option<OperationSeq> {
        let end = self
            .text
            .next_grapheme_boundary(self.state.byte_index)
            .expect("Cursor is always on a grapheme");
        if end == self.text.len() {
            // Deleting the final newline would break the `Text` invariant.
            return None;
        }
        let mut ops = OperationSeq::new();
        ops.retain(self.state.byte_index);
        ops.delete(&self.text.slice(self.state.byte_index..end).to_string());
        ops.retain_rest(&self.text)
            .expect("Operations fit within text");
        self.text.apply(&ops).expect("Operations are well formed");
        self.state.transform(&ops, &self.text);
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
        Action::MoveTo { byte_index } => {
            cursor.move_to(*byte_index);
        }
        Action::MoveLeft { count } => {
            cursor.move_left(usize::from(*count));
        }
        Action::MoveRight { count } => {
            cursor.move_right(usize::from(*count));
        }
        Action::MoveUp { goal_column, count } => {
            cursor.move_up(usize::from(*goal_column), usize::from(*count));
        }
        Action::MoveDown { goal_column, count } => {
            cursor.move_down(usize::from(*goal_column), usize::from(*count));
        }
        Action::MoveToPrevByte { byte, count } => {
            cursor.move_to_prev_byte(*byte, usize::from(*count));
        }
        Action::MoveToNextByte { byte, count } => {
            cursor.move_to_next_byte(*byte, usize::from(*count));
        }
        Action::MoveToPrevBlank { count } => {
            cursor.move_to_prev_blank(usize::from(*count));
        }
        Action::MoveToNextBlank { count } => {
            cursor.move_to_next_blank(usize::from(*count));
        }
        Action::MoveToStart => cursor.move_to_start(),
        Action::MoveToEnd => cursor.move_to_end(),
        Action::MoveToBottom => cursor.move_to_bottom(),
        Action::MoveToLineStart => cursor.move_to_line_start(),
        Action::MoveToLineNonBlankStart => cursor.move_to_line_non_blank_start(),
        Action::MoveToLineEnd => cursor.move_to_line_end(),
        Action::MoveUntilLineEnd => cursor.move_until_line_end(),
        Action::InsertChar { char } => {
            cursor.insert_char(*char);
        }
        Action::Insert { text } => {
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
    fn try_from((text, byte_index): (R, usize)) -> anyhow::Result<Self> {
        let text = Box::new(text.into());
        let state = Box::new(CursorState { byte_index });
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
    use hegel::{TestCase, generators as gs};
    use ropey::Rope;

    #[test]
    fn delete_before_can_merge_graphemes_past_cursor() {
        // Cursor sits on a combining mark (U+036B) preceded by a Control (SUB, U+001A).
        // Deleting the Control causes '@' and the combining mark to merge into a single
        // grapheme cluster, so the cursor's post-transform position is no longer a grapheme
        // start, and the snap floors it onto the merged grapheme.
        let mut cursor = CursorView::try_from(("@\u{1a}\u{36b}", 2)).unwrap();
        cursor.delete_before();
        cursor.assert_invariants().unwrap();
        // '@' merged with the combining mark into "@\u{36b}" (one grapheme starting at 0).
        assert_eq!(cursor.byte_index(), 0);
        assert_eq!(**cursor.text, "@\u{36b}\n");
    }

    #[test]
    fn insert_changes_grapheme_boundary() {
        // combining acute accent (´)
        let mut cursor = CursorView::try_from(("\u{0301}", 0)).unwrap();
        cursor.insert("e");
        cursor.assert_invariants().unwrap();
    }

    #[test]
    fn move_to_next_byte_repeated_after_multibyte_match() {
        let mut cursor = CursorView::try_from(("\nۉ", 3)).unwrap();
        cursor.move_to_line_non_blank_start();
        cursor.move_to_next_byte(219, 2);
        cursor.assert_invariants().unwrap();
    }

    #[test]
    fn move_to_next_blank_repeated_after_crlf_match() {
        // `Text::from("\r")` normalizes to "\r\n", a single CRLF grapheme.
        let mut cursor = CursorView::try_from(("\r", 0)).unwrap();
        cursor.insert("\n");
        cursor.move_left(27);
        cursor.move_to_next_blank(10);
        cursor.delete_after();
        cursor.assert_invariants().unwrap();
    }

    #[test]
    fn restore_snaps_to_grapheme_start_after_insert() {
        let mut cursor = CursorView::try_from(("\u{0301}", 0)).unwrap();
        let snapshot = cursor.save();
        cursor.insert("e");
        assert!(cursor.restore(&snapshot));
        cursor.assert_invariants().unwrap();
    }

    #[test]
    fn delete_after_preserves_final_newline() {
        let mut cursor = CursorView::try_from(("x", 1)).unwrap();
        assert_eq!(cursor.grapheme(), Rope::from("\n").slice(..));
        assert_eq!(cursor.delete_after(), None);
        assert_eq!(**cursor.text, "x\n");
    }

    #[test]
    fn expected_behavior_left_right() {
        let mut cursor = CursorView::try_from(("", 0)).unwrap();
        cursor.assert_invariants().unwrap();

        cursor.insert("hello");
        assert_eq!(**cursor.text, "hello\n");
        assert_eq!(cursor.byte_index(), 5);

        cursor.delete_before();
        cursor.delete_before();
        assert_eq!(**cursor.text, "hel\n");
        assert_eq!(cursor.byte_index(), 3);

        cursor.move_left(2);
        assert_eq!(cursor.byte_index(), 1);

        cursor.delete_after();
        assert_eq!(cursor.byte_index(), 1);
        assert_eq!(**cursor.text, "hl\n");

        cursor.move_right(1);
        assert_eq!(cursor.byte_index(), 2);
        // The trailing newline is the last grapheme; can't move past it.
        cursor.move_right(1);
        assert_eq!(cursor.byte_index(), 2);

        // Deleting the grapheme under the cursor would delete the final newline; refused.
        cursor.delete_after();
        assert_eq!(**cursor.text, "hl\n");
        assert_eq!(cursor.byte_index(), 2);

        cursor.delete_before();
        cursor.delete_before();
        assert_eq!(**cursor.text, "\n");
        assert_eq!(cursor.byte_index(), 0);

        cursor.delete_before();
        assert_eq!(**cursor.text, "\n");
        assert_eq!(cursor.byte_index(), 0);
    }

    #[test]
    fn expected_behavior_up_down() {
        let mut cursor = CursorView::try_from(("", 0)).unwrap();
        cursor.assert_invariants().unwrap();

        let text = "0\n234\n6789AB\n";
        cursor.insert(text);
        // On the invariant trailing newline (the empty final line the insert pushed down).
        assert_eq!(cursor.grapheme(), Rope::from("\n").slice(..));
        assert_eq!(cursor.line().to_string(), "\n");
        assert_eq!(cursor.byte_index(), 13);
        assert_eq!(cursor.display_column(), 0);

        cursor.move_left(1);
        // On the newline terminating line 2.
        assert_eq!(cursor.grapheme(), Rope::from("\n").slice(..));
        assert_eq!(cursor.byte_index(), 12);
        assert_eq!(cursor.display_column(), 6);

        cursor.move_up(cursor.display_column(), 1);
        // On the newline terminating line 1, which is shorter than the goal column.
        assert_eq!(cursor.grapheme(), Rope::from("\n").slice(..));
        assert_eq!(cursor.byte_index(), 5);
        assert_eq!(cursor.display_column(), 3);

        cursor.move_left(1);
        // On "4" in line 1.
        assert_eq!(cursor.grapheme(), Rope::from("4").slice(..));
        assert_eq!(cursor.line().to_string(), "234\n");
        assert_eq!(cursor.byte_index(), 4);
        assert_eq!(cursor.display_column(), 2);
    }

    #[test]
    fn move_to_line_end() {
        let mut cursor = CursorView::try_from(("x\ny\n", 2)).unwrap();
        assert_eq!(cursor.grapheme(), Rope::from("y").slice(..));

        cursor.move_to_line_end();
        assert_eq!(cursor.byte_index(), 3);
        assert_eq!(cursor.grapheme(), Rope::from("\n").slice(..));

        // Idempotent: already on the line end.
        cursor.move_to_line_end();
        assert_eq!(cursor.byte_index(), 3);

        cursor.move_until_line_end();
        assert_eq!(cursor.byte_index(), 2);
        assert_eq!(cursor.grapheme(), Rope::from("y").slice(..));

        // On an empty line, `move_until_line_end` stays on the line end.
        let mut cursor = CursorView::try_from(("\nx\n", 0)).unwrap();
        cursor.move_until_line_end();
        assert_eq!(cursor.byte_index(), 0);
    }

    #[test]
    fn move_to_next_byte_count_skips_previous_matches() {
        let mut cursor = CursorView::try_from(("hello world\n", 0)).unwrap();

        assert!(cursor.move_to_next_byte(b'l', 1));
        assert_eq!(cursor.byte_index(), 2);

        let mut cursor = CursorView::try_from(("hello world\n", 0)).unwrap();
        assert!(cursor.move_to_next_byte(b'l', 2));
        assert_eq!(cursor.byte_index(), 3);

        let mut cursor = CursorView::try_from(("hello world\n", 0)).unwrap();
        assert!(cursor.move_to_next_byte(b'l', 3));
        assert_eq!(cursor.byte_index(), 9);
    }

    #[test]
    fn move_to_next_byte_skips_match_under_cursor() {
        // Like Kakoune's `f`, the search starts after the grapheme the cursor occupies.
        let mut cursor = CursorView::try_from(("ll\n", 0)).unwrap();
        assert!(cursor.move_to_next_byte(b'l', 1));
        assert_eq!(cursor.byte_index(), 1);
    }

    #[test]
    fn move_to_next_blank_count_skips_previous_matches() {
        let mut cursor = CursorView::try_from(("a b\tc\n", 0)).unwrap();

        assert!(cursor.move_to_next_blank(1));
        assert_eq!(cursor.byte_index(), 1);

        let mut cursor = CursorView::try_from(("a b\tc\n", 0)).unwrap();
        assert!(cursor.move_to_next_blank(2));
        assert_eq!(cursor.byte_index(), 3);

        let mut cursor = CursorView::try_from(("a b\tc\n", 0)).unwrap();
        assert!(cursor.move_to_next_blank(3));
        assert_eq!(cursor.byte_index(), 5);
    }

    #[hegel::test(test_cases = 1000)]
    fn fuzz(tc: TestCase) {
        struct StateMachine {
            text: Text,
            state: CursorState,
        }
        #[hegel::state_machine]
        #[expect(clippy::needless_pass_by_value)]
        impl StateMachine {
            fn cursor(&mut self) -> CursorMut<'_> {
                CursorMut::new(&mut self.text, &mut self.state).expect("Cursor state kept valid")
            }
            #[rule]
            fn move_to(&mut self, tc: TestCase) {
                let byte_index = tc.draw(gs::integers::<usize>().max_value(self.text.len()));
                self.cursor().move_to(byte_index);
            }
            #[rule]
            fn move_left(&mut self, tc: TestCase) {
                let count = tc.draw(gs::integers::<usize>().min_value(1).max_value(100));
                self.cursor().move_left(count);
            }
            #[rule]
            fn move_right(&mut self, tc: TestCase) {
                let count = tc.draw(gs::integers::<usize>().min_value(1).max_value(100));
                self.cursor().move_right(count);
            }
            #[rule]
            fn move_up(&mut self, tc: TestCase) {
                let count = tc.draw(gs::integers::<usize>().min_value(1).max_value(100));
                let mut cursor = self.cursor();
                let goal_column = cursor.display_column();
                cursor.move_up(goal_column, count);
            }
            #[rule]
            fn move_down(&mut self, tc: TestCase) {
                let count = tc.draw(gs::integers::<usize>().min_value(1).max_value(100));
                let mut cursor = self.cursor();
                let goal_column = cursor.display_column();
                cursor.move_down(goal_column, count);
            }
            #[rule]
            fn move_to_prev_byte(&mut self, tc: TestCase) {
                let byte = tc.draw(gs::integers::<u8>());
                let count = tc.draw(gs::integers::<usize>().min_value(1).max_value(100));
                self.cursor().move_to_prev_byte(byte, count);
            }
            #[rule]
            fn move_to_next_byte(&mut self, tc: TestCase) {
                let byte = tc.draw(gs::integers::<u8>());
                let count = tc.draw(gs::integers::<usize>().min_value(1).max_value(100));
                self.cursor().move_to_next_byte(byte, count);
            }
            #[rule]
            fn move_to_prev_blank(&mut self, tc: TestCase) {
                let count = tc.draw(gs::integers::<usize>().min_value(1).max_value(100));
                self.cursor().move_to_prev_blank(count);
            }
            #[rule]
            fn move_to_next_blank(&mut self, tc: TestCase) {
                let count = tc.draw(gs::integers::<usize>().min_value(1).max_value(100));
                self.cursor().move_to_next_blank(count);
            }
            #[rule]
            fn move_to_start(&mut self, _: TestCase) {
                self.cursor().move_to_start();
            }
            #[rule]
            fn move_to_end(&mut self, _: TestCase) {
                self.cursor().move_to_end();
            }
            #[rule]
            fn move_to_bottom(&mut self, _: TestCase) {
                self.cursor().move_to_bottom();
            }
            #[rule]
            fn move_to_line_start(&mut self, _: TestCase) {
                self.cursor().move_to_line_start();
            }
            #[rule]
            fn move_to_line_non_blank_start(&mut self, _: TestCase) {
                self.cursor().move_to_line_non_blank_start();
            }
            #[rule]
            fn move_to_line_end(&mut self, _: TestCase) {
                self.cursor().move_to_line_end();
            }
            #[rule]
            fn move_until_line_end(&mut self, _: TestCase) {
                self.cursor().move_until_line_end();
            }
            #[rule]
            fn insert_char(&mut self, tc: TestCase) {
                let text = tc.draw(gs::text().min_size(1));
                let c = text.chars().next().expect("min_size(1) yields a char");
                self.cursor().insert_char(c);
            }
            #[rule]
            fn insert(&mut self, tc: TestCase) {
                let text = tc.draw(gs::text());
                self.cursor().insert(&text);
            }
            #[rule]
            fn delete_before(&mut self, tc: TestCase) {
                let count = tc.draw(gs::integers::<usize>().min_value(1).max_value(100));
                let mut cursor = self.cursor();
                for _ in 0..count {
                    cursor.delete_before();
                }
            }
            #[rule]
            fn delete_after(&mut self, tc: TestCase) {
                let count = tc.draw(gs::integers::<usize>().min_value(1).max_value(100));
                let mut cursor = self.cursor();
                for _ in 0..count {
                    cursor.delete_after();
                }
            }
            #[invariant]
            fn invariants(&self, _: TestCase) {
                let _ = Cursor::new(&self.text, &self.state)
                    .expect("Cursor remains valid after every operation");
                let rope = self.text.rope();
                assert!(
                    rope.len() > 0 && rope.byte(rope.len() - 1) == b'\n',
                    "Text keeps its trailing newline"
                );
            }
        }
        let machine = StateMachine {
            text: Text::new(),
            state: CursorState::default(),
        };
        hegel::stateful::run(machine, tc);
    }
}
