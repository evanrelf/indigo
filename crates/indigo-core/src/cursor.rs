use crate::{display_width::DisplayWidth, ot::EditSeq, rope::RopeExt as _, text::Text};
use indigo_wrap::{WBox, WMut, WRef, Wrap, WrapMut, WrapRef};
use ropey::{Rope, RopeSlice};
use std::thread;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum Error {
    #[error("Char offset {char_offset} exceeds end of text at {end_offset}")]
    ExceedsEnd {
        char_offset: usize,
        end_offset: usize,
    },

    #[error("Char offset {char_offset} does not lie on grapheme boundary")]
    NotOnGraphemeBoundary { char_offset: usize },
}

#[derive(Clone, Debug, Default)]
pub struct CursorState {
    /// Gap (offset) in text, counting by Unicode scalar values (`char`s).
    pub char_offset: usize,
}

impl CursorState {
    /// Snap to nearest grapheme boundary. This is a no-op if the cursor is already valid.
    pub fn snap(&mut self, text: &Rope) {
        self.char_offset = text.ceil_grapheme_boundary(self.char_offset);
    }

    #[must_use]
    pub fn snapped(mut self, text: &Rope) -> Self {
        self.snap(text);
        self
    }

    #[must_use]
    pub fn column(&self, text: &Rope) -> usize {
        let current_line_index = text.char_to_line(self.char_offset);
        let current_line_char_index = text.line_to_char(current_line_index);
        text.slice(current_line_char_index..self.char_offset)
            .display_width()
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
    pub fn char_offset(&self) -> usize {
        self.state.char_offset
    }

    #[must_use]
    pub fn column(&self) -> usize {
        self.state.column(&self.text)
    }

    #[must_use]
    pub fn grapheme(&self) -> Option<RopeSlice<'_>> {
        let start = self.state.char_offset;
        let end = self.text.next_grapheme_boundary(start)?;
        Some(self.text.slice(start..end))
    }

    #[must_use]
    pub fn is_at_start(&self) -> bool {
        self.state.char_offset == 0
    }

    #[must_use]
    pub fn is_at_end(&self) -> bool {
        self.state.char_offset == self.text.len_chars()
    }

    pub(crate) fn assert_invariants(&self) -> anyhow::Result<()> {
        if self.state.char_offset > self.text.len_chars() {
            anyhow::bail!(Error::ExceedsEnd {
                char_offset: self.state.char_offset,
                end_offset: self.text.len_chars(),
            });
        }
        if !self.text.is_grapheme_boundary(self.state.char_offset) {
            anyhow::bail!(Error::NotOnGraphemeBoundary {
                char_offset: self.state.char_offset,
            });
        }
        Ok(())
    }
}

impl<W: WrapMut> CursorView<'_, W> {
    pub fn snap(&mut self) {
        self.state.snap(&self.text);
    }

    pub fn move_to(&mut self, char_offset: usize) {
        assert!(self.text.is_grapheme_boundary(char_offset));
        self.state.char_offset = char_offset;
    }

    pub fn move_left(&mut self, count: usize) -> bool {
        for _ in 0..count {
            if let Some(prev) = self.text.prev_grapheme_boundary(self.state.char_offset)
                && self.state.char_offset != prev
            {
                self.state.char_offset = prev;
            } else {
                return false;
            }
        }
        count > 0
    }

    pub fn move_right(&mut self, count: usize) -> bool {
        for _ in 0..count {
            if let Some(next) = self.text.next_grapheme_boundary(self.state.char_offset)
                && self.state.char_offset != next
            {
                self.state.char_offset = next;
            } else {
                return false;
            }
        }
        count > 0
    }

    pub fn move_up(&mut self, goal_column: usize, count: usize) -> bool {
        for _ in 0..count {
            let current_line_index = self.text.char_to_line(self.state.char_offset);
            if current_line_index == 0 {
                return false;
            }
            let target_line_index = current_line_index - 1;
            let target_line_char_index = self.text.line_to_char(target_line_index);
            let target_line_slice = self.text.line(target_line_index);
            let mut target_line_prefix = 0;
            let mut char_offset = target_line_char_index;
            for grapheme in target_line_slice.graphemes() {
                if grapheme.chars().any(|c| c == '\n' || c == '\r') {
                    break;
                }
                let grapheme_width = grapheme.display_width();
                if target_line_prefix + grapheme_width > goal_column {
                    break;
                }
                target_line_prefix += grapheme_width;
                char_offset += grapheme.len_chars();
            }
            self.state.char_offset = char_offset;
        }
        count > 0
    }

    pub fn move_down(&mut self, goal_column: usize, count: usize) -> bool {
        for _ in 0..count {
            let current_line_index = self.text.char_to_line(self.state.char_offset);
            let target_line_index = current_line_index + 1;
            if self.state.char_offset == self.text.len_chars() {
                return false;
            }
            if target_line_index >= self.text.len_lines_indigo() {
                self.state.char_offset = self.text.len_chars();
                return true;
            }
            let target_line_char_index = self.text.line_to_char(target_line_index);
            let target_line_slice = self.text.line(target_line_index);
            let mut target_line_prefix = 0;
            let mut char_offset = target_line_char_index;
            for grapheme in target_line_slice.graphemes() {
                if grapheme.chars().any(|c| c == '\n' || c == '\r') {
                    break;
                }
                let grapheme_width = grapheme.display_width();
                if target_line_prefix + grapheme_width > goal_column {
                    break;
                }
                target_line_prefix += grapheme_width;
                char_offset += grapheme.len_chars();
            }
            self.state.char_offset = char_offset;
        }
        count > 0
    }

    pub fn move_to_prev_byte(&mut self, byte: u8, count: usize) -> bool {
        for _ in 0..count {
            if let Some(char_offset) = self.text.find_prev_byte(..self.state.char_offset, byte) {
                self.state.char_offset = char_offset;
                self.move_right(1);
            } else {
                return false;
            }
        }
        count > 0
    }

    pub fn move_to_next_byte(&mut self, byte: u8, count: usize) -> bool {
        for _ in 0..count {
            if let Some(char_offset) = self.text.find_next_byte(self.state.char_offset.., byte) {
                self.state.char_offset = char_offset;
            } else {
                return false;
            }
        }
        count > 0
    }

    pub fn move_to_top(&mut self) {
        self.state.char_offset = 0;
    }

    pub fn move_to_bottom(&mut self) {
        self.state.char_offset = self.text.len_chars();
        self.move_to_line_start();
    }

    pub fn move_to_line_start(&mut self) {
        let current_line_index = self.text.char_to_line(self.state.char_offset);
        let line_start_char_offset = self.text.line_to_char(current_line_index);
        self.state.char_offset = line_start_char_offset;
    }

    pub fn move_to_line_non_blank_start(&mut self) {
        let current_line_index = self.text.char_to_line(self.state.char_offset);
        let line_start_char_offset = self.text.line_to_char(current_line_index);
        let line_slice = self.text.line(current_line_index);
        let mut char_offset = line_start_char_offset;
        for grapheme in line_slice.graphemes() {
            if grapheme.chars().any(|c| c == '\n' || c == '\r') {
                break;
            }
            if !grapheme.chars().all(|c| c.is_whitespace()) {
                self.state.char_offset = char_offset;
                return;
            }
            char_offset += grapheme.len_chars();
        }
        self.state.char_offset = char_offset;
    }

    pub fn move_to_line_end(&mut self) {
        let current_line_index = self.text.char_to_line(self.state.char_offset);
        let line_start_char_offset = self.text.line_to_char(current_line_index);
        let line_slice = self.text.line(current_line_index);
        let mut char_offset = line_start_char_offset;
        for grapheme in line_slice.graphemes() {
            if grapheme.chars().any(|c| c == '\n' || c == '\r') {
                break;
            }
            char_offset += grapheme.len_chars();
        }
        self.state.char_offset = char_offset;
    }

    pub fn insert_char(&mut self, char: char) -> EditSeq {
        self.insert(&char.to_string())
    }

    pub fn insert(&mut self, text: &str) -> EditSeq {
        self.assert_invariants().unwrap();
        let mut edits = EditSeq::new();
        edits.retain(self.state.char_offset);
        edits.insert(text);
        edits.retain_rest(&self.text);
        self.text.edit(&edits).expect("Edits are well formed");
        self.state.char_offset = edits.transform_char_offset(self.state.char_offset);
        self.snap();
        edits
    }

    // Behavior traditionally associated with the Backspace key.
    pub fn delete_before(&mut self) -> Option<EditSeq> {
        self.assert_invariants().unwrap();
        let mut char_offset = self.state.char_offset;
        if let Some(prev) = self.text.prev_grapheme_boundary(char_offset)
            && char_offset != prev
        {
            char_offset = prev;
        } else {
            return None;
        }
        let mut edits = EditSeq::new();
        edits.retain(char_offset);
        edits.delete(self.state.char_offset - char_offset);
        edits.retain_rest(&self.text);
        self.text.edit(&edits).expect("Edits are well formed");
        self.state.char_offset = edits.transform_char_offset(self.state.char_offset);
        self.snap();
        Some(edits)
    }

    // Behavior traditionally associated with the Delete key.
    pub fn delete_after(&mut self) -> Option<EditSeq> {
        self.assert_invariants().unwrap();
        let mut char_offset = self.state.char_offset;
        if let Some(next) = self.text.next_grapheme_boundary(char_offset)
            && char_offset != next
        {
            char_offset = next;
        } else {
            return None;
        }
        let mut edits = EditSeq::new();
        edits.retain(self.state.char_offset);
        edits.delete(char_offset - self.state.char_offset);
        edits.retain_rest(&self.text);
        self.text.edit(&edits).expect("Edits are well formed");
        self.state.char_offset = edits.transform_char_offset(self.state.char_offset);
        self.snap();
        Some(edits)
    }
}

impl<R> TryFrom<(R, usize)> for CursorView<'_, WBox>
where
    R: Into<Text>,
{
    type Error = anyhow::Error;
    fn try_from((text, char_offset): (R, usize)) -> anyhow::Result<Self> {
        let text = Box::new(text.into());
        let state = Box::new(CursorState { char_offset });
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
    fn insert_changes_grapheme_boundary() {
        // combining acute accent (´)
        let mut cursor = CursorView::try_from(("\u{0301}", 0)).unwrap();
        cursor.insert("e");
        cursor.assert_invariants().unwrap();
    }

    #[test]
    fn expected_behavior_left_right() {
        let mut cursor = CursorView::try_from(("", 0)).unwrap();
        cursor.assert_invariants().unwrap();

        cursor.insert("hello");
        assert_eq!(**cursor.text, "hello");
        assert_eq!(cursor.char_offset(), 5);

        cursor.delete_before();
        cursor.delete_before();
        assert_eq!(**cursor.text, "hel");
        assert_eq!(cursor.char_offset(), 3);

        cursor.move_left(2);
        assert_eq!(cursor.char_offset(), 1);

        cursor.delete_after();
        assert_eq!(cursor.char_offset(), 1);
        assert_eq!(**cursor.text, "hl");

        cursor.move_right(1);
        assert_eq!(cursor.char_offset(), 2);
        cursor.move_right(1);
        assert_eq!(cursor.char_offset(), 2);

        cursor.delete_after();
        assert_eq!(**cursor.text, "hl");
        assert_eq!(cursor.char_offset(), 2);

        cursor.delete_before();
        cursor.delete_before();
        assert_eq!(**cursor.text, "");
        assert_eq!(cursor.char_offset(), 0);

        cursor.delete_before();
        assert_eq!(**cursor.text, "");
        assert_eq!(cursor.char_offset(), 0);
    }

    #[test]
    fn expected_behavior_up_down() {
        let mut cursor = CursorView::try_from(("", 0)).unwrap();
        cursor.assert_invariants().unwrap();

        let text = "0\n234\n6789AB\n";
        cursor.insert(text);
        // At end of text.
        assert_eq!(cursor.grapheme(), None);
        assert_eq!(cursor.char_offset(), 13);
        assert_eq!(cursor.char_offset(), text.chars().count());
        assert_eq!(cursor.column(), 0);

        cursor.move_left(1);
        // At final newline on line 2.
        assert_eq!(cursor.grapheme(), Some(Rope::from("\n").slice(..)));
        assert_eq!(cursor.char_offset(), 12);
        assert_eq!(cursor.column(), 6);

        cursor.move_up(cursor.column(), 1);
        // At second newline on line 1, which is shorter than the goal column.
        assert_eq!(cursor.grapheme(), Some(Rope::from("\n").slice(..)));
        assert_eq!(cursor.char_offset(), 5);
        // Goal column should remain the same through vertical movement.
        assert_eq!(cursor.column(), 3);

        cursor.move_left(1);
        // At "4" on line 1.
        assert_eq!(cursor.grapheme(), Some(Rope::from("4").slice(..)));
        assert_eq!(cursor.char_offset(), 4);
        // Goal column should change through horizontal movement.
        assert_eq!(cursor.column(), 2);
    }

    #[test]
    fn fuzz() {
        arbtest(|u| {
            let text = Text::new();
            let char_offset = if u.arbitrary::<bool>()? {
                text.floor_grapheme_boundary(u.arbitrary::<usize>()?)
            } else {
                text.ceil_grapheme_boundary(u.arbitrary::<usize>()?)
            };
            let mut cursor = CursorView::try_from((text, char_offset)).unwrap();
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
                        cursor.move_up(cursor.column(), count);
                        tx.send(format!("move_left() x{count}")).unwrap();
                    }
                    3 => {
                        let count = max(1, u.choose_index(99)?);
                        cursor.move_down(cursor.column(), count);
                        tx.send(format!("move_right() x{count}")).unwrap();
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
