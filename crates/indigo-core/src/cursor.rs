use crate::{
    display_width::DisplayWidth,
    ot::OperationSeq,
    rope::{LINE_TYPE, RopeExt as _},
    text::Text,
};
use indigo_wrap::{WBox, WMut, WRef, Wrap, WrapMut, WrapRef};
use ropey::{Rope, RopeSlice};
use std::thread;
use thiserror::Error;

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

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Bias {
    Before,
    After,
}

#[derive(Clone, Debug, Default)]
pub struct CursorState {
    pub byte_offset: usize,
}

impl CursorState {
    /// Snap to nearest grapheme boundary. This is a no-op if the cursor is already valid.
    pub fn snap_to_grapheme_boundary(&mut self, text: &Rope) {
        self.byte_offset = text.ceil_grapheme_boundary(self.byte_offset);
    }

    #[must_use]
    pub fn snapped_to_grapheme_boundary(mut self, text: &Rope) -> Self {
        self.snap_to_grapheme_boundary(text);
        self
    }

    pub fn transform(&mut self, ops: &OperationSeq) {
        self.byte_offset = ops.transform_byte_offset(self.byte_offset);
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

    #[must_use]
    pub fn display_column(&self, bias: Bias) -> Option<usize> {
        let Ok(byte_index) = self.byte_index(bias) else {
            return None;
        };
        Some(self.text.display_column(byte_index))
    }

    #[must_use]
    pub fn grapheme(&self, bias: Bias) -> Option<RopeSlice<'_>> {
        let Ok(byte_index) = self.byte_index(bias) else {
            return None;
        };
        let start = byte_index;
        let end = self.text.next_grapheme_boundary(start)?;
        Some(self.text.slice(start..end))
    }

    #[must_use]
    pub fn line(&self, bias: Bias) -> Option<RopeSlice<'_>> {
        let byte_index = self.byte_index(bias).ok()?;
        let line_index = self.text.byte_to_line_idx(byte_index, LINE_TYPE);
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

    pub(crate) fn assert_invariants(&self) -> anyhow::Result<()> {
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
    pub fn snap_to_grapheme_boundary(&mut self) {
        self.state.snap_to_grapheme_boundary(&self.text);
    }

    pub fn move_to(&mut self, byte_offset: usize) {
        assert!(self.text.is_grapheme_boundary(byte_offset));
        self.state.byte_offset = byte_offset;
    }

    pub fn move_left(&mut self, count: usize) -> bool {
        for _ in 0..count {
            if let Some(prev) = self.text.prev_grapheme_boundary(self.state.byte_offset)
                && self.state.byte_offset != prev
            {
                self.state.byte_offset = prev;
            } else {
                return false;
            }
        }
        count > 0
    }

    pub fn move_right(&mut self, count: usize) -> bool {
        for _ in 0..count {
            if let Some(next) = self.text.next_grapheme_boundary(self.state.byte_offset)
                && self.state.byte_offset != next
            {
                self.state.byte_offset = next;
            } else {
                return false;
            }
        }
        count > 0
    }

    pub fn move_up(&mut self, goal_column: usize, bias: Bias, count: usize) -> bool {
        if self.text.len() == 0 {
            return false;
        }
        for _ in 0..count {
            #[expect(clippy::manual_let_else)]
            let byte_index = match self.byte_index(bias) {
                Ok(n) | Err(Some(n)) => n,
                Err(None) => unreachable!("Already checked rope length"),
            };
            let current_line_index = self.text.byte_to_line_idx(byte_index, LINE_TYPE);
            if current_line_index == 0 {
                return false;
            }
            let target_line_index = current_line_index - 1;
            let target_line_byte_index = self.text.line_to_byte_idx(target_line_index, LINE_TYPE);
            let target_line_slice = self.text.line(target_line_index, LINE_TYPE);
            let mut target_line_prefix = 0;
            let mut byte_offset = target_line_byte_index;
            for grapheme in target_line_slice.graphemes() {
                if grapheme.chars().any(|c| c == '\n' || c == '\r') {
                    break;
                }
                let grapheme_width = grapheme.display_width();
                if target_line_prefix + grapheme_width > goal_column {
                    break;
                }
                target_line_prefix += grapheme_width;
                byte_offset += grapheme.len();
            }
            self.state.byte_offset = byte_offset;
        }
        count > 0
    }

    pub fn move_down(&mut self, goal_column: usize, bias: Bias, count: usize) -> bool {
        if self.text.len() == 0 {
            return false;
        }
        for _ in 0..count {
            #[expect(clippy::manual_let_else)]
            let byte_index = match self.byte_index(bias) {
                Ok(n) | Err(Some(n)) => n,
                Err(None) => unreachable!("Already checked rope length"),
            };
            let current_line_index = self.text.byte_to_line_idx(byte_index, LINE_TYPE);
            let target_line_index = current_line_index + 1;
            if self.state.byte_offset == self.text.len() {
                return false;
            }
            if target_line_index >= self.text.len_lines_indigo() {
                self.state.byte_offset = self.text.len();
                return true;
            }
            let target_line_byte_index = self.text.line_to_byte_idx(target_line_index, LINE_TYPE);
            let target_line_slice = self.text.line(target_line_index, LINE_TYPE);
            let mut target_line_prefix = 0;
            let mut byte_offset = target_line_byte_index;
            for grapheme in target_line_slice.graphemes() {
                if grapheme.chars().any(|c| c == '\n' || c == '\r') {
                    break;
                }
                let grapheme_width = grapheme.display_width();
                if target_line_prefix + grapheme_width > goal_column {
                    break;
                }
                target_line_prefix += grapheme_width;
                byte_offset += grapheme.len();
            }
            self.state.byte_offset = byte_offset;
        }
        count > 0
    }

    pub fn move_to_prev_byte(&mut self, byte: u8, count: usize) -> bool {
        for _ in 0..count {
            if let Some(byte_offset) = self.text.find_prev_byte(..self.state.byte_offset, &[byte]) {
                self.state.byte_offset = byte_offset;
                self.move_right(1);
            } else {
                return false;
            }
        }
        count > 0
    }

    pub fn move_to_next_byte(&mut self, byte: u8, count: usize) -> bool {
        for _ in 0..count {
            if let Some(byte_offset) = self.text.find_next_byte(self.state.byte_offset.., &[byte]) {
                self.state.byte_offset = byte_offset;
            } else {
                return false;
            }
        }
        count > 0
    }

    pub fn move_to_prev_blank(&mut self, count: usize) -> bool {
        const BYTES: &[u8] = b" \t\n\r";
        for _ in 0..count {
            if let Some(byte_offset) = self.text.find_prev_byte(..self.state.byte_offset, BYTES) {
                self.state.byte_offset = byte_offset;
                self.move_right(1);
            } else {
                return false;
            }
        }
        count > 0
    }

    pub fn move_to_next_blank(&mut self, count: usize) -> bool {
        const BYTES: &[u8] = b" \t\n\r";
        for _ in 0..count {
            if let Some(byte_offset) = self.text.find_next_byte(self.state.byte_offset.., BYTES) {
                self.state.byte_offset = byte_offset;
            } else {
                return false;
            }
        }
        count > 0
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

        #[expect(clippy::manual_let_else)]
        let byte_index = match self.byte_index(bias) {
            Ok(n) | Err(Some(n)) => n,
            Err(None) => unreachable!("Already checked rope length"),
        };

        let line_index = self.text.byte_to_line_idx(byte_index, LINE_TYPE);
        let line_start_byte_offset = self.text.line_to_byte_idx(line_index, LINE_TYPE);
        self.state.byte_offset = line_start_byte_offset;
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

    pub fn move_to_line_end(&mut self, bias: Bias) {
        if self.text.len() == 0 || self.is_at_end() {
            return;
        }

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
            return;
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
    }

    pub fn insert_char(&mut self, char: char) -> OperationSeq {
        self.insert(&char.to_string())
    }

    pub fn insert(&mut self, text: &str) -> OperationSeq {
        self.assert_invariants().unwrap();
        let mut ops = OperationSeq::new();
        ops.retain(self.state.byte_offset);
        ops.insert(text);
        ops.retain_rest(&self.text);
        self.text.apply(&ops).expect("Operations are well formed");
        self.state.byte_offset = ops.transform_byte_offset(self.state.byte_offset);
        self.snap_to_grapheme_boundary();
        ops
    }

    // Behavior traditionally associated with the Backspace key.
    pub fn delete_before(&mut self) -> Option<OperationSeq> {
        self.assert_invariants().unwrap();
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
        self.state.byte_offset = ops.transform_byte_offset(self.state.byte_offset);
        self.snap_to_grapheme_boundary();
        Some(ops)
    }

    // Behavior traditionally associated with the Delete key.
    pub fn delete_after(&mut self) -> Option<OperationSeq> {
        self.assert_invariants().unwrap();
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
        self.state.byte_offset = ops.transform_byte_offset(self.state.byte_offset);
        self.snap_to_grapheme_boundary();
        Some(ops)
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
                        tx.send(format!("move_left() x{count}")).unwrap();
                    }
                    3 => {
                        let count = max(1, u.choose_index(99)?);
                        cursor.move_down(cursor.display_column(bias).unwrap_or(0), bias, count);
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
