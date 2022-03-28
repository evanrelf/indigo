use crate::{cursor::Cursor, range::Range};
use std::ops::{Deref, DerefMut};

#[derive(Default)]
pub struct Rope {
    pub ropey: ropey::Rope,
}

impl Rope {
    pub fn cursor_to_index(&self, cursor: &Cursor) -> Option<usize> {
        let line_index = self.try_line_to_char(cursor.line).ok()?;
        let line_length = self.get_line(cursor.line)?.len_chars();
        if line_length > cursor.column {
            Some(line_index + cursor.column)
        } else {
            None
        }
    }

    pub fn index_to_cursor(&self, index: usize) -> Option<Cursor> {
        let line = self.try_char_to_line(index).ok()?;
        let column = index - self.try_line_to_char(line).ok()?;
        Some(Cursor {
            line,
            column,
            target_column: None,
        })
    }

    pub fn is_valid_cursor(&self, cursor: &Cursor) -> bool {
        self.cursor_to_index(cursor).is_some()
    }

    pub fn range_to_slice(&self, range: &Range) -> Option<ropey::RopeSlice> {
        let anchor_index = self.cursor_to_index(&range.anchor)?;
        let head_index = self.cursor_to_index(&range.head)?;
        if range.is_forwards() {
            self.get_slice(anchor_index..=head_index)
        } else {
            self.get_slice(head_index..=anchor_index)
        }
    }

    pub fn corrected_cursor(&self, cursor: &Cursor) -> Option<Cursor> {
        let line_length = self.get_line(cursor.line)?.len_chars();
        if line_length == 0 {
            return None;
        }

        #[allow(clippy::collapsible_else_if)]
        if let Some(target_column) = cursor.target_column {
            if line_length > target_column {
                Some(Cursor {
                    column: target_column,
                    target_column: None,
                    ..cursor.clone()
                })
            } else {
                Some(Cursor {
                    column: line_length - 1,
                    ..cursor.clone()
                })
            }
        } else {
            if line_length > cursor.column {
                Some(cursor.clone())
            } else {
                Some(Cursor {
                    column: line_length - 1,
                    target_column: Some(cursor.column),
                    ..cursor.clone()
                })
            }
        }
    }

    pub fn corrected_range(&self, range: &Range) -> Option<Range> {
        let anchor = self.corrected_cursor(&range.anchor)?;
        let head = self.corrected_cursor(&range.head)?;
        Some(Range { anchor, head })
    }
}

impl From<ropey::Rope> for Rope {
    fn from(ropey: ropey::Rope) -> Self {
        Self { ropey }
    }
}

impl Deref for Rope {
    type Target = ropey::Rope;

    fn deref(&self) -> &Self::Target {
        &self.ropey
    }
}

impl DerefMut for Rope {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.ropey
    }
}
