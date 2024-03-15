use crate::{Position, RopeExt as _, Selection};
use ropey::Rope;
use std::cmp::{max, min};

// TODO: Create new `Text` type wrapping `Rope` that handles inserting and deleting via operations.
// `insert_char` and `backspace` will be dropped. The former is a special case of inserting a
// string, the latter is too TUI-related; `remove` is the more general primitive.
//
// `Buffer` becomes responsible _only_ for things that relate to the `Text` _and_ the `Selection`,
// such as cursor+anchor movement, inserting and deleting with implicit indexing/positioning,
// modifying selection with a regular expression, etc.

// TODO: I guess undo/redo history (for text and selection) should live in the buffer? But not file
// path, since it could still be a virtual buffer like scratch or the command-line or whatever.

#[derive(Debug)]
pub struct Buffer {
    pub text: Rope,
    pub selection: Selection,
}

impl Buffer {
    pub fn extend_to(&mut self, line: usize, column: usize) -> anyhow::Result<()> {
        self.selection.cursor.line = line;
        self.selection.cursor.column = column;
        self.selection.target_column = None;
        self.selection.cursor.correct(&self.text)
    }

    pub fn extend_up(&mut self, distance: usize) -> anyhow::Result<()> {
        self.selection.target_column = match self.selection.target_column {
            None => Some(self.selection.cursor.column),
            Some(target_column) => Some(max(self.selection.cursor.column, target_column)),
        };

        self.selection.cursor.line = self.selection.cursor.line.saturating_sub(distance);
        self.selection.cursor.column = self
            .selection
            .target_column
            .unwrap_or(self.selection.cursor.column);
        self.selection.cursor.correct(&self.text)?;

        if self.selection.target_column.unwrap_or(0) <= self.selection.cursor.column {
            self.selection.target_column = None;
        }

        Ok(())
    }

    pub fn extend_down(&mut self, distance: usize) -> anyhow::Result<()> {
        self.selection.target_column = match self.selection.target_column {
            None => Some(self.selection.cursor.column),
            Some(target_column) => Some(max(self.selection.cursor.column, target_column)),
        };

        let last_line = self.text.len_lines_indigo().saturating_sub(1);

        self.selection.cursor.line = min(last_line, self.selection.cursor.line + distance);
        self.selection.cursor.column = self
            .selection
            .target_column
            .unwrap_or(self.selection.cursor.column);
        self.selection.cursor.correct(&self.text)?;

        if self.selection.target_column.unwrap_or(0) <= self.selection.cursor.column {
            self.selection.target_column = None;
        }

        Ok(())
    }

    pub fn extend_left(&mut self, distance: usize) -> anyhow::Result<()> {
        self.selection.cursor = self
            .selection
            .cursor
            .via_char_index(&self.text, |index| index.saturating_sub(distance))?;
        self.selection.target_column = None;
        Ok(())
    }

    pub fn extend_right(&mut self, distance: usize) -> anyhow::Result<()> {
        self.selection.cursor = self
            .selection
            .cursor
            .via_char_index(&self.text, |index| index + distance)?;
        self.selection.target_column = None;
        Ok(())
    }

    pub fn move_to(&mut self, line: usize, column: usize) -> anyhow::Result<()> {
        self.extend_to(line, column)?;
        self.selection.reduce();
        Ok(())
    }

    pub fn move_up(&mut self, distance: usize) -> anyhow::Result<()> {
        self.extend_up(distance)?;
        self.selection.reduce();
        Ok(())
    }

    pub fn move_down(&mut self, distance: usize) -> anyhow::Result<()> {
        self.extend_down(distance)?;
        self.selection.reduce();
        Ok(())
    }

    pub fn move_left(&mut self, distance: usize) -> anyhow::Result<()> {
        self.extend_left(distance)?;
        self.selection.reduce();
        Ok(())
    }

    pub fn move_right(&mut self, distance: usize) -> anyhow::Result<()> {
        self.extend_right(distance)?;
        self.selection.reduce();
        Ok(())
    }

    pub fn insert_char(&mut self, char: char) -> anyhow::Result<()> {
        let index = self.selection.cursor.to_char_index(&self.text)?;

        self.text.insert_char(index, char);

        self.selection.cursor = Position::from_char_index(index + 1, &self.text)?;

        Ok(())
    }

    pub fn insert(&mut self, str: &str) -> anyhow::Result<()> {
        let index = self.selection.cursor.to_char_index(&self.text)?;

        self.text.insert(index, str);

        self.selection.cursor = Position::from_char_index(index + str.len(), &self.text)?;

        Ok(())
    }

    pub fn backspace(&mut self) -> anyhow::Result<()> {
        let index = self.selection.cursor.to_char_index(&self.text)?;

        if index == 0 {
            return Ok(());
        }

        self.text.remove(index - 1..index);

        self.selection.cursor = Position::from_char_index(index - 1, &self.text)?;

        Ok(())
    }
}

impl Default for Buffer {
    fn default() -> Self {
        Self {
            text: Rope::from("\n"),
            selection: Selection::default(),
        }
    }
}
