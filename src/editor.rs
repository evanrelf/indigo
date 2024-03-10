use crate::{mode::Mode, position::Position, rope::RopeExt as _};
use ropey::Rope;
use std::cmp::{max, min};

#[derive(Debug, Default)]
pub struct Editor {
    pub text: Rope,
    pub cursor: Position,
    pub target_column: Option<usize>,
    pub scroll: Position,
    pub mode: Mode,
}

impl Editor {
    pub fn move_to(&mut self, line: usize, column: usize) -> anyhow::Result<()> {
        self.cursor.line = line;
        self.cursor.column = column;
        self.target_column = None;
        self.cursor.correct(&self.text)
    }

    pub fn move_up(&mut self, distance: usize) -> anyhow::Result<()> {
        self.target_column = match self.target_column {
            None => Some(self.cursor.column),
            Some(target_column) => Some(max(self.cursor.column, target_column)),
        };

        self.cursor.line = self.cursor.line.saturating_sub(distance);
        self.cursor.column = self.target_column.unwrap_or(self.cursor.column);
        self.cursor.correct(&self.text)?;

        if self.target_column.unwrap_or(0) <= self.cursor.column {
            self.target_column = None;
        }

        Ok(())
    }

    pub fn move_down(&mut self, distance: usize) -> anyhow::Result<()> {
        self.target_column = match self.target_column {
            None => Some(self.cursor.column),
            Some(target_column) => Some(max(self.cursor.column, target_column)),
        };

        let last_line = self.text.len_lines_indigo().saturating_sub(1);

        self.cursor.line = min(last_line, self.cursor.line + distance);
        self.cursor.column = self.target_column.unwrap_or(self.cursor.column);
        self.cursor.correct(&self.text)?;

        if self.target_column.unwrap_or(0) <= self.cursor.column {
            self.target_column = None;
        }

        Ok(())
    }

    pub fn move_left(&mut self, distance: usize) -> anyhow::Result<()> {
        self.cursor = self
            .cursor
            .via_char_index(&self.text, |index| index.saturating_sub(distance))?;
        self.target_column = None;
        Ok(())
    }

    pub fn move_right(&mut self, distance: usize) -> anyhow::Result<()> {
        self.cursor = self
            .cursor
            .via_char_index(&self.text, |index| index + distance)?;
        self.target_column = None;
        Ok(())
    }

    pub fn scroll_to(&mut self, line: usize, column: usize) {
        let last_line = self.text.len_lines_indigo().saturating_sub(1);
        self.scroll.line = min(line, last_line);
        self.scroll.column = column;
    }

    pub fn scroll_up(&mut self, distance: usize) {
        self.scroll_to(
            self.scroll.line.saturating_sub(distance),
            self.scroll.column,
        );
    }

    pub fn scroll_down(&mut self, distance: usize) {
        self.scroll_to(self.scroll.line + distance, self.scroll.column);
    }

    pub fn scroll_left(&mut self, distance: usize) {
        self.scroll_to(
            self.scroll.line,
            self.scroll.column.saturating_sub(distance),
        );
    }

    pub fn scroll_right(&mut self, distance: usize) {
        self.scroll_to(self.scroll.line, self.scroll.column + distance);
    }

    pub fn insert_char(&mut self, c: char) -> anyhow::Result<()> {
        let index = self.cursor.to_char_index(&self.text)?;

        self.text.insert_char(index, c);

        self.cursor = Position::from_char_index(index + 1, &self.text)?;

        Ok(())
    }

    pub fn backspace(&mut self) -> anyhow::Result<()> {
        let index = self.cursor.to_char_index(&self.text)?;

        if index == 0 {
            return Ok(());
        }

        self.text.remove(index - 1..index);

        self.cursor = Position::from_char_index(index - 1, &self.text)?;

        Ok(())
    }
}
