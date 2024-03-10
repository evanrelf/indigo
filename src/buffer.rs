use crate::{position::Position, rope::RopeExt as _};
use ropey::Rope;
use std::cmp::{max, min};

#[derive(Debug)]
pub struct Buffer {
    pub text: Rope,
    pub anchor: Position,
    pub cursor: Position,
    pub target_column: Option<usize>,
}

impl Buffer {
    pub fn start(&self) -> Position {
        min(self.anchor, self.cursor)
    }

    pub fn end(&self) -> Position {
        max(self.anchor, self.cursor)
    }

    pub fn is_backward(&self) -> bool {
        self.anchor > self.cursor
    }

    pub fn is_forward(&self) -> bool {
        self.anchor <= self.cursor
    }

    pub fn flip(&mut self) {
        std::mem::swap(&mut self.anchor, &mut self.cursor);
    }

    pub fn flip_backward(&mut self) {
        if self.is_forward() {
            self.flip();
        }
    }

    pub fn flip_forward(&mut self) {
        if self.is_backward() {
            self.flip();
        }
    }

    pub fn reduce(&mut self) {
        self.anchor = self.cursor;
    }

    pub fn extend_to(&mut self, line: usize, column: usize) -> anyhow::Result<()> {
        self.cursor.line = line;
        self.cursor.column = column;
        self.target_column = None;
        self.cursor.correct(&self.text)
    }

    pub fn extend_up(&mut self, distance: usize) -> anyhow::Result<()> {
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

    pub fn extend_down(&mut self, distance: usize) -> anyhow::Result<()> {
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

    pub fn extend_left(&mut self, distance: usize) -> anyhow::Result<()> {
        self.cursor = self
            .cursor
            .via_char_index(&self.text, |index| index.saturating_sub(distance))?;
        self.target_column = None;
        Ok(())
    }

    pub fn extend_right(&mut self, distance: usize) -> anyhow::Result<()> {
        self.cursor = self
            .cursor
            .via_char_index(&self.text, |index| index + distance)?;
        self.target_column = None;
        Ok(())
    }

    pub fn move_to(&mut self, line: usize, column: usize) -> anyhow::Result<()> {
        self.extend_to(line, column)?;
        self.reduce();
        Ok(())
    }

    pub fn move_up(&mut self, distance: usize) -> anyhow::Result<()> {
        self.extend_up(distance)?;
        self.reduce();
        Ok(())
    }

    pub fn move_down(&mut self, distance: usize) -> anyhow::Result<()> {
        self.extend_down(distance)?;
        self.reduce();
        Ok(())
    }

    pub fn move_left(&mut self, distance: usize) -> anyhow::Result<()> {
        self.extend_left(distance)?;
        self.reduce();
        Ok(())
    }

    pub fn move_right(&mut self, distance: usize) -> anyhow::Result<()> {
        self.extend_right(distance)?;
        self.reduce();
        Ok(())
    }

    pub fn insert_char(&mut self, char: char) -> anyhow::Result<()> {
        let index = self.cursor.to_char_index(&self.text)?;

        self.text.insert_char(index, char);

        self.cursor = Position::from_char_index(index + 1, &self.text)?;

        Ok(())
    }

    pub fn insert(&mut self, str: &str) -> anyhow::Result<()> {
        let index = self.cursor.to_char_index(&self.text)?;

        self.text.insert(index, str);

        self.cursor = Position::from_char_index(index + str.len(), &self.text)?;

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

impl Default for Buffer {
    fn default() -> Self {
        Self {
            text: Rope::from("\n"),
            anchor: Position { line: 0, column: 0 },
            cursor: Position { line: 0, column: 0 },
            target_column: None,
        }
    }
}
