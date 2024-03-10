use crate::{position::Position, rope::RopeExt as _};
use ropey::Rope;
use std::cmp::min;

#[derive(Debug, Default)]
pub struct Editor {
    pub text: Rope,
    pub cursor: Position,
    pub scroll: Position,
}

impl Editor {
    pub fn move_to(&mut self, line: usize, column: usize) -> anyhow::Result<()> {
        self.cursor.line = line;
        self.cursor.column = column;
        self.cursor.correct(&self.text)
    }

    pub fn move_up(&mut self, distance: usize) -> anyhow::Result<()> {
        self.move_to(
            self.cursor.line.saturating_sub(distance),
            self.cursor.column,
        )
    }

    pub fn move_down(&mut self, distance: usize) -> anyhow::Result<()> {
        self.move_to(self.cursor.line + distance, self.cursor.column)
    }

    pub fn move_left(&mut self, distance: usize) -> anyhow::Result<()> {
        let index = self
            .cursor
            .to_char_index(&self.text)?
            .saturating_sub(distance);
        let position = Position::from_char_index(index, &self.text)?;
        self.cursor = position;
        Ok(())
    }

    pub fn move_right(&mut self, distance: usize) -> anyhow::Result<()> {
        let index = self.cursor.to_char_index(&self.text)? + distance;
        let position = Position::from_char_index(index, &self.text)?;
        self.cursor = position;
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
}
