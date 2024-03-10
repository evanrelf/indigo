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
    pub fn move_to(&mut self, line: usize, column: usize) {
        self.cursor.line = line;
        self.cursor.column = column;
    }

    pub fn move_up(&mut self, distance: usize) {
        self.cursor.line = self.cursor.line.saturating_sub(distance);
    }

    pub fn move_down(&mut self, distance: usize) {
        self.cursor.line += distance;
    }

    pub fn move_left(&mut self, distance: usize) {
        self.cursor.column = self.cursor.column.saturating_sub(distance);
    }

    pub fn move_right(&mut self, distance: usize) {
        self.cursor.column += distance;
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
