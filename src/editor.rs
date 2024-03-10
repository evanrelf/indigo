use crate::rope::RopeExt as _;
use ropey::Rope;
use std::cmp::min;

#[derive(Debug, Default)]
pub struct Editor {
    pub text: Rope,
    pub cursor: (usize, usize),
    pub scroll: (usize, usize),
}

impl Editor {
    pub fn move_to(&mut self, line: usize, column: usize) {
        let (cursor_line, cursor_column) = &mut self.cursor;
        *cursor_line = line;
        *cursor_column = column;
    }

    pub fn move_up(&mut self, distance: usize) {
        let (cursor_line, _cursor_column) = &mut self.cursor;
        *cursor_line = cursor_line.saturating_sub(distance);
    }

    pub fn move_down(&mut self, distance: usize) {
        let (cursor_line, _cursor_column) = &mut self.cursor;
        *cursor_line += distance;
    }

    pub fn move_left(&mut self, distance: usize) {
        let (_cursor_line, cursor_column) = &mut self.cursor;
        *cursor_column = cursor_column.saturating_sub(distance);
    }

    pub fn move_right(&mut self, distance: usize) {
        let (_cursor_line, cursor_column) = &mut self.cursor;
        *cursor_column += distance;
    }

    pub fn scroll_to(&mut self, line: usize, column: usize) {
        let (scroll_line, scroll_column) = &mut self.scroll;
        let last_line = self.text.len_lines_indigo().saturating_sub(1);
        *scroll_line = min(line, last_line);
        *scroll_column = column;
    }

    pub fn scroll_up(&mut self, distance: usize) {
        let (scroll_line, scroll_column) = self.scroll;
        self.scroll_to(scroll_line.saturating_sub(distance), scroll_column);
    }

    pub fn scroll_down(&mut self, distance: usize) {
        let (scroll_line, scroll_column) = self.scroll;
        self.scroll_to(scroll_line + distance, scroll_column);
    }

    pub fn scroll_left(&mut self, distance: usize) {
        let (scroll_line, scroll_column) = self.scroll;
        self.scroll_to(scroll_line, scroll_column.saturating_sub(distance));
    }

    pub fn scroll_right(&mut self, distance: usize) {
        let (scroll_line, scroll_column) = self.scroll;
        self.scroll_to(scroll_line, scroll_column + distance);
    }
}
