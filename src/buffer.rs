use crate::{rope::RopeExt as _, selection::Selection};
use ropey::Rope;
use std::{cmp::min, path::PathBuf};

#[derive(Clone, Default)]
pub struct Buffer {
    path: PathBuf,
    contents: Rope,
    selection: Selection,
    is_modified: bool,
    is_read_only: bool,
    vertical_scroll: usize,
    horizontal_scroll: usize,
}

impl Buffer {
    pub fn scroll_up(&self, distance: usize) -> Self {
        self.scroll_to_line(self.vertical_scroll.saturating_sub(distance))
    }

    pub fn scroll_down(&self, distance: usize) -> Self {
        self.scroll_to_line(self.vertical_scroll + distance)
    }

    pub fn scroll_left(&self, distance: usize) -> Self {
        self.scroll_to_column(self.horizontal_scroll.saturating_sub(distance))
    }

    pub fn scroll_right(&self, distance: usize) -> Self {
        self.scroll_to_column(self.horizontal_scroll + distance)
    }

    pub fn scroll_to_line(&self, line: usize) -> Self {
        let last_line = self.contents.len_lines_indigo().saturating_sub(1);
        Self {
            vertical_scroll: min(line, last_line),
            ..self.clone()
        }
    }

    pub fn scroll_to_column(&self, column: usize) -> Self {
        Self {
            horizontal_scroll: column,
            ..self.clone()
        }
    }

    pub fn is_valid(&self) -> bool {
        todo!()
    }
}
