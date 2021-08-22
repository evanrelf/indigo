use ropey::Rope;
use std::fs::File;
use std::io::BufReader;
use std::path::Path;

pub struct Selection {
    pub anchor_index: usize,
    pub cursor_index: usize,
}

impl Selection {
    pub fn new() -> Selection {
        Selection {
            anchor_index: 0,
            cursor_index: 0,
        }
    }
}

pub struct Buffer {
    pub contents: Rope,
    pub primary_selection_index: usize,
    pub selections: Vec<Selection>,
    pub viewport_lines_offset: usize,
    pub viewport_columns_offset: usize,
}

impl Buffer {
    pub fn new() -> Buffer {
        Buffer {
            contents: Rope::new(),
            primary_selection_index: 0,
            selections: vec![Selection::new()],
            viewport_lines_offset: 0,
            viewport_columns_offset: 0,
        }
    }

    pub fn from_file<P>(path: P) -> Buffer
    where
        P: AsRef<Path>,
    {
        Buffer {
            contents: Rope::from_reader(BufReader::new(File::open(path).unwrap())).unwrap(),
            primary_selection_index: 0,
            selections: vec![Selection::new()],
            viewport_lines_offset: 0,
            viewport_columns_offset: 0,
        }
    }

    pub fn index_to_coordinates(&self, index: usize) -> (usize, usize) {
        let line_line_index = self.contents.char_to_line(index);
        let line_char_index = self.contents.line_to_char(line_line_index);
        (line_line_index, index - line_char_index)
    }

    pub fn scroll_up(&mut self, distance: usize) {
        self.viewport_lines_offset = self.viewport_lines_offset.saturating_sub(distance);
    }

    pub fn scroll_down(&mut self, distance: usize) {
        let new_viewport_lines_offset = self.viewport_lines_offset.saturating_add(distance);
        if new_viewport_lines_offset <= self.contents.len_lines() {
            self.viewport_lines_offset = new_viewport_lines_offset;
        }
    }

    pub fn scroll_left(&mut self, distance: usize) {
        self.viewport_columns_offset = self.viewport_columns_offset.saturating_sub(distance);
    }

    pub fn scroll_right(&mut self, distance: usize) {
        self.viewport_columns_offset = self.viewport_columns_offset.saturating_add(distance);
    }

    pub fn move_cursor_left(&mut self, distance: usize) {
        for selection in &mut self.selections {
            selection.cursor_index = selection.cursor_index.saturating_sub(distance);
        }
    }

    pub fn move_cursor_right(&mut self, distance: usize) {
        for selection in &mut self.selections {
            selection.cursor_index = selection.cursor_index.saturating_add(distance);
        }
    }

    pub fn reduce_selection(&mut self) {
        for selection in &mut self.selections {
            selection.anchor_index = selection.cursor_index;
        }
    }
}
