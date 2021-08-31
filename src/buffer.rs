use crate::selection::Selection;
use ropey::Rope;
use std::fs::File;
use std::io::BufReader;
use std::path::Path;

pub struct Buffer {
    pub contents: Rope,

    pub selections: Vec<Selection>,
    pub primary_selection: usize,

    pub viewport_lines_offset: usize,
    pub viewport_columns_offset: usize,
}

impl Buffer {
    pub fn new() -> Buffer {
        Buffer {
            contents: Rope::new(),

            selections: vec![Selection::default()],
            primary_selection: 0,

            viewport_lines_offset: 0,
            viewport_columns_offset: 0,
        }
    }

    pub fn from_file<P>(path: P) -> Buffer
    where
        P: AsRef<Path>,
    {
        let file = File::open(path).unwrap();
        let reader = BufReader::new(file);

        Buffer {
            contents: Rope::from_reader(reader).unwrap(),

            selections: vec![Selection::default()],
            primary_selection: 0,

            viewport_lines_offset: 0,
            viewport_columns_offset: 0,
        }
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
}
