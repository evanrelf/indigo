use ropey::Rope;
use std::fs::File;
use std::io::BufReader;
use std::path::Path;

pub struct Selection {
    anchor_index: usize,
    cursor_index: usize,
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
    primary_selection_index: usize,
    selections: Vec<Selection>,
    lines_offset: usize,
    columns_offset: usize,
}

impl Buffer {
    pub fn new() -> Buffer {
        Buffer {
            contents: Rope::new(),
            primary_selection_index: 0,
            selections: vec![Selection::new()],
            lines_offset: 0,
            columns_offset: 0,
        }
    }

    pub fn from_file<P>(path: P) -> Buffer
    where
        P: AsRef<Path>,
    {
        let contents = Rope::from_reader(BufReader::new(File::open(path).unwrap())).unwrap();
        Buffer {
            contents,
            primary_selection_index: 0,
            selections: vec![Selection::new()],
            lines_offset: 0,
            columns_offset: 0,
        }
    }

    pub fn lines_offset(&self) -> usize {
        self.lines_offset
    }

    pub fn columns_offset(&self) -> usize {
        self.columns_offset
    }

    pub fn scroll_up(&mut self, distance: usize) {
        self.lines_offset = self.lines_offset.saturating_sub(distance);
    }

    pub fn scroll_down(&mut self, distance: usize) {
        let new_lines_offset = self.lines_offset.saturating_add(distance);
        if new_lines_offset <= self.contents.len_lines() {
            self.lines_offset = new_lines_offset;
        }
    }

    pub fn scroll_left(&mut self, distance: usize) {
        self.columns_offset = self.columns_offset.saturating_sub(distance);
    }

    pub fn scroll_right(&mut self, distance: usize) {
        self.columns_offset = self.columns_offset.saturating_add(distance);
    }
}
