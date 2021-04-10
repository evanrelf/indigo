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
    selections: Vec<Selection>,
    pub lines_offset: usize,
    pub columns_offset: usize,
}

impl Buffer {
    pub fn new() -> Buffer {
        Buffer {
            contents: Rope::new(),
            selections: Vec::new(),
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
            selections: Vec::new(),
            lines_offset: 0,
            columns_offset: 0,
        }
    }
}
