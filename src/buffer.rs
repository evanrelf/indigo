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
    contents: Rope,
    selections: Vec<Selection>,
}

impl Buffer {
    pub fn new() -> Buffer {
        Buffer {
            contents: Rope::new(),
            selections: Vec::new(),
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
        }
    }
}
