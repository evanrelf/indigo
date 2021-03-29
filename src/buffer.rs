use ropey::Rope;
use std::fs::File;
use std::io::BufReader;
use std::path::Path;

pub struct Buffer {
    pub contents: Rope,
}

impl Buffer {
    pub fn new(contents: Rope) -> Buffer {
        Buffer { contents }
    }

    pub fn from_file<P>(path: P) -> Buffer
    where
        P: AsRef<Path>,
    {
        let contents = Rope::from_reader(BufReader::new(File::open(path).unwrap())).unwrap();
        Buffer { contents }
    }

    // pub fn coordinates_to_index(&self, line: usize, column: usize) -> Option<usize> {
    //     todo!()
    // }

    // pub fn index_to_coordinates(&self, index: usize) -> (usize, usize) {
    //     let line = self.contents.char_to_line(index);
    //     let column = todo!();
    //     (line, column)
    // }

    pub fn insert_char(&mut self, line: usize, column: usize, character: char) {
        let index = self.contents.line_to_char(line) + column;
        self.contents.insert_char(index, character);
    }

    pub fn delete_char(&mut self, line: usize, column: usize) {
        let index = self.contents.line_to_char(line) + column;
        self.contents.remove(index..(index + 1));
    }
}
