use ropey::Rope;

pub struct Buffer {
    pub contents: Rope,
}

impl Buffer {
    pub fn new(contents: Rope) -> Buffer {
        Buffer { contents }
    }

    pub fn insert_char(&mut self, line: usize, column: usize, character: char) {
        let index = self.contents.line_to_char(line) + column;
        self.contents.insert_char(index, character);
    }

    pub fn delete_char(&mut self, line: usize, column: usize) {
        let index = self.contents.line_to_char(line) + column;
        self.contents.remove(index..(index + 1));
    }
}
