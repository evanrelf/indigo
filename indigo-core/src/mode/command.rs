use ropey::Rope;
use std::cmp::min;

#[derive(Clone, Debug, Default)]
pub struct CommandMode {
    // TODO: Maybe change back to `String` for convenience
    command: Rope,
    cursor: usize,
}

impl CommandMode {
    #[must_use]
    pub fn command(&self) -> &Rope {
        &self.command
    }

    #[must_use]
    pub fn cursor(&self) -> usize {
        self.cursor
    }

    pub fn insert_char(&mut self, c: char) {
        self.command.insert_char(self.cursor, c);
        self.cursor += 1;
    }

    pub fn backspace(&mut self) {
        if self.cursor > 0 {
            let cursor = self.cursor - 1;
            self.command.remove(cursor..=cursor);
            self.cursor = cursor;
        }
    }

    pub fn clear_backward(&mut self) {
        self.command.remove(..self.cursor);
        self.cursor = 0;
    }

    pub fn clear_forward(&mut self) {
        self.command.remove(self.cursor..);
    }

    pub fn move_backward(&mut self, distance: usize) {
        self.cursor = self.cursor.saturating_sub(distance);
    }

    pub fn move_forward(&mut self, distance: usize) {
        self.cursor = min(self.command.len_chars(), self.cursor + distance);
    }

    pub fn move_line_begin(&mut self) {
        self.cursor = 0;
    }

    pub fn move_line_end(&mut self) {
        self.cursor = self.command.len_chars();
    }
}
