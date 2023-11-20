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

    pub fn insert(&mut self, s: &str) {
        self.command.insert(self.cursor, s);
        self.cursor += s.len();
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

    pub fn move_forward_word(&mut self) {
        if self.cursor == self.command.len_chars() {
            return;
        }

        let distance = self
            .command
            .slice(self.cursor..)
            .chars()
            .enumerate()
            .skip_while(|(_, c)| !is_word_char(*c))
            .skip_while(|(_, c)| is_word_char(*c))
            .map(|(i, _)| i)
            .next();

        match distance {
            Some(distance) => self.move_forward(distance),
            None => self.move_line_end(),
        }
    }

    pub fn move_backward_word(&mut self) {
        if self.cursor == 0 {
            return;
        }

        let distance = self
            .command
            .slice(..self.cursor)
            .chars()
            .collect::<Vec<_>>()
            .into_iter()
            .rev()
            .enumerate()
            .skip_while(|(_, c)| !is_word_char(*c))
            .skip_while(|(_, c)| is_word_char(*c))
            .map(|(i, _)| i)
            .next();

        match distance {
            Some(distance) => self.move_backward(distance),
            None => self.move_line_begin(),
        }
    }

    pub fn move_line_begin(&mut self) {
        self.cursor = 0;
    }

    pub fn move_line_end(&mut self) {
        self.cursor = self.command.len_chars();
    }
}

fn is_word_char(c: char) -> bool {
    // TODO: Not sure if '_' or other characters should be included?
    c.is_ascii_alphanumeric()
}
