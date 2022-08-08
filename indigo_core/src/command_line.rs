use std::cmp::min;

#[derive(Clone, Default)]
pub struct CommandLine {
    contents: String,
    cursor_index: usize,
}

impl CommandLine {
    #[must_use]
    pub fn contents(&self) -> &String {
        &self.contents
    }

    #[must_use]
    pub fn cursor_index(&self) -> usize {
        self.cursor_index
    }

    #[must_use]
    pub fn move_left(&self, distance: usize) -> Self {
        let mut command_line = self.clone();
        command_line.cursor_index = command_line.cursor_index.saturating_sub(distance);
        command_line
    }

    #[must_use]
    pub fn move_right(&self, distance: usize) -> Self {
        let mut command_line = self.clone();
        command_line.cursor_index = min(
            command_line.contents.len(),
            command_line.cursor_index + distance,
        );
        command_line
    }

    #[must_use]
    pub fn move_line_begin(&self) -> Self {
        let mut command_line = self.clone();
        command_line.cursor_index = 0;
        command_line
    }

    #[must_use]
    pub fn move_line_end(&self) -> Self {
        let mut command_line = self.clone();
        command_line.cursor_index = command_line.contents.len();
        command_line
    }

    #[must_use]
    pub fn insert_char(&self, c: char) -> Self {
        let mut command_line = self.clone();
        command_line.contents.insert(command_line.cursor_index, c);
        command_line.cursor_index += 1;
        command_line
    }

    #[must_use]
    pub fn backspace(&self) -> Self {
        let mut command_line = self.clone();
        if command_line.cursor_index > 0 {
            command_line.contents.remove(command_line.cursor_index - 1);
            command_line.cursor_index -= 1;
        }
        command_line
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_insert_char() {
        let mut command_line = CommandLine::default();
        command_line = command_line.insert_char('h');
        command_line = command_line.insert_char('i');
        assert_eq!(command_line.contents(), "hi");
    }
}
