use ropey::Rope;

#[derive(Debug, Default)]
pub struct Editor {
    pub text: Rope,
    pub cursor: (usize, usize),
    pub scroll: (usize, usize),
}

impl Editor {
    pub fn move_to(&mut self, line: usize, column: usize) {
        let (cursor_line, cursor_column) = &mut self.cursor;
        *cursor_line = line;
        *cursor_column = column;
    }

    pub fn move_up(&mut self, distance: usize) {
        let (cursor_line, _cursor_column) = &mut self.cursor;
        *cursor_line = cursor_line.saturating_sub(distance);
    }

    pub fn move_down(&mut self, distance: usize) {
        let (cursor_line, _cursor_column) = &mut self.cursor;
        *cursor_line += distance;
    }

    pub fn move_left(&mut self, distance: usize) {
        let (_cursor_line, cursor_column) = &mut self.cursor;
        *cursor_column = cursor_column.saturating_sub(distance);
    }

    pub fn move_right(&mut self, distance: usize) {
        let (_cursor_line, cursor_column) = &mut self.cursor;
        *cursor_column += distance;
    }

    pub fn scroll_to(&mut self, line: usize, column: usize) {
        let (scroll_line, scroll_column) = &mut self.scroll;
        *scroll_line = line;
        *scroll_column = column;
    }

    pub fn scroll_up(&mut self, distance: usize) {
        let (scroll_line, _scroll_column) = &mut self.scroll;
        *scroll_line = scroll_line.saturating_sub(distance);
    }

    pub fn scroll_down(&mut self, distance: usize) {
        let (scroll_line, _scroll_column) = &mut self.scroll;
        *scroll_line += distance;
    }

    pub fn scroll_left(&mut self, distance: usize) {
        let (_scroll_line, scroll_column) = &mut self.scroll;
        *scroll_column = scroll_column.saturating_sub(distance);
    }

    pub fn scroll_right(&mut self, distance: usize) {
        let (_scroll_line, scroll_column) = &mut self.scroll;
        *scroll_column += distance;
    }
}
