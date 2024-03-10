use ropey::Rope;

#[derive(Debug, Default)]
pub struct Editor {
    pub text: Rope,
    pub cursor: (usize, usize),
    pub scroll: (usize, usize),
}

impl Editor {
    pub fn move_up(&mut self, distance: usize) {
        let (line, _column) = &mut self.cursor;
        *line = line.saturating_sub(distance);
    }

    pub fn move_down(&mut self, distance: usize) {
        let (line, _column) = &mut self.cursor;
        *line += distance;
    }

    pub fn move_left(&mut self, distance: usize) {
        let (_line, column) = &mut self.cursor;
        *column = column.saturating_sub(distance);
    }

    pub fn move_right(&mut self, distance: usize) {
        let (_line, column) = &mut self.cursor;
        *column += distance;
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
