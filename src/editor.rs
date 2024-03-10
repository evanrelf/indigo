use ropey::Rope;

#[derive(Debug, Default)]
pub struct Editor {
    pub text: Rope,
    pub cursor: (usize, usize),
    pub scroll: usize,
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
        self.scroll = self.scroll.saturating_sub(distance);
    }

    pub fn scroll_down(&mut self, distance: usize) {
        self.scroll += distance;
    }
}
