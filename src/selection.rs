#[derive(Clone)]
pub struct Position {
    pub line: usize,
    pub column: usize,
}

impl Position {
    pub fn move_up(&mut self, distance: usize) {
        self.line = self.line.saturating_sub(distance);
    }

    pub fn move_down(&mut self, distance: usize) {
        self.line += distance;
    }

    pub fn move_left(&mut self, distance: usize) {
        self.column = self.column.saturating_sub(distance);
    }

    pub fn move_right(&mut self, distance: usize) {
        self.column += distance;
    }
}

pub struct Selection {
    pub cursor: Position,
    pub anchor: Position,
}

impl Selection {
    pub fn new() -> Selection {
        Selection {
            cursor: Position { line: 0, column: 0 },
            anchor: Position { line: 0, column: 0 },
        }
    }

    pub fn is_reduced(&self) -> bool {
        self.anchor.line == self.cursor.line && self.anchor.column == self.cursor.column
    }

    pub fn reduce(&mut self) {
        if !self.is_reduced() {
            self.anchor.line = self.cursor.line;
            self.anchor.column = self.cursor.column;
        }
    }

    pub fn flip(&mut self) {
        if !self.is_reduced() {
            let cursor_line = self.cursor.line;
            let cursor_column = self.cursor.column;

            self.cursor.line = self.anchor.line;
            self.cursor.column = self.anchor.column;

            self.anchor.line = cursor_line;
            self.anchor.column = cursor_column;
        }
    }
}
