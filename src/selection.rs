pub struct Position {
    pub line: usize,
    pub column: usize,
}

impl Position {
    pub fn move_up(&mut self, distance: usize) -> bool {
        let allowed = self.line >= distance;
        if allowed {
            self.line -= distance;
        }
        allowed
    }

    pub fn move_down(&mut self, distance: usize) -> bool {
        self.line += distance;
        true
    }

    pub fn move_left(&mut self, distance: usize) -> bool {
        let allowed = self.column >= distance;
        if allowed {
            self.column -= distance;
        }
        allowed
    }

    pub fn move_right(&mut self, distance: usize) -> bool {
        self.column += distance;
        true
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
}
