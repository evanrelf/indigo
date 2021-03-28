pub enum Direction {
    Up,
    Down,
    Left,
    Right,
}

pub struct Cursor {
    line: usize,
    column: usize,
}

impl Cursor {
    pub fn new(line: usize, column: usize) -> Cursor {
        Cursor { line, column }
    }

    pub fn line(&self) -> usize {
        self.line
    }

    pub fn column(&self) -> usize {
        self.column
    }

    pub fn move_cursor(&mut self, direction: Direction, distance: usize) -> bool {
        match direction {
            Direction::Up if self.line >= distance => {
                self.line -= distance;
                true
            }
            Direction::Down => {
                self.line += distance;
                true
            }
            Direction::Left if self.column >= distance => {
                self.column -= distance;
                true
            }
            Direction::Right => {
                self.column += distance;
                true
            }
            _ => false,
        }
    }
}
