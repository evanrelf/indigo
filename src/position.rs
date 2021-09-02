use std::fmt::Display;

#[derive(Clone, Copy, PartialEq, PartialOrd, Ord, Eq, Debug)]
pub struct Position {
    pub line: usize,
    pub column: usize,
}

impl Position {
    pub fn new(line: usize, column: usize) -> Position {
        Position { line, column }
    }

    pub fn move_up(&mut self, distance: usize) -> &Position {
        self.line = self.line.saturating_sub(distance);
        self
    }

    pub fn move_down(&mut self, distance: usize) -> &Position {
        self.line = self.line.saturating_add(distance);
        self
    }

    pub fn move_left(&mut self, distance: usize) -> &Position {
        self.column = self.column.saturating_sub(distance);
        self
    }

    pub fn move_right(&mut self, distance: usize) -> &Position {
        self.column = self.column.saturating_add(distance);
        self
    }
}

impl Display for Position {
    fn fmt(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(formatter, "{}:{}", self.line, self.column)
    }
}

impl Default for Position {
    fn default() -> Position {
        Position { line: 0, column: 0 }
    }
}

impl From<(usize, usize)> for Position {
    fn from((line, column): (usize, usize)) -> Position {
        Position { line, column }
    }
}

#[test]
fn test_partial_ord() {
    assert!(Position::from((0, 0)) < Position::from((1, 0)));
    assert!(Position::from((0, 99)) < Position::from((1, 0)));
    assert!(Position::from((1, 0)) < Position::from((1, 1)));
}
