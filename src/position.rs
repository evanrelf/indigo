#[derive(Clone)]
pub struct Position {
    pub line: usize,
    pub column: usize,
}

impl From<(usize, usize)> for Position {
    fn from((line, column): (usize, usize)) -> Position {
        Position { line, column }
    }
}

impl From<Position> for (usize, usize) {
    fn from(Position { line, column }: Position) -> (usize, usize) {
        (line, column)
    }
}

impl Default for Position {
    fn default() -> Position {
        Position { line: 0, column: 0 }
    }
}
