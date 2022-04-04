use ropey::Rope;
use std::fmt::Display;

#[derive(Clone, Debug, Default, PartialEq, PartialOrd)]
pub struct Position {
    pub line: usize,
    pub column: usize,
}

impl Position {
    pub fn to_rope_index(&self, rope: &Rope) -> Option<usize> {
        // Assert line is valid
        if rope.len_lines() <= self.line {
            return None;
        }

        // Assert column is valid
        if rope.line(self.line).len_chars() <= self.column {
            return None;
        }

        Some(rope.line_to_char(self.line) + self.column)
    }

    pub fn to_rope_index_lossy(&self, rope: &Rope) -> (usize, bool) {
        let mut lossy = false;

        // Get valid line
        let lines = rope.len_lines();
        let line = if lines <= self.line {
            // When line goes beyond end of rope, use last line
            lossy = true;
            // Subtracting 1 to convert to zero-based index, subtracting another 1 to remove
            // ropey's mysterious empty final line
            lines.saturating_sub(2)
        } else {
            self.line
        };

        // Get valid column
        let columns = rope.line(line).len_chars();
        let column = if columns <= self.column {
            // When column goes beyond end of line, use last column
            lossy = true;
            columns.saturating_sub(1)
        } else {
            self.column
        };

        (rope.line_to_char(line) + column, lossy)
    }

    pub fn from_rope_index(rope: &Rope, index: usize) -> Option<Self> {
        // Assert index is valid
        if rope.len_chars() <= index {
            return None;
        }

        let line = rope.char_to_line(index);

        let column = index - rope.line_to_char(line);

        Some(Self { line, column })
    }

    pub fn from_rope_index_lossy(rope: &Rope, index: usize) -> (Self, bool) {
        let mut lossy = false;

        // Get valid index
        let index = if rope.len_chars() <= index {
            // When index goes beyond end of rope, use last index
            lossy = true;
            rope.len_chars().saturating_sub(1)
        } else {
            index
        };

        let line = rope.char_to_line(index);

        let column = index - rope.line_to_char(line);

        (Self { line, column }, lossy)
    }

    pub fn corrected(&self, rope: &Rope) -> (Self, bool) {
        let (index, lossy) = self.to_rope_index_lossy(rope);
        // `unwrap` is safe here because `to_rope_index_lossy` returns a valid rope index
        let position = Self::from_rope_index(rope, index).unwrap();
        (position, lossy)
    }
}

impl Display for Position {
    fn fmt(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(formatter, "{}:{}", self.line + 1, self.column + 1)
    }
}

impl From<(usize, usize)> for Position {
    fn from((line, column): (usize, usize)) -> Self {
        Self { line, column }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_partialord() {
        assert!(Position::from((0, 0)) < Position::from((1, 0)));
        assert!(Position::from((0, 99)) < Position::from((1, 0)));
        assert!(Position::from((1, 0)) < Position::from((1, 1)));
    }

    #[test]
    fn test_rope_index_roundtrip() {
        let rope = Rope::from_str("hello\nworld\n!\n");

        let case = |line: usize, column: usize| {
            let position = Position { line, column };
            assert_eq!(
                Some(position.clone()),
                position
                    .to_rope_index(&rope)
                    .and_then(|index| Position::from_rope_index(&rope, index)),
            );
        };

        case(0, 0);
        case(1, 4);
        case(1, 5);
        case(2, 0);
    }

    #[test]
    fn test_corrected() {
        let rope = Rope::from_str("hello\nworld\n!\n");

        let case = |before: (usize, usize), after: (usize, usize)| {
            let before_position = Position {
                line: before.0,
                column: before.1,
            };
            let expected_after_position = Position {
                line: after.0,
                column: after.1,
            };
            let (actual_after_position, _) = before_position.corrected(&rope);

            assert_eq!(expected_after_position, actual_after_position,);
        };

        case((0, 0), (0, 0));
        case((1, 4), (1, 4));
        case((1, 5), (1, 5));
        case((2, 0), (2, 0));
        case((0, 99), (0, 5));
        case((99, 99), (2, 1));
    }
}
