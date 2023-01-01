use ropey::Rope;
use std::cmp::min;

#[derive(Clone, Copy, Debug, Default, Eq, Ord, PartialEq, PartialOrd)]
pub struct Position {
    pub line: usize,
    pub column: usize,
}

enum Direction {
    Backward,
    Forward,
}

impl Position {
    #[must_use]
    pub fn is_valid(&self, rope: Option<&Rope>) -> bool {
        if let Some(rope) = rope {
            let is_line_valid = self.line <= rope.len_lines().saturating_sub(1);
            let is_column_valid = self.column < rope.line(self.line).len_chars();
            is_line_valid && is_column_valid
        } else {
            *self == Self::default()
        }
    }

    #[must_use]
    pub fn corrected(&self, rope: &Rope) -> Self {
        let index = self.to_rope_index(rope);
        Self::from_rope_index(rope, index)
    }

    #[must_use]
    pub fn to_rope_index(&self, rope: &Rope) -> usize {
        self.to_rope_index_c(rope).0
    }

    #[doc(hidden)]
    #[must_use]
    pub fn to_rope_index_c(&self, rope: &Rope) -> (usize, bool) {
        assert!(rope.len_chars() > 0, "cannot handle empty ropes yet");

        let mut corrected = false;

        // Get valid line
        let lines = rope.len_lines().saturating_sub(1);
        let line = if lines <= self.line {
            // When line goes beyond end of rope, use last line
            corrected = true;
            lines.saturating_sub(1)
        } else {
            self.line
        };

        // Get valid column
        let columns = rope.line(line).len_chars();
        let column = if columns <= self.column {
            // When column goes beyond end of line, use last column
            corrected = true;
            columns.saturating_sub(1)
        } else {
            self.column
        };

        let index = rope.line_to_char(line) + column;

        (index, corrected)
    }

    #[must_use]
    pub fn from_rope_index(rope: &Rope, index: usize) -> Self {
        Self::from_rope_index_c(rope, index).0
    }

    #[doc(hidden)]
    #[must_use]
    pub fn from_rope_index_c(rope: &Rope, index: usize) -> (Self, bool) {
        let mut corrected = false;

        // Get valid index
        let index = if rope.len_chars() <= index {
            // When index goes beyond end of rope, use last index
            corrected = true;
            rope.len_chars().saturating_sub(1)
        } else {
            index
        };

        let line = rope.char_to_line(index);

        let column = index - rope.line_to_char(line);

        let position = Self { line, column };

        assert!(rope.len_chars() > 0, "cannot handle empty ropes yet");
        (position, corrected)
    }
}

#[must_use]
pub fn is_index_valid(index: usize, rope: Option<&Rope>) -> bool {
    if let Some(rope) = rope {
        rope.len_chars() > index
    } else {
        index == 0
    }
}

#[must_use]
fn vertically(position: &Position, rope: &Rope, direction: Direction, distance: usize) -> Position {
    let desired_position = Position {
        line: match direction {
            Direction::Backward => position.line.saturating_sub(distance),
            Direction::Forward => {
                // Subtracting 1 to remove ropey's mysterious empty final line
                let last_line = rope.len_lines().saturating_sub(1);
                // Prevent `corrected` from moving us to the last index in the rope if we try to go
                // below the last line
                min(position.line + distance, last_line)
            }
        },
        column: position.column,
    };

    desired_position.corrected(rope)
}

#[must_use]
fn horizontally(
    position: &Position,
    rope: &Rope,
    direction: Direction,
    distance: usize,
) -> Position {
    let index = position.to_rope_index(rope);

    let desired_index = match direction {
        Direction::Backward => index.saturating_sub(distance),
        Direction::Forward => index + distance,
    };

    Position::from_rope_index(rope, desired_index)
}

#[must_use]
pub fn up(position: &Position, rope: &Rope, distance: usize) -> Position {
    vertically(position, rope, Direction::Backward, distance)
}

#[must_use]
pub fn down(position: &Position, rope: &Rope, distance: usize) -> Position {
    vertically(position, rope, Direction::Forward, distance)
}

#[must_use]
pub fn left(position: &Position, rope: &Rope, distance: usize) -> Position {
    horizontally(position, rope, Direction::Backward, distance)
}

#[must_use]
pub fn right(position: &Position, rope: &Rope, distance: usize) -> Position {
    horizontally(position, rope, Direction::Forward, distance)
}

#[must_use]
pub fn top() -> Position {
    Position { line: 0, column: 0 }
}

#[must_use]
pub fn bottom(rope: &Rope) -> Position {
    // Subtracting 1 to remove ropey's mysterious empty final line
    let index = rope.line_to_char(rope.len_lines().saturating_sub(2));
    Position::from_rope_index(rope, index)
}

#[must_use]
pub fn end(rope: &Rope) -> Position {
    let index = rope.len_chars().saturating_sub(1);
    Position::from_rope_index(rope, index)
}

#[must_use]
pub fn line_begin(line: usize, rope: &Rope) -> Position {
    let last_line = rope.len_lines().saturating_sub(2);
    if line > last_line {
        bottom(rope)
    } else {
        Position { line, column: 0 }
    }
}

#[must_use]
pub fn line_first_non_blank(line: usize, rope: &Rope) -> Position {
    let blanks = [' ', '\t'];

    let first_non_blank = rope
        .line(line)
        .chars()
        .enumerate()
        .find(|(_, c)| !blanks.contains(c));

    match first_non_blank {
        None => line_end(line, rope),
        Some((column, _)) => Position { line, column },
    }
}

#[must_use]
pub fn line_end(line: usize, rope: &Rope) -> Position {
    match rope.get_line(line) {
        None => end(rope),
        Some(rope_slice) => {
            let column = rope_slice.len_chars() - 1;
            Position { line, column }
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use proptest::prelude::*;

    proptest! {
        #[test]
        fn test_to_rope_index_alphanumeric(s in "[a-zA-Z0-9 \\n]+", line: usize, column: usize) {
            let rope = &Rope::from(s);
            let position = Position { line, column };
            let _ = position.to_rope_index(rope);
        }

        #[test]
        fn test_from_rope_index_alphanumeric(s in "[a-zA-Z0-9 \\n]+", i: usize) {
            let rope = &Rope::from(s);
            let _ = Position::from_rope_index(rope, i);
        }

        #[test]
        fn test_to_rope_index_unicode(s in "\\PC+", line: usize, column: usize) {
            let rope = &Rope::from(s);
            let position = Position { line, column };
            let _ = position.to_rope_index(rope);
        }

        #[test]
        fn test_from_rope_index_unicode(s in "\\PC+", i: usize) {
            let rope = &Rope::from(s);
            let _ = Position::from_rope_index(rope, i);
        }
    }

    #[test]
    fn test_to_rope_index_empty() {
        let position = Position { line: 0, column: 0 };
        let rope = &Rope::from("");
        let _ = position.to_rope_index(rope);
    }

    #[test]
    fn test_from_rope_index_empty() {
        let rope = &Rope::from("");
        let _ = Position::from_rope_index(rope, 0);
    }

    #[test]
    fn test_to_rope_index_one_line() {
        let position = Position { line: 0, column: 0 };
        let rope = &Rope::from("x");
        let _ = position.to_rope_index(rope);
    }

    #[test]
    fn test_from_rope_index_one_line() {
        let rope = &Rope::from("x");
        let _ = Position::from_rope_index(rope, 0);
    }

    #[test]
    fn test_to_rope_index_one_line_eol() {
        let position = Position { line: 0, column: 0 };
        let rope = &Rope::from("\n");
        let _ = position.to_rope_index(rope);
    }

    #[test]
    fn test_from_rope_index_one_line_eol() {
        let rope = &Rope::from("\n");
        let _ = Position::from_rope_index(rope, 0);
    }

    #[test]
    fn test_roundtrip() {
        let rope = &Rope::from("Hello\nworld\n");

        let cases: Vec<(usize, Position)> = vec![
            (0, (0, 0)),
            (1, (0, 1)),
            (5, (0, 5)),
            (6, (1, 0)),
            (11, (1, 5)),
        ]
        .into_iter()
        .map(|(i, (line, column))| (i, Position { line, column }))
        .collect();

        for (index, position) in cases {
            let (actual_index, index_corrected) = position.to_rope_index_c(rope);
            let (actual_position, position_corrected) = Position::from_rope_index_c(rope, index);
            assert!(!index_corrected && !position_corrected);
            assert_eq!(index, actual_index);
            assert_eq!(position, actual_position);
        }
    }

    #[test]
    fn test_from_rope_index() {
        let rope = &Rope::from("Hello\nworld\n");

        let cases: Vec<(usize, Position, bool)> = vec![
            (0, (0, 0), false),
            (1, (0, 1), false),
            (5, (0, 5), false),
            (6, (1, 0), false),
            (11, (1, 5), false),
            (12, (1, 5), true),
            (999, (1, 5), true),
        ]
        .into_iter()
        .map(|(i, (line, column), c)| (i, Position { line, column }, c))
        .collect();

        for (index, position, corrected) in cases {
            let (actual_position, actual_corrected) = Position::from_rope_index_c(rope, index);
            assert_eq!(position, actual_position);
            assert_eq!(corrected, actual_corrected);
        }
    }

    #[test]
    fn test_to_rope_index() {
        let rope = &Rope::from("Hello\nworld\n");

        let cases: Vec<(Position, usize, bool)> = vec![
            ((0, 0), 0, false),
            ((0, 1), 1, false),
            ((0, 5), 5, false),
            ((1, 0), 6, false),
            ((1, 5), 11, false),
            ((1, 6), 11, true),
            ((99, 99), 11, true),
        ]
        .into_iter()
        .map(|((line, column), i, c)| (Position { line, column }, i, c))
        .collect();

        for (position, index, corrected) in cases {
            let (actual_index, actual_corrected) = position.to_rope_index_c(rope);
            assert_eq!(index, actual_index);
            assert_eq!(corrected, actual_corrected);
        }
    }
}
