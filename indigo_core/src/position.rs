use ropey::Rope;
use std::{
    cmp::{max, min},
    num::NonZeroUsize,
};

#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub struct Position {
    pub line: NonZeroUsize,
    pub column: NonZeroUsize,
}

enum Direction {
    Backward,
    Forward,
}

impl Position {
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

        let rope_line = self.line.get() - 1;

        // Get valid line
        let lines = rope.len_lines().saturating_sub(1);
        let line = if lines <= rope_line {
            // When line goes beyond end of rope, use last line
            corrected = true;
            lines.saturating_sub(1)
        } else {
            rope_line
        };

        let rope_column = self.column.get() - 1;

        // Get valid column
        let columns = rope.line(line).len_chars();
        let column = if columns <= rope_column {
            // When column goes beyond end of line, use last column
            corrected = true;
            columns.saturating_sub(1)
        } else {
            rope_column
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

        let rope_line = rope.char_to_line(index);

        let rope_column = index - rope.line_to_char(rope_line);

        let position = Self {
            line: NonZeroUsize::new(rope_line + 1).unwrap(),
            column: NonZeroUsize::new(rope_column + 1).unwrap(),
        };

        assert!(rope.len_chars() > 0, "cannot handle empty ropes yet");
        (position, corrected)
    }
}

impl Default for Position {
    fn default() -> Self {
        Self {
            line: NonZeroUsize::new(1).unwrap(),
            column: NonZeroUsize::new(1).unwrap(),
        }
    }
}

#[must_use]
pub fn position_is_valid(position: &Position, rope: Option<&Rope>) -> bool {
    if let Some(rope) = rope {
        let rope_line = position.line.get() - 1;

        // Assert line is valid
        if rope.len_lines().saturating_sub(1) < rope_line {
            return false;
        }

        let rope_column = position.column.get() - 1;

        // Assert column is valid
        if rope.line(rope_line).len_chars() <= rope_column {
            return false;
        }

        true
    } else {
        *position == Position::default()
    }
}

#[must_use]
pub fn index_is_valid(index: usize, rope: Option<&Rope>) -> bool {
    if let Some(rope) = rope {
        rope.len_chars() > index
    } else {
        index == 0
    }
}

impl From<(NonZeroUsize, NonZeroUsize)> for Position {
    fn from((line, column): (NonZeroUsize, NonZeroUsize)) -> Self {
        Self { line, column }
    }
}

impl TryFrom<(usize, usize)> for Position {
    type Error = ();

    fn try_from((line, column): (usize, usize)) -> Result<Self, Self::Error> {
        Ok(Self {
            line: NonZeroUsize::new(line).ok_or(())?,
            column: NonZeroUsize::new(column).ok_or(())?,
        })
    }
}

#[must_use]
fn vertically(position: &Position, rope: &Rope, direction: Direction, distance: usize) -> Position {
    let desired_position = Position {
        line: NonZeroUsize::new(max(1, {
            match direction {
                Direction::Backward => position.line.get().saturating_sub(distance),
                Direction::Forward => {
                    // Subtracting 1 to remove ropey's mysterious empty final line
                    let last_line = rope.len_lines().saturating_sub(1);
                    // Prevent `corrected` from moving us to the last index in the rope if we try to go
                    // below the last line
                    min(position.line.get() + distance, last_line)
                }
            }
        }))
        .unwrap(),
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
    Position::try_from((1, 1)).unwrap()
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
pub fn line_begin(line: NonZeroUsize, rope: &Rope) -> Position {
    let line = line.get() - 1;
    let last_line = rope.len_lines().saturating_sub(2);
    if line > last_line {
        bottom(rope)
    } else {
        Position::try_from((line + 1, 1)).unwrap()
    }
}

#[must_use]
pub fn line_first_non_blank(line: NonZeroUsize, rope: &Rope) -> Position {
    let blanks = [' ', '\t'];

    let first_non_blank = rope
        .line(line.get() - 1)
        .chars()
        .enumerate()
        .find(|(_, c)| !blanks.contains(c));

    match first_non_blank {
        None => line_end(line, rope),
        Some((column, _)) => Position::try_from((line.get(), column + 1)).unwrap(),
    }
}

#[must_use]
pub fn line_end(line: NonZeroUsize, rope: &Rope) -> Position {
    match rope.get_line(line.get() - 1) {
        None => end(rope),
        Some(rope_slice) => {
            let column = NonZeroUsize::new(rope_slice.len_chars()).unwrap();
            Position::from((line, column))
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use proptest::prelude::*;

    proptest! {
        #[test]
        fn test_to_rope_index_alphanumeric(s in "[a-zA-Z0-9 \\n]+", line in 1usize.., column in 1usize..) {
            let rope = &Rope::from(s);
            let position = Position::try_from((line, column)).unwrap();
            let _ = position.to_rope_index(rope);
        }

        #[test]
        fn test_from_rope_index_alphanumeric(s in "[a-zA-Z0-9 \\n]+", i: usize) {
            let rope = &Rope::from(s);
            let _ = Position::from_rope_index(rope, i);
        }

        #[test]
        fn test_to_rope_index_unicode(s in "\\PC+", line in 1usize.., column in 1usize..) {
            let rope = &Rope::from(s);
            let position = Position::try_from((line, column)).unwrap();
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
        let position = Position::try_from((1, 1)).unwrap();
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
        let position = Position::try_from((1, 1)).unwrap();
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
        let position = Position::try_from((1, 1)).unwrap();
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
            (0, (1, 1)),
            (1, (1, 2)),
            (5, (1, 6)),
            (6, (2, 1)),
            (11, (2, 6)),
        ]
        .into_iter()
        .map(|(i, p)| (i, p.try_into().unwrap()))
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
            (0, (1, 1), false),
            (1, (1, 2), false),
            (5, (1, 6), false),
            (6, (2, 1), false),
            (11, (2, 6), false),
            (12, (2, 6), true),
            (999, (2, 6), true),
        ]
        .into_iter()
        .map(|(i, p, c)| (i, p.try_into().unwrap(), c))
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
            ((1, 1), 0, false),
            ((1, 2), 1, false),
            ((1, 6), 5, false),
            ((2, 1), 6, false),
            ((2, 6), 11, false),
            ((2, 7), 11, true),
            ((99, 99), 11, true),
        ]
        .into_iter()
        .map(|(p, i, c)| (p.try_into().unwrap(), i, c))
        .collect();

        for (position, index, corrected) in cases {
            let (actual_index, actual_corrected) = position.to_rope_index_c(rope);
            assert_eq!(index, actual_index);
            assert_eq!(corrected, actual_corrected);
        }
    }
}
