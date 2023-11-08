use crate::{conversion::Conversion, rope::RopeExt as _};
use ropey::Rope;

#[derive(Clone, Copy, Debug, Default, Eq, Ord, PartialEq, PartialOrd)]
pub struct Position {
    pub line: usize,
    pub column: usize,
}

impl Position {
    pub fn from_char_index(index: usize, rope: &Rope) -> anyhow::Result<Conversion<Self>> {
        if rope.len_chars() == 0 {
            anyhow::bail!("Rope is empty");
        }

        let mut corrected = false;

        let index = if rope.len_chars() <= index {
            corrected = true;
            rope.len_chars().saturating_sub(1)
        } else {
            index
        };

        let line = rope.char_to_line(index);

        let column = index - rope.line_to_char(line);

        let position = Self { line, column };

        if corrected {
            Ok(Conversion::Corrected(position))
        } else {
            Ok(Conversion::Valid(position))
        }
    }

    pub fn to_char_index(self, rope: &Rope) -> anyhow::Result<Conversion<usize>> {
        if rope.len_chars() == 0 {
            anyhow::bail!("Rope is empty");
        }

        let mut corrected = false;

        let last_line = rope.len_lines_indigo().saturating_sub(1);
        let line = if self.line > last_line {
            // When line goes beyond end of rope, use last line
            corrected = true;
            last_line
        } else {
            self.line
        };

        let last_column = rope.line(line).len_chars().saturating_sub(1);
        let column = if corrected {
            // When line was corrected, use last column
            last_column
        } else if self.column > last_column {
            // When column goes beyond end of line, use last column
            corrected = true;
            last_column
        } else {
            self.column
        };

        let index = column + rope.line_to_char(line);

        if corrected {
            Ok(Conversion::Corrected(index))
        } else {
            Ok(Conversion::Valid(index))
        }
    }

    pub fn assert_valid(&self) {}
}

impl From<(usize, usize)> for Position {
    fn from((line, column): (usize, usize)) -> Self {
        Self { line, column }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::conversion::Conversion;

    #[test]
    fn position_default() {
        assert_eq!(Position::default(), Position::from((0, 0)));
    }

    #[test]
    fn position_ord() {
        assert!(Position::from((0, 0)) < Position::from((0, 1)));
        assert!(Position::from((0, 0)) < Position::from((1, 0)));
        assert!(Position::from((5, 5)) < Position::from((5, 6)));
        assert!(Position::from((5, 5)) < Position::from((6, 0)));
    }

    #[test]
    fn from_char_index_invalid() {
        let rope = Rope::default();
        let index = 1;
        assert!(Position::from_char_index(index, &rope).is_err());
    }

    #[test]
    fn from_char_index_corrected() {
        let rope = Rope::from_str("foo\nbar");
        let index = 7;
        assert!(matches!(
            Position::from_char_index(index, &rope),
            Ok(Conversion::Corrected(Position { line: 1, column: 2 }))
        ));

        let rope = Rope::from_str("foo\nbar\n");
        let index = 8;
        assert!(matches!(
            Position::from_char_index(index, &rope),
            Ok(Conversion::Corrected(Position { line: 1, column: 3 }))
        ));
    }

    #[test]
    fn from_char_index_valid() {
        let rope = Rope::from_str("foo\nbar\n");
        let index = 7;
        assert!(matches!(
            Position::from_char_index(index, &rope),
            Ok(Conversion::Valid(Position { line: 1, column: 3 }))
        ));
    }

    #[test]
    fn to_char_index_invalid() {
        let rope = Rope::default();
        let position = Position { line: 0, column: 0 };
        assert!(position.to_char_index(&rope).is_err());
    }

    #[test]
    fn to_char_index_corrected() {
        let rope = Rope::from_str("foo\nbar");
        let position = Position { line: 0, column: 4 };
        assert!(matches!(
            position.to_char_index(&rope),
            Ok(Conversion::Corrected(3))
        ));

        let rope = Rope::from_str("foo\nbar");
        let position = Position { line: 9, column: 0 };
        assert!(matches!(
            position.to_char_index(&rope),
            Ok(Conversion::Corrected(6))
        ));

        let rope = Rope::from_str("foo\nbar\n");
        let position = Position { line: 9, column: 0 };
        assert!(matches!(
            position.to_char_index(&rope),
            Ok(Conversion::Corrected(7))
        ));
    }

    #[test]
    fn to_char_index_valid() {
        let rope = Rope::from_str("foo\nbar");
        let position = Position { line: 1, column: 2 };
        assert!(matches!(
            position.to_char_index(&rope),
            Ok(Conversion::Valid(6))
        ));

        let rope = Rope::from_str("foo\nbar\n");
        let position = Position { line: 1, column: 3 };
        assert!(matches!(
            position.to_char_index(&rope),
            Ok(Conversion::Valid(7))
        ));
    }
}
