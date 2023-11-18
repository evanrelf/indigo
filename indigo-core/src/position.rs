use crate::RopeExt as _;
use ropey::Rope;

#[derive(Clone, Copy, Debug, Default, Eq, Ord, PartialEq, PartialOrd)]
pub struct Position {
    pub line: usize,
    pub column: usize,
}

impl Position {
    pub fn from_char_index(index: usize, rope: &Rope) -> anyhow::Result<Self> {
        Self::from_char_index_strict(index, rope).map(|x| match x {
            Ok(position) | Err(position) => position,
        })
    }

    pub fn from_char_index_strict(index: usize, rope: &Rope) -> anyhow::Result<Result<Self, Self>> {
        let mut corrected = false;

        if rope.len_chars() == 0 {
            anyhow::bail!("Rope is empty");
        }

        let index = if rope.len_chars() <= index {
            corrected = true;
            // When index goes beyond end of rope, use last index
            rope.len_chars().saturating_sub(1)
        } else {
            index
        };

        let line = rope.char_to_line(index);

        let column = index - rope.line_to_char(line);

        let position = Self { line, column };

        if corrected {
            Ok(Err(position))
        } else {
            Ok(Ok(position))
        }
    }

    pub fn to_char_index(self, rope: &Rope) -> anyhow::Result<usize> {
        self.to_char_index_strict(rope).map(|x| match x {
            Ok(index) | Err(index) => index,
        })
    }

    pub fn to_char_index_strict(self, rope: &Rope) -> anyhow::Result<Result<usize, usize>> {
        if rope.len_chars() == 0 {
            anyhow::bail!("Rope is empty");
        }

        let mut line_corrected = false;
        let mut column_corrected = false;

        let last_line = rope.len_lines_indigo().saturating_sub(1);
        let line = if self.line > last_line {
            // When line goes beyond end of rope, use last line
            line_corrected = true;
            last_line
        } else {
            self.line
        };

        let last_column = rope.line(line).len_chars().saturating_sub(1);
        let column = if line_corrected || self.column > last_column {
            // When line was corrected, or column goes beyond end of line, use last column
            column_corrected = true;
            last_column
        } else {
            self.column
        };

        let index = column + rope.line_to_char(line);

        if line_corrected || column_corrected {
            Ok(Err(index))
        } else {
            Ok(Ok(index))
        }
    }

    pub fn correct(&mut self, rope: &Rope) {
        let index = self.to_char_index(rope).unwrap_or_default();
        *self = Self::from_char_index(index, rope).unwrap_or_default();
    }

    // TODO: Bring back corrected return value so we can be more strict with insertion functions

    pub fn insert_char(&self, c: char, rope: &mut Rope) -> anyhow::Result<Self> {
        // TODO: Fail if position isn't valid, without correction
        let index = self.to_char_index(rope).unwrap_or_default();
        let position = Self::from_char_index(index + 1, rope)
            .expect("position is valid because it comes from a known valid index");
        rope.try_insert_char(index, c)?;
        Ok(position)
    }

    pub fn insert(&self, s: &str, rope: &mut Rope) -> anyhow::Result<Self> {
        // TODO: Fail if position isn't valid, without correction
        let index = self.to_char_index(rope).unwrap_or_default();
        let position = Self::from_char_index(index + s.len(), rope)
            .expect("position is valid because it comes from a known valid index");
        rope.try_insert(index, s)?;
        Ok(position)
    }

    pub fn assert_valid(&self) {
        // `Position`s are trivially valid
    }
}

impl From<(usize, usize)> for Position {
    fn from((line, column): (usize, usize)) -> Self {
        Self { line, column }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

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
    fn from_char_index() {
        let rope = Rope::default();
        let index = 1;
        assert!(Position::from_char_index(index, &rope).is_err());

        let rope = Rope::from_str("foo\nbar");
        let index = 7;
        assert!(matches!(
            Position::from_char_index(index, &rope),
            Ok(Position { line: 1, column: 2 })
        ));

        let rope = Rope::from_str("foo\nbar\n");
        let index = 8;
        assert!(matches!(
            Position::from_char_index(index, &rope),
            Ok(Position { line: 1, column: 3 })
        ));

        let rope = Rope::from_str("foo\nbar\n");
        let index = 7;
        assert!(matches!(
            Position::from_char_index(index, &rope),
            Ok(Position { line: 1, column: 3 })
        ));
    }

    #[test]
    fn to_char_index() {
        let rope = Rope::default();
        let position = Position { line: 0, column: 0 };
        assert!(position.to_char_index(&rope).is_err());

        let rope = Rope::from_str("foo\nbar");
        let position = Position { line: 0, column: 4 };
        assert!(matches!(position.to_char_index(&rope), Ok(3)));

        let rope = Rope::from_str("foo\nbar");
        let position = Position { line: 9, column: 0 };
        assert!(matches!(position.to_char_index(&rope), Ok(6)));

        let rope = Rope::from_str("foo\nbar\n");
        let position = Position { line: 9, column: 0 };
        assert!(matches!(position.to_char_index(&rope), Ok(7)));

        let rope = Rope::from_str("foo\nbar");
        let position = Position { line: 1, column: 2 };
        assert!(matches!(position.to_char_index(&rope), Ok(6)));

        let rope = Rope::from_str("foo\nbar\n");
        let position = Position { line: 1, column: 3 };
        assert!(matches!(position.to_char_index(&rope), Ok(7)));
    }
}
