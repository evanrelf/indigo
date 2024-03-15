use crate::{Conversion, RopeExt as _};
use ropey::Rope;

#[derive(Clone, Copy, Debug, Default, Eq, Ord, PartialEq, PartialOrd)]
pub struct Position {
    pub line: usize,
    pub column: usize,
}

impl Position {
    /// Convert a character index in a rope into a position.
    ///
    /// If the index exceeds the length of the rope, it is snapped to the last character. Fails if
    /// the rope is empty.
    ///
    /// If you need to know if a correction was made, use [`Position::from_char_index_strict`].
    pub fn from_char_index(index: usize, rope: &Rope) -> anyhow::Result<Self> {
        Self::from_char_index_strict(index, rope).map(Conversion::into_inner)
    }

    /// Convert a character index in a rope into a position.
    ///
    /// If the index exceeds the length of the rope, it is snapped to the last character. Fails if
    /// the rope is empty.
    pub fn from_char_index_strict(index: usize, rope: &Rope) -> anyhow::Result<Conversion<Self>> {
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
            Ok(Conversion::Corrected(position))
        } else {
            Ok(Conversion::Valid(position))
        }
    }

    /// Convert a position in a rope into a character index.
    ///
    /// If the position's column exceeds the length of a line, it is snapped to the last column. If
    /// the position exceeds the length of the rope, it is snapped to the last character. Fails if
    /// the rope is empty.
    ///
    /// If you need to know if a correction was made, use [`Position::to_char_index_strict`].
    pub fn to_char_index(self, rope: &Rope) -> anyhow::Result<usize> {
        self.to_char_index_strict(rope).map(Conversion::into_inner)
    }

    /// Convert a position in a rope into a character index.
    ///
    /// If the position's column exceeds the length of a line, it is snapped to the last column. If
    /// the position exceeds the length of the rope, it is snapped to the last character. Fails if
    /// the rope is empty.
    pub fn to_char_index_strict(self, rope: &Rope) -> anyhow::Result<Conversion<usize>> {
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
            Ok(Conversion::Corrected(index))
        } else {
            Ok(Conversion::Valid(index))
        }
    }

    pub fn via_char_index<F>(self, rope: &Rope, update: F) -> anyhow::Result<Self>
    where
        F: FnOnce(usize) -> usize,
    {
        self.via_char_index_strict(rope, update)
            .map(Conversion::into_inner)
    }

    pub fn via_char_index_strict<F>(
        self,
        rope: &Rope,
        update: F,
    ) -> anyhow::Result<Conversion<Self>>
    where
        F: FnOnce(usize) -> usize,
    {
        let mut corrected = false;

        let index = self.to_char_index_strict(rope)?.map(update);
        corrected |= index.is_corrected();

        let position = Self::from_char_index_strict(index.into_inner(), rope)?;
        corrected |= position.is_corrected();

        if corrected {
            Ok(Conversion::Corrected(position.into_inner()))
        } else {
            Ok(Conversion::Valid(position.into_inner()))
        }
    }

    /// Correct a position so it's valid in the provided rope.
    ///
    /// If the position's column exceeds the length of a line, it is snapped to the last column. If
    /// the position exceeds the length of the rope, it is snapped to the last character. Fails
    /// if the rope is empty.
    pub fn correct(&mut self, rope: &Rope) -> anyhow::Result<()> {
        let index = self.to_char_index(rope)?;
        *self = Self::from_char_index(index, rope)?;
        Ok(())
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

    // TODO: Test strict conversion functions instead

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
