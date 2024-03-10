use crate::{conversion::Conversion, rope::RopeExt as _};
use ropey::Rope;

#[derive(Clone, Copy, Debug)]
pub struct Position {
    pub line: usize,
    pub column: usize,
}

impl Position {
    pub fn from_char_index(index: usize, rope: &Rope) -> anyhow::Result<Self> {
        Self::from_char_index_strict(index, rope).map(Conversion::into_inner)
    }

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

    pub fn to_char_index(self, rope: &Rope) -> anyhow::Result<usize> {
        self.to_char_index_strict(rope).map(Conversion::into_inner)
    }

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

    pub fn correct(&mut self, rope: &Rope) -> anyhow::Result<()> {
        let index = self.to_char_index(rope)?;
        *self = Self::from_char_index(index, rope)?;
        Ok(())
    }
}