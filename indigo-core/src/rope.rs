use crate::Position;
use ropey::{Rope, RopeSlice};

pub trait RopeSliceExt {
    fn len_lines_indigo(&self) -> usize;
}

impl RopeSliceExt for RopeSlice<'_> {
    fn len_lines_indigo(&self) -> usize {
        if self.len_chars() == 0 {
            return 0;
        }
        let last_char = self.char(self.len_chars() - 1);
        self.len_lines() - if last_char == '\n' { 1 } else { 0 }
    }
}

impl RopeSliceExt for Rope {
    fn len_lines_indigo(&self) -> usize {
        if self.len_chars() == 0 {
            return 0;
        }
        let last_char = self.char(self.len_chars() - 1);
        self.len_lines() - if last_char == '\n' { 1 } else { 0 }
    }
}

pub trait RopeExt {
    fn try_insert_char_indigo(&mut self, position: Position, c: char) -> anyhow::Result<Position>;

    fn insert_char_indigo(&mut self, position: Position, c: char) -> Position {
        self.try_insert_char_indigo(position, c).unwrap()
    }

    fn try_insert_indigo(&mut self, position: Position, s: &str) -> anyhow::Result<Position>;

    fn insert_indigo(&mut self, position: Position, s: &str) -> Position {
        self.try_insert_indigo(position, s).unwrap()
    }
}

impl RopeExt for Rope {
    fn try_insert_char_indigo(&mut self, position: Position, c: char) -> anyhow::Result<Position> {
        let index = position.to_char_index(self).unwrap_or_default();
        self.try_insert_char(index, c)?;
        let position = Position::from_char_index(index + 1, self).unwrap();
        Ok(position)
    }

    fn try_insert_indigo(&mut self, position: Position, s: &str) -> anyhow::Result<Position> {
        let index = position.to_char_index(self).unwrap_or_default();
        self.try_insert(index, s)?;
        let position = Position::from_char_index(index + s.len(), self).unwrap();
        Ok(position)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn rope_length() {
        let rope = Rope::default();
        assert_eq!(rope.len_chars(), 0);
        assert_eq!(rope.len_lines(), 1);
        assert_eq!(rope.len_lines_indigo(), 0);

        let rope = Rope::from_str("x");
        assert_eq!(rope.len_chars(), 1);
        assert_eq!(rope.len_lines(), 1);
        assert_eq!(rope.len_lines_indigo(), 1);

        let rope = Rope::from_str("\n");
        assert_eq!(rope.len_chars(), 1);
        assert_eq!(rope.len_lines(), 2);
        assert_eq!(rope.len_lines_indigo(), 1);

        let rope = Rope::from_str("x\n");
        assert_eq!(rope.len_chars(), 2);
        assert_eq!(rope.len_lines(), 2);
        assert_eq!(rope.len_lines_indigo(), 1);

        let rope = Rope::from_str("x\ny\nz");
        assert_eq!(rope.len_chars(), 5);
        assert_eq!(rope.len_lines(), 3);
        assert_eq!(rope.len_lines_indigo(), 3);

        let rope = Rope::from_str("x\ny\nz\n");
        assert_eq!(rope.len_chars(), 6);
        assert_eq!(rope.len_lines(), 4);
        assert_eq!(rope.len_lines_indigo(), 3);
    }

    #[test]
    fn rope_char() {
        assert_eq!(Rope::default().get_char(0), None);
        assert_eq!(Rope::default().get_char(1), None);
        assert_eq!(Rope::from_str("xyz").get_char(0), Some('x'));
        assert_eq!(Rope::from_str("xyz").get_char(1), Some('y'));
        assert_eq!(Rope::from_str("xyz").get_char(2), Some('z'));
        assert_eq!(Rope::from_str("xyz").get_char(3), None);
    }

    #[test]
    fn rope_line() {
        assert_eq!(Rope::default().get_line(0), Some("".into()));
        assert_eq!(Rope::from_str("x\ny\nz").get_line(9), None);
        assert_eq!(Rope::from_str("x").get_line(0), Some("x".into()));
        assert_eq!(Rope::from_str("x\n").get_line(0), Some("x\n".into()));
        assert_eq!(Rope::from_str("\nx").get_line(0), Some("\n".into()));
        assert_eq!(Rope::from_str("\nx").get_line(1), Some("x".into()));
        assert_eq!(Rope::from_str("x\ny\nz").get_line(0), Some("x\n".into()));
        assert_eq!(Rope::from_str("x\ny\nz").get_line(1), Some("y\n".into()));
        assert_eq!(Rope::from_str("x\ny\nz").get_line(2), Some("z".into()));
    }
}
