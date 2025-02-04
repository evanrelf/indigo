mod graphemes_iter;
mod graphemes_step;

use crate::rope::graphemes_iter::RopeGraphemes;
use ropey::{Rope, RopeSlice};

// (x, y) or (column, line) is kind of a misnomer because characters in a rope have variable width
// (could be a ZWSP or ZWJ, could be an emoji, could be only part of a multi-char grapheme, etc).
#[derive(Clone, Copy)]
pub struct RopePosition {
    /// Line index
    pub y: usize,
    /// Char offset in line (e.g. 0 is the first character of the line)
    pub x: usize,
}

pub trait RopeExt {
    // `ropey` counts lines in a non-intuitive way, at least for my purposes. This method provides
    // an alternative, Indigo-specific line count.
    //
    // See the unit tests below for examples, and this GitHub issue for more info:
    // https://github.com/cessen/ropey/issues/60
    fn len_lines_indigo(&self) -> usize;

    fn char_index_to_grapheme_index(&self, char_index: usize) -> usize;

    // Does not snap to graphemes.
    fn char_index_to_position(
        &self,
        char_index: usize,
    ) -> Option<Result<RopePosition, RopePosition>>;

    // Does not snap to graphemes.
    fn position_to_char_index(&self, position: RopePosition) -> Result<usize, usize>;

    fn get_grapheme(&self, char_index: usize) -> Option<RopeSlice>;

    fn grapheme(&self, char_index: usize) -> RopeSlice {
        self.get_grapheme(char_index).unwrap()
    }

    fn graphemes(&self) -> RopeGraphemes<'_>;

    // TODO: Implement this, like `unicode-segmentation`:
    // https://docs.rs/unicode-segmentation/latest/unicode_segmentation/trait.UnicodeSegmentation.html#tymethod.grapheme_indices
    // fn grapheme_indices(&self) -> RopeGraphemeIndices<'_>;

    fn prev_grapheme_boundary(&self, char_index: usize) -> usize {
        self.get_prev_grapheme_boundary(char_index).unwrap()
    }

    fn get_prev_grapheme_boundary(&self, char_index: usize) -> anyhow::Result<usize>;

    fn next_grapheme_boundary(&self, char_index: usize) -> usize {
        self.get_next_grapheme_boundary(char_index).unwrap()
    }

    fn get_next_grapheme_boundary(&self, char_index: usize) -> anyhow::Result<usize>;

    fn is_grapheme_boundary(&self, char_index: usize) -> bool {
        self.try_is_grapheme_boundary(char_index).unwrap()
    }

    fn try_is_grapheme_boundary(&self, char_index: usize) -> anyhow::Result<bool>;
}

impl RopeExt for RopeSlice<'_> {
    fn len_lines_indigo(&self) -> usize {
        if self.len_chars() == 0 {
            return 0;
        }
        let last_char = self.char(self.len_chars() - 1);
        self.len_lines() - if last_char == '\n' { 1 } else { 0 }
    }

    fn char_index_to_grapheme_index(&self, char_index: usize) -> usize {
        self.slice(..char_index).graphemes().count()
    }

    fn char_index_to_position(
        &self,
        mut char_index: usize,
    ) -> Option<Result<RopePosition, RopePosition>> {
        let mut corrected = false;

        if self.len_chars() == 0 {
            return None;
        }

        if self.len_chars() <= char_index {
            corrected = true;
            // When index goes beyond end of rope, use last index
            char_index = self.len_chars().saturating_sub(1);
        }

        let y = self.char_to_line(char_index);

        let x = char_index - self.line_to_char(y);

        let position = RopePosition { y, x };

        if corrected {
            Some(Err(position))
        } else {
            Some(Ok(position))
        }
    }

    fn position_to_char_index(&self, position: RopePosition) -> Result<usize, usize> {
        let RopePosition { y, x } = position;

        let Some(line) = self.get_line(y) else {
            // Position goes beyond last line of rope, so we snap to last character of rope
            return Err(self.len_chars());
        };

        let line_char_index = self
            .try_line_to_char(y)
            .expect("Line is known to exist at this point");

        let line_length = line.len_chars();

        if x > line_length {
            // Position goes beyond last character of line, so we snap to last character of line
            return Err(line_char_index + line_length.saturating_sub(1));
        }

        Ok(line_char_index + x)
    }

    fn get_grapheme(&self, char_index: usize) -> Option<RopeSlice> {
        if char_index < self.len_chars() {
            let start = char_index;
            let end = self.next_grapheme_boundary(start);
            Some(self.slice(start..end))
        } else {
            None
        }
    }

    fn graphemes(&self) -> RopeGraphemes<'_> {
        RopeGraphemes::new(self)
    }

    fn get_prev_grapheme_boundary(&self, char_index: usize) -> anyhow::Result<usize> {
        graphemes_step::get_prev_grapheme_boundary(self, char_index)
    }

    fn get_next_grapheme_boundary(&self, char_index: usize) -> anyhow::Result<usize> {
        graphemes_step::get_next_grapheme_boundary(self, char_index)
    }

    fn try_is_grapheme_boundary(&self, char_index: usize) -> anyhow::Result<bool> {
        graphemes_step::try_is_grapheme_boundary(self, char_index)
    }
}

impl RopeExt for Rope {
    fn len_lines_indigo(&self) -> usize {
        if self.len_chars() == 0 {
            return 0;
        }
        let last_char = self.char(self.len_chars() - 1);
        self.len_lines() - if last_char == '\n' { 1 } else { 0 }
    }

    fn char_index_to_grapheme_index(&self, char_index: usize) -> usize {
        self.slice(..char_index).graphemes().count()
    }

    fn char_index_to_position(
        &self,
        char_index: usize,
    ) -> Option<Result<RopePosition, RopePosition>> {
        self.slice(0..).char_index_to_position(char_index)
    }

    fn position_to_char_index(&self, position: RopePosition) -> Result<usize, usize> {
        self.slice(0..).position_to_char_index(position)
    }

    fn get_grapheme(&self, char_index: usize) -> Option<RopeSlice> {
        if char_index < self.len_chars() {
            let start = char_index;
            let end = self.next_grapheme_boundary(start);
            Some(self.slice(start..end))
        } else {
            None
        }
    }

    fn graphemes(&self) -> RopeGraphemes<'_> {
        RopeGraphemes::new(&self.slice(0..))
    }

    fn get_prev_grapheme_boundary(&self, char_index: usize) -> anyhow::Result<usize> {
        self.slice(0..).get_prev_grapheme_boundary(char_index)
    }

    fn get_next_grapheme_boundary(&self, char_index: usize) -> anyhow::Result<usize> {
        self.slice(0..).get_next_grapheme_boundary(char_index)
    }

    fn try_is_grapheme_boundary(&self, char_index: usize) -> anyhow::Result<bool> {
        self.slice(0..).try_is_grapheme_boundary(char_index)
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

    #[test]
    fn rope_position_to_char_index() {
        let f = |s, y, x| Rope::from_str(s).position_to_char_index(RopePosition { y, x });
        assert_eq!(f("", 0, 0), Ok(0));
        assert_eq!(f("", 1, 0), Err(0));
        assert_eq!(f("", 0, 1), Err(0));
        assert_eq!(f("x\ny", 1, 0), Ok(2));
    }

    #[test]
    fn rope_char_index_to_position() {
        // todo!()
    }
}
