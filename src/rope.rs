pub mod graphemes_iter;
pub mod graphemes_step;

use crate::rope::graphemes_iter::RopeGraphemes;
use ropey::{Rope, RopeSlice};

pub trait RopeExt {
    // `ropey` counts lines in a non-intuitive way, at least for my purposes. This method provides
    // an alternative, Indigo-specific line count.
    //
    // See the unit tests below for examples, and this GitHub issue for more info:
    // https://github.com/cessen/ropey/issues/60
    fn len_lines_indigo(&self) -> usize;

    fn graphemes(&self) -> RopeGraphemes<'_>;
}

impl RopeExt for RopeSlice<'_> {
    fn len_lines_indigo(&self) -> usize {
        if self.len_chars() == 0 {
            return 0;
        }
        let last_char = self.char(self.len_chars() - 1);
        self.len_lines() - if last_char == '\n' { 1 } else { 0 }
    }

    fn graphemes(&self) -> RopeGraphemes<'_> {
        RopeGraphemes::new(self)
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

    fn graphemes(&self) -> RopeGraphemes<'_> {
        RopeGraphemes::new(&self.slice(0..))
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
