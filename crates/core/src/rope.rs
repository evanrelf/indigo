mod graphemes_iter;
mod graphemes_step;

use crate::rope::graphemes_iter::{GraphemeBoundaries, Graphemes};
use anyhow::Context as _;
use ropey::{Rope, RopeSlice};

pub trait RopeExt {
    // `ropey` counts lines in a non-intuitive way, at least for my purposes. This method provides
    // an alternative, Indigo-specific line count.
    //
    // See the unit tests below for examples, and this GitHub issue for more info:
    // https://github.com/cessen/ropey/issues/60
    fn len_lines_indigo(&self) -> usize;

    fn char_index_to_grapheme_index(&self, char_index: usize) -> usize;

    fn get_grapheme(&self, char_index: usize) -> Option<RopeSlice>;

    fn grapheme(&self, char_index: usize) -> RopeSlice {
        self.get_grapheme(char_index).unwrap()
    }

    fn graphemes(&self) -> Graphemes<'_>;

    fn grapheme_boundaries(&self) -> GraphemeBoundaries<'_>;

    // TODO: Implement this, like `unicode-segmentation`:
    // https://docs.rs/unicode-segmentation/latest/unicode_segmentation/trait.UnicodeSegmentation.html#tymethod.grapheme_indices
    // fn grapheme_indices(&self) -> GraphemeIndices<'_>;

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

    fn get_grapheme(&self, char_index: usize) -> Option<RopeSlice> {
        if char_index < self.len_chars() {
            let start = char_index;
            let end = self.next_grapheme_boundary(start);
            Some(self.slice(start..end))
        } else {
            None
        }
    }

    fn graphemes(&self) -> Graphemes<'_> {
        Graphemes::new(self)
    }

    fn grapheme_boundaries(&self) -> GraphemeBoundaries<'_> {
        GraphemeBoundaries::new(self)
    }

    fn get_prev_grapheme_boundary(&self, char_index: usize) -> anyhow::Result<usize> {
        Ok(graphemes_step::prev_grapheme_boundary(self, char_index))
    }

    fn get_next_grapheme_boundary(&self, char_index: usize) -> anyhow::Result<usize> {
        graphemes_step::next_grapheme_boundary(self, char_index).context("")
    }

    fn try_is_grapheme_boundary(&self, char_index: usize) -> anyhow::Result<bool> {
        Ok(graphemes_step::is_grapheme_boundary(self, char_index))
    }
}

impl RopeExt for Rope {
    fn len_lines_indigo(&self) -> usize {
        self.slice(0..).len_lines_indigo()
    }

    fn char_index_to_grapheme_index(&self, char_index: usize) -> usize {
        self.slice(0..).char_index_to_grapheme_index(char_index)
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

    fn graphemes(&self) -> Graphemes<'_> {
        Graphemes::new(&self.slice(0..))
    }

    fn grapheme_boundaries(&self) -> GraphemeBoundaries<'_> {
        GraphemeBoundaries::new(&self.slice(0..))
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
    use arbtest::arbtest;

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
    fn grapheme_boundaries() {
        arbtest(|u| {
            let rope = Rope::from_str(u.arbitrary()?);
            let step: Vec<usize> = {
                let mut char_index = 0;
                let mut bs = vec![0];
                while let Ok(b) = rope.get_next_grapheme_boundary(char_index) {
                    char_index = b;
                    bs.push(b);
                }
                bs
            };
            let iter: Vec<usize> = rope.grapheme_boundaries().collect();
            assert_eq!(
                step, iter,
                "mismatched grapheme boundaries\nrope = {rope:?}"
            );
            Ok(())
        });
    }
}
