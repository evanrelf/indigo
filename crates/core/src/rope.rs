mod graphemes_iter;
mod graphemes_step;

use crate::rope::graphemes_iter::{GraphemeBoundaries, Graphemes};
use ropey::{Rope, RopeSlice};

#[derive(Clone, Copy)]
pub enum Bias {
    Before,
    After,
}

pub trait RopeExt {
    fn as_slice(&self) -> RopeSlice<'_>;

    // `ropey` counts lines in a non-intuitive way, at least for my purposes. This method provides
    // an alternative, Indigo-specific line count.
    //
    // See the unit tests below for examples, and this GitHub issue for more info:
    // https://github.com/cessen/ropey/issues/60
    fn len_lines_indigo(&self) -> usize {
        let rope = self.as_slice();
        if rope.len_chars() == 0 {
            return 0;
        }
        let last_char = rope.char(rope.len_chars() - 1);
        rope.len_lines() - if last_char == '\n' { 1 } else { 0 }
    }

    fn byte_index_to_grapheme_index(&self, byte_index: usize) -> Option<usize> {
        let char_index = self.as_slice().try_byte_to_char(byte_index).ok()?;
        Some(self.as_slice().slice(..char_index).graphemes().count())
    }

    fn char_index_to_grapheme_index(&self, char_index: usize) -> Option<usize> {
        Some(self.as_slice().get_slice(..char_index)?.graphemes().count())
    }

    fn get_grapheme(&self, byte_index: usize) -> Option<RopeSlice> {
        let rope = self.as_slice();
        let start = byte_index;
        let end = rope.next_grapheme_boundary(start)?;
        // TODO: Add byte slice method
        let start = rope.try_byte_to_char(start).ok()?;
        let end = rope.byte_to_char(end);
        Some(rope.slice(start..end))
    }

    fn grapheme(&self, byte_index: usize) -> RopeSlice {
        self.get_grapheme(byte_index).unwrap()
    }

    fn graphemes(&self) -> Graphemes<'_> {
        Graphemes::new(&self.as_slice())
    }

    fn grapheme_boundaries(&self) -> GraphemeBoundaries<'_> {
        GraphemeBoundaries::new(&self.as_slice())
    }

    fn prev_grapheme_boundary(&self, byte_index: usize) -> Option<usize> {
        graphemes_step::byte_prev_grapheme_boundary(&self.as_slice(), byte_index)
    }

    fn next_grapheme_boundary(&self, byte_index: usize) -> Option<usize> {
        graphemes_step::byte_next_grapheme_boundary(&self.as_slice(), byte_index)
    }

    fn is_grapheme_boundary(&self, byte_index: usize) -> bool {
        graphemes_step::byte_is_grapheme_boundary(&self.as_slice(), byte_index)
    }

    fn snap_to_grapheme_boundary(&self, byte_index: usize, bias: Bias) -> usize {
        let length = self.as_slice().len_bytes();
        if byte_index > length {
            return length;
        }
        if self.is_grapheme_boundary(byte_index) {
            return byte_index;
        }
        match bias {
            Bias::Before => self.prev_grapheme_boundary(byte_index).unwrap(),
            Bias::After => self.next_grapheme_boundary(byte_index).unwrap(),
        }
    }
}

impl RopeExt for RopeSlice<'_> {
    fn as_slice(&self) -> RopeSlice<'_> {
        *self
    }
}

impl RopeExt for Rope {
    fn as_slice(&self) -> RopeSlice<'_> {
        self.slice(0..)
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
    fn graphemes() {
        let a = |text: &str| {
            Rope::from_str(text)
                .graphemes()
                .map(String::from)
                .collect::<Vec<_>>()
        };
        let e = |graphemes: &'static [&str]| {
            graphemes
                .iter()
                .copied()
                .map(String::from)
                .collect::<Vec<_>>()
        };
        assert_eq!(a(""), e(&[]));
        assert_eq!(a("hello"), e(&["h", "e", "l", "l", "o"]));
        assert_eq!(a("x\t\n🇯🇵👨‍👨‍👧"), e(&["x", "\t", "\n", "🇯🇵", "👨‍👨‍👧"]));
    }

    #[test]
    fn grapheme_boundaries() {
        arbtest(|u| {
            let rope = Rope::from_str(u.arbitrary()?);
            let step: Vec<usize> = {
                let mut byte_index = 0;
                let mut bs = vec![0];
                while let Some(b) = rope.next_grapheme_boundary(byte_index) {
                    byte_index = b;
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

    #[test]
    fn grapheme_counts() {
        arbtest(|u| {
            let rope = Rope::from_str(u.arbitrary()?);
            assert_eq!(
                rope.grapheme_boundaries().count(),
                rope.graphemes().count() + 1,
                "mismatched grapheme boundary counts\nrope = {rope:?}"
            );
            Ok(())
        });
    }
}
