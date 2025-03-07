use crate::graphemes::{self, GraphemeBoundaries, Graphemes};
use ropey::{Rope, RopeSlice};

#[derive(Clone, Copy)]
pub enum SnapBias {
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

    fn char_index_to_grapheme_index(&self, char_index: usize) -> usize {
        self.as_slice().slice(..char_index).graphemes().count()
    }

    fn get_grapheme(&self, char_index: usize) -> Option<RopeSlice> {
        let rope = self.as_slice();
        let start = char_index;
        rope.next_grapheme_boundary(start)
            .map(|end| rope.slice(start..end))
    }

    fn grapheme(&self, char_index: usize) -> RopeSlice {
        self.get_grapheme(char_index).unwrap()
    }

    fn graphemes(&self) -> Graphemes<'_> {
        Graphemes::new(&self.as_slice())
    }

    fn grapheme_boundaries(&self) -> GraphemeBoundaries<'_> {
        GraphemeBoundaries::new(&self.as_slice())
    }

    fn prev_grapheme_boundary(&self, char_index: usize) -> Option<usize> {
        graphemes::prev_grapheme_boundary(&self.as_slice(), char_index)
    }

    fn next_grapheme_boundary(&self, char_index: usize) -> Option<usize> {
        graphemes::next_grapheme_boundary(&self.as_slice(), char_index)
    }

    fn is_grapheme_boundary(&self, char_index: usize) -> bool {
        graphemes::is_grapheme_boundary(&self.as_slice(), char_index)
    }

    fn snap_to_grapheme_boundary(&self, char_index: usize, bias: SnapBias) -> usize {
        match bias {
            SnapBias::Before => self.floor_grapheme_boundary(char_index),
            SnapBias::After => self.ceil_grapheme_boundary(char_index),
        }
    }

    fn floor_grapheme_boundary(&self, char_index: usize) -> usize {
        graphemes::floor_grapheme_boundary(&self.as_slice(), char_index)
    }

    fn ceil_grapheme_boundary(&self, char_index: usize) -> usize {
        graphemes::ceil_grapheme_boundary(&self.as_slice(), char_index)
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
                let mut char_index = 0;
                let mut bs = vec![0];
                while let Some(b) = rope.next_grapheme_boundary(char_index) {
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
