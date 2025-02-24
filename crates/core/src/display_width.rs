use crate::rope::RopeExt as _;
use ropey::{Rope, RopeSlice};
use std::{borrow::Cow, cmp::max};
use unicode_segmentation::UnicodeSegmentation as _;
use unicode_width::UnicodeWidthStr;

pub trait DisplayWidth {
    fn display_width(&self) -> usize;
}

impl DisplayWidth for char {
    fn display_width(&self) -> usize {
        self.to_string().as_str().display_width()
    }
}

impl DisplayWidth for &str {
    fn display_width(&self) -> usize {
        self.graphemes(true).map(grapheme_width).sum()
    }
}

impl DisplayWidth for RopeSlice<'_> {
    fn display_width(&self) -> usize {
        self.graphemes()
            .map(|grapheme| {
                let cow = Cow::<str>::from(grapheme);
                let str = cow.as_ref();
                grapheme_width(str)
            })
            .sum()
    }
}

impl DisplayWidth for Rope {
    fn display_width(&self) -> usize {
        self.slice(..).display_width()
    }
}

fn grapheme_width(grapheme: &str) -> usize {
    if let Some(b'\t') = grapheme.as_bytes().first() {
        return 8;
    }

    max(1, UnicodeWidthStr::width(grapheme))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test() {
        let s = "";
        assert_eq!(s.display_width(), 0);

        let c = '\x00'; // null
        assert_eq!(c.display_width(), 1);

        let s = "\x00"; // null
        assert_eq!(s.display_width(), 1);

        let s = "\u{200B}"; // zws
        assert_eq!(s.display_width(), 1);

        let s = "abc";
        assert_eq!(s.display_width(), 3);

        let s = "🇯🇵";
        assert_eq!(s.display_width(), 2);

        let s = "👩🏻‍❤️‍💋‍👩🏻";
        assert_eq!(s.display_width(), 2);

        let c = '\t';
        assert_eq!(c.display_width(), 8);

        let s = "\t";
        assert_eq!(s.display_width(), 8);

        let s = "\n";
        assert_eq!(s.display_width(), 1);
    }
}
