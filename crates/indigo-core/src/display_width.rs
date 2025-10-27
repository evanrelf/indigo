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
    if let Some('\t') = grapheme.chars().next() {
        return 8;
    }

    max(1, UnicodeWidthStr::width(grapheme))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test() {
        assert_eq!("".display_width(), 0);
        assert_eq!("‍".display_width(), 1); // zwj
        assert_eq!('\x00'.display_width(), 1); // null
        assert_eq!("\x00".display_width(), 1); // null
        assert_eq!("\u{200B}".display_width(), 1); // zws
        assert_eq!("abc".display_width(), 3);
        assert_eq!("🇯🇵".display_width(), 2);
        assert_eq!("👩🏻‍❤️‍💋‍👩🏻".display_width(), 2);
        assert_eq!('\t'.display_width(), 8);
        assert_eq!("\t".display_width(), 8);
        assert_eq!("\n".display_width(), 1);
    }
}
