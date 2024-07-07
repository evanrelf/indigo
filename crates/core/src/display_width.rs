use crate::RopeExt as _;
use ropey::{Rope, RopeSlice};
use std::{borrow::Cow, cmp::max};
use unicode_segmentation::UnicodeSegmentation as _;
use unicode_width::UnicodeWidthStr;

pub trait DisplayWidth {
    fn display_width(&self) -> usize;
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

    // TODO: Implement custom width to accomodate emoji with zero-width joiners.
    // `unicode-width` doesn't support this at the moment:
    // https://github.com/unicode-rs/unicode-width/issues/4.
    //
    // WezTerm dealt with this here: https://github.com/wez/wezterm/commit/1ab438c1e266ab24.

    // const EMOJI_MODIFIER_BASE: icu_properties::sets::CodePointSetDataBorrowed =
    //     icu_properties::sets::emoji_modifier_base();

    // const EMOJI_MODIFIER: icu_properties::sets::CodePointSetDataBorrowed =
    //     icu_properties::sets::emoji_modifier();

    // for char in str.chars() {
    //     if EMOJI_MODIFIER_BASE.contains(char) || EMOJI_MODIFIER.contains(char) {
    //         // panic!("grapheme: {}, char: {}", str, char);
    //         return 2;
    //     }
    // }

    max(1, UnicodeWidthStr::width(grapheme))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test() {
        let s = "";
        assert_eq!(s.display_width(), 0);

        let s = "\x00"; // null
        assert_eq!(s.display_width(), 1);

        let s = "\u{200B}"; // zws
        assert_eq!(s.display_width(), 1);

        let s = "abc";
        assert_eq!(s.display_width(), 3);

        let s = "🇯🇵";
        assert_eq!(s.display_width(), 2);

        let s = "👩🏻‍❤️‍💋‍👩🏻";
        assert_eq!(s.display_width(), 12); // TODO: I want this to be 2 :(

        let s = "\t";
        assert_eq!(s.display_width(), 8);

        let s = "\n";
        assert_eq!(s.display_width(), 1);
    }
}
