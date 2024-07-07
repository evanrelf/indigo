use crate::RopeExt as _;
use ropey::{Rope, RopeSlice};
use std::{borrow::Cow, cmp::max};
use unicode_width::UnicodeWidthStr;

pub trait DisplayWidth {
    fn display_width(&self) -> usize;
}

impl DisplayWidth for RopeSlice<'_> {
    fn display_width(&self) -> usize {
        rope_slice_display_width(self)
    }
}

impl DisplayWidth for Rope {
    fn display_width(&self) -> usize {
        self.slice(..).display_width()
    }
}

fn rope_slice_display_width(slice: &RopeSlice) -> usize {
    fn grapheme_width(grapheme: RopeSlice) -> usize {
        if let Some('\t') = grapheme.get_char(0) {
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

        let cow = Cow::<str>::from(grapheme);
        let str = cow.as_ref();

        // for char in str.chars() {
        //     if EMOJI_MODIFIER_BASE.contains(char) || EMOJI_MODIFIER.contains(char) {
        //         // panic!("grapheme: {}, char: {}", str, char);
        //         return 2;
        //     }
        // }

        max(1, UnicodeWidthStr::width(str))
    }

    slice.graphemes().map(grapheme_width).sum()
}
