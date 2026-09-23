use crate::grapheme::Graphemes;
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
        if let Some(width) = display_width_fast(self.as_bytes()) {
            return width;
        }
        self.graphemes(true).map(grapheme_width).sum()
    }
}

impl DisplayWidth for RopeSlice<'_> {
    fn display_width(&self) -> usize {
        // Chunk widths are additive because the fast path admits no grapheme that could span a
        // chunk boundary.
        if let Some(width) = self
            .chunks()
            .map(|chunk| display_width_fast(chunk.as_bytes()))
            .sum()
        {
            return width;
        }
        Graphemes::new(self)
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

const TAB_WIDTH: usize = 8;

fn display_width_fast(bytes: &[u8]) -> Option<usize> {
    let mut width = 0;
    for byte in bytes {
        match byte {
            b'\t' => width += TAB_WIDTH,
            0x20..=0x7E => width += 1,
            _ => return None,
        }
    }
    Some(width)
}

fn grapheme_width(grapheme: &str) -> usize {
    if let Some('\t') = grapheme.chars().next() {
        return TAB_WIDTH;
    }
    max(1, UnicodeWidthStr::width(grapheme))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_display_width() {
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
        // Boundary cases for the printable ASCII + tab fast path.
        assert_eq!(" ~".display_width(), 2); // 0x20 and 0x7E, the fast path's edges
        assert_eq!("\x7f".display_width(), 1); // DEL, just past the fast path
        assert_eq!("a\tb".display_width(), 10); // tabs stay on the fast path, 8 columns each
        assert_eq!("\t\t".display_width(), 16);
        assert_eq!("\x08".display_width(), 1); // backspace, the control char next to tab
        // An ASCII letter and a combining mark form one grapheme; the ASCII prefix must not be
        // counted separately from it.
        assert_eq!("abce\u{0301}".display_width(), 4);
    }

    #[test]
    fn display_width_fast_path_matches_graphemes_across_chunks() {
        // Build ropes large enough to span multiple chunks, with and without non-ASCII content,
        // and check the `RopeSlice` impl agrees with the grapheme-by-grapheme `&str` impl.
        for text in ["ascii only ", "ascii\tand\ttabs ", "mixed e\u{301} 🇯🇵 \t "] {
            let string = text.repeat(10_000 / text.len() + 1);
            let rope = Rope::from_str(&string);
            assert!(rope.chunks().count() > 1);
            let expected: usize = string.graphemes(true).map(grapheme_width).sum();
            assert_eq!(rope.slice(..).display_width(), expected);
            assert_eq!(string.as_str().display_width(), expected);
        }
    }
}
