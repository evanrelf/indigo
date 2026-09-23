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
    use hegel::{TestCase, generators as gs};
    use std::str;

    fn oracle(str: &str) -> usize {
        str.graphemes(true).map(grapheme_width).sum()
    }

    fn text_gen() -> gs::TextGenerator {
        /// Exercise every grapheme rule that could interact with ASCII.
        const ALPHABET: &str = "ab1 \t\r\n\x00\x7f\u{0301}\u{200D}\u{FE0F}\u{20E3}\u{0600}\u{0602}🇯🇵👩❤💋\u{1F3FB}中é\u{1100}\u{1161}\u{0915}\u{094D}\u{0937}";
        gs::text().alphabet(ALPHABET).max_size(64)
    }

    #[hegel::test(test_cases = 20_000)]
    fn str_width_matches_oracle(tc: TestCase) {
        let text: String = tc.draw(text_gen());
        assert_eq!(text.as_str().display_width(), oracle(&text));
        for char in text.chars() {
            assert_eq!(char.display_width(), oracle(char.encode_utf8(&mut [0; 4])));
        }
    }

    #[hegel::test(test_cases = 10_000)]
    fn rope_slice_width_matches_oracle(tc: TestCase) {
        // Repeat the text enough to span several chunks, then measure random sub-slices so that
        // chunk boundaries land at arbitrary points inside the text.
        let text: String = tc.draw(text_gen().min_size(1));
        let repeats = tc.draw(gs::integers::<usize>().min_value(1).max_value(4_000));
        let string = text.repeat(repeats);
        let rope = Rope::from_str(&string);
        let start = tc.draw(gs::integers::<usize>().max_value(string.len()));
        let end = tc.draw(
            gs::integers::<usize>()
                .min_value(start)
                .max_value(string.len()),
        );
        let start = str::floor_char_boundary(&string, start);
        let end = str::floor_char_boundary(&string, end);
        assert_eq!(
            rope.slice(start..end).display_width(),
            oracle(&string[start..end])
        );
    }
}
