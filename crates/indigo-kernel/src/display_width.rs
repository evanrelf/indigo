use ropey::{Rope, RopeSlice};
use std::cmp::max;
use unicode_segmentation::UnicodeSegmentation as _;
use unicode_width::UnicodeWidthStr;

pub trait DisplayWidth {
    fn display_width(&self) -> usize;
}

impl DisplayWidth for char {
    fn display_width(&self) -> usize {
        str_width(self.encode_utf8(&mut [0; 4]))
    }
}

impl DisplayWidth for &str {
    fn display_width(&self) -> usize {
        str_width(self)
    }
}

impl DisplayWidth for RopeSlice<'_> {
    fn display_width(&self) -> usize {
        // Nearly every slice is one line inside one chunk, which needs no iterator or copy.
        let (chunk, _) = self.chunk(0);
        if chunk.len() == self.len() {
            return str_width(chunk);
        }
        str_width(&String::from(*self))
    }
}

impl DisplayWidth for Rope {
    fn display_width(&self) -> usize {
        self.slice(..).display_width()
    }
}

const TAB_WIDTH: usize = 8;

fn str_width(str: &str) -> usize {
    let bytes = str.as_bytes();
    if bytes.is_ascii() {
        return ascii_width(bytes);
    }
    // Two adjacent ASCII bytes always lie in different grapheme clusters, CRLF aside, and every
    // rule that looks back past a byte is reset by an ASCII byte. So cutting the string only
    // between such pairs hands the segmenter exactly the clusters it would find in the whole.
    let joined = |&a: &u8, &b: &u8| !(a.is_ascii() && b.is_ascii()) || (a == b'\r' && b == b'\n');
    let mut width = 0;
    let mut start = 0;
    for run in bytes.chunk_by(joined) {
        let end = start + run.len();
        width += if run.is_ascii() {
            ascii_width(run)
        } else {
            str[start..end].graphemes(true).map(grapheme_width).sum()
        };
        start = end;
    }
    width
}

/// Width of ASCII text, where every byte is its own grapheme except a CRLF pair.
fn ascii_width(bytes: &[u8]) -> usize {
    let tabs = bytes
        .iter()
        .map(|&byte| usize::from(byte == b'\t'))
        .sum::<usize>();
    let crlfs = bytes.windows(2).filter(|pair| pair == b"\r\n").count();
    bytes.len() + (TAB_WIDTH - 1) * tabs - crlfs
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
