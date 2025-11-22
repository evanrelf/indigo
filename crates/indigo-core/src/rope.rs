use crate::grapheme::{self, Graphemes};
use ropey::{Rope, RopeSlice};
use std::ops::{Bound, RangeBounds};

pub trait RopeExt {
    fn as_slice(&self) -> RopeSlice<'_>;

    // `ropey` counts lines in a non-intuitive way, at least for my purposes. This method provides
    // an alternative, Indigo-specific line count.
    //
    // See the unit tests below for examples, and this GitHub issue for more info:
    // https://github.com/cessen/ropey/issues/60
    #[expect(clippy::bool_to_int_with_if)]
    fn len_lines_indigo(&self) -> usize {
        let rope = self.as_slice();
        if rope.len_chars() == 0 {
            return 0;
        }
        let last_char = rope.char(rope.len_chars() - 1);
        rope.len_lines() - if last_char == '\n' { 1 } else { 0 }
    }

    fn find_next_byte<R>(&self, char_range: R, byte: u8) -> Option<usize>
    where
        R: RangeBounds<usize> + Clone,
    {
        let rope = self.as_slice().slice(char_range.clone());
        let haystack = rope.chunks().map(|c| c.as_bytes());
        let start = match char_range.start_bound() {
            Bound::Included(n) => *n,
            Bound::Excluded(_) => unreachable!("wtf"),
            Bound::Unbounded => 0,
        };
        Some(start + rope.byte_to_char(memchr(byte, haystack)?))
    }

    fn find_prev_byte<R>(&self, char_range: R, byte: u8) -> Option<usize>
    where
        R: RangeBounds<usize> + Clone,
    {
        let rope = self.as_slice().slice(char_range.clone());
        let haystack = rope
            .chunks_at_char(rope.len_chars())
            .0
            .reversed()
            .map(|c| c.as_bytes());
        let end = match char_range.end_bound() {
            Bound::Included(n) => *n,
            Bound::Excluded(n) => rope.byte_to_char(rope.char_to_byte(*n).saturating_sub(1)),
            Bound::Unbounded => rope.len_chars().saturating_sub(1),
        };
        Some(end - rope.byte_to_char(memrchr(byte, haystack)?))
    }

    fn find_next_byte3<R>(&self, char_range: R, byte1: u8, byte2: u8, byte3: u8) -> Option<usize>
    where
        R: RangeBounds<usize> + Clone,
    {
        let rope = self.as_slice().slice(char_range.clone());
        let haystack = rope.chunks().map(|c| c.as_bytes());
        let start = match char_range.start_bound() {
            Bound::Included(n) => *n,
            Bound::Excluded(_) => unreachable!("wtf"),
            Bound::Unbounded => 0,
        };
        Some(start + rope.byte_to_char(memchr3(byte1, byte2, byte3, haystack)?))
    }

    fn find_prev_byte3<R>(&self, char_range: R, byte1: u8, byte2: u8, byte3: u8) -> Option<usize>
    where
        R: RangeBounds<usize> + Clone,
    {
        let rope = self.as_slice().slice(char_range.clone());
        let haystack = rope
            .chunks_at_char(rope.len_chars())
            .0
            .reversed()
            .map(|c| c.as_bytes());
        let end = match char_range.end_bound() {
            Bound::Included(n) => *n,
            Bound::Excluded(n) => rope.byte_to_char(rope.char_to_byte(*n).saturating_sub(1)),
            Bound::Unbounded => rope.len_chars().saturating_sub(1),
        };
        Some(end - rope.byte_to_char(memrchr3(byte1, byte2, byte3, haystack)?))
    }

    fn get_grapheme(&self, char_index: usize) -> Option<RopeSlice<'_>> {
        let rope = self.as_slice();
        let start = char_index;
        rope.next_grapheme_boundary(start)
            .map(|end| rope.slice(start..end))
    }

    fn grapheme(&self, char_index: usize) -> RopeSlice<'_> {
        self.get_grapheme(char_index).unwrap()
    }

    fn graphemes(&self) -> Graphemes<'_> {
        Graphemes::new(&self.as_slice())
    }

    fn is_grapheme_boundary(&self, char_offset: usize) -> bool {
        grapheme::is_grapheme_boundary(&self.as_slice(), char_offset)
    }

    fn prev_grapheme_boundary(&self, char_offset: usize) -> Option<usize> {
        grapheme::prev_grapheme_boundary(&self.as_slice(), char_offset)
    }

    fn next_grapheme_boundary(&self, char_offset: usize) -> Option<usize> {
        grapheme::next_grapheme_boundary(&self.as_slice(), char_offset)
    }

    fn floor_grapheme_boundary(&self, char_offset: usize) -> usize {
        grapheme::floor_grapheme_boundary(&self.as_slice(), char_offset)
    }

    fn ceil_grapheme_boundary(&self, char_offset: usize) -> usize {
        grapheme::ceil_grapheme_boundary(&self.as_slice(), char_offset)
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

/// Variant of [`memchr::memchr()`] for a discontiguous haystack.
pub fn memchr<'a>(needle: u8, haystacks: impl IntoIterator<Item = &'a [u8]>) -> Option<usize> {
    let mut haystacks_byte_index = 0;
    for haystack in haystacks {
        if let Some(needle_byte_index) =
            memchr::memchr(needle, haystack).map(|i| haystacks_byte_index + i)
        {
            return Some(needle_byte_index);
        }
        haystacks_byte_index += haystack.len();
    }
    None
}

/// Variant of [`memchr::memrchr()`] for a discontiguous haystack.
///
/// Assumes byte slices are provided in reverse order, and returned byte index is from the back. For
/// example:
///
/// ```rust
/// # use indigo_core::rope::memrchr;
/// let haystacks = ["123", "456"].iter().rev().map(|s| s.as_bytes());
/// assert_eq!(memrchr(b'4', haystacks), Some(2));
/// ```
pub fn memrchr<'a>(needle: u8, haystacks: impl IntoIterator<Item = &'a [u8]>) -> Option<usize> {
    let mut haystacks_byte_index = 0; // Starting from the end
    for haystack in haystacks {
        if let Some(needle_byte_index) = memchr::memrchr(needle, haystack)
            .map(|i| haystacks_byte_index + (haystack.len() - (i + 1)))
        {
            return Some(needle_byte_index);
        }
        haystacks_byte_index += haystack.len();
    }
    None
}

/// Variant of [`memchr::memchr3()`] for a discontiguous haystack. See [`memchr()`] for more info.
pub fn memchr3<'a>(
    needle1: u8,
    needle2: u8,
    needle3: u8,
    haystacks: impl IntoIterator<Item = &'a [u8]>,
) -> Option<usize> {
    let mut haystacks_byte_index = 0;
    for haystack in haystacks {
        if let Some(needle_byte_index) =
            memchr::memchr3(needle1, needle2, needle3, haystack).map(|i| haystacks_byte_index + i)
        {
            return Some(needle_byte_index);
        }
        haystacks_byte_index += haystack.len();
    }
    None
}

/// Variant of [`memchr::memrchr3()`] for a discontiguous haystack. See [`memrchr()`] for more info.
pub fn memrchr3<'a>(
    needle1: u8,
    needle2: u8,
    needle3: u8,
    haystacks: impl IntoIterator<Item = &'a [u8]>,
) -> Option<usize> {
    let mut haystacks_byte_index = 0; // Starting from the end
    for haystack in haystacks {
        if let Some(needle_byte_index) = memchr::memrchr3(needle1, needle2, needle3, haystack)
            .map(|i| haystacks_byte_index + (haystack.len() - (i + 1)))
        {
            return Some(needle_byte_index);
        }
        haystacks_byte_index += haystack.len();
    }
    None
}

#[cfg(test)]
mod tests {
    use super::*;

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
        assert_eq!(Rope::from_str("x\ny\r\nz").get_line(0), Some("x\n".into()));
        assert_eq!(
            Rope::from_str("x\ny\r\nz").get_line(1),
            Some("y\r\n".into())
        );
        assert_eq!(Rope::from_str("x\ny\r\nz").get_line(2), Some("z".into()));
    }

    #[test]
    fn test_memchr() {
        let haystacks = || ["foo", "bar", "baz", "qux"].iter().map(|s| s.as_bytes());
        assert_eq!(memchr(b'0', haystacks()), None);
        assert_eq!(memchr(b'f', haystacks()), Some(0));
        assert_eq!(memchr(b'o', haystacks()), Some(1));
        assert_eq!(memchr(b'a', haystacks()), Some(4));
        assert_eq!(memchr(b'x', haystacks()), Some(11));
    }

    #[test]
    fn test_memrchr() {
        let haystacks = || {
            ["foo", "bar", "baz", "qux"]
                .iter()
                .rev()
                .map(|s| s.as_bytes())
        };
        assert_eq!(memrchr(b'0', haystacks()), None);
        assert_eq!(memrchr(b'f', haystacks()), Some(11));
        assert_eq!(memrchr(b'o', haystacks()), Some(9));
        assert_eq!(memrchr(b'a', haystacks()), Some(4));
        assert_eq!(memrchr(b'x', haystacks()), Some(0));
    }

    #[test]
    fn test_find_next_byte() {
        let mut rope = Rope::new();
        while rope.chunks().count() < 10 {
            rope.append(Rope::from("hello"));
        }
        let mut length = rope.len_chars();
        let char_index = length / 2;
        rope.insert(char_index, "!");
        length += 1;
        assert_eq!(rope.find_next_byte(.., b'!'), Some(char_index));
        assert_eq!(
            rope.find_next_byte(0..(length / 3) * 2, b'!'),
            Some(char_index)
        );
        assert_eq!(rope.find_next_byte(0..length / 3, b'!'), None);
    }

    #[test]
    fn test_find_prev_byte() {
        let mut rope = Rope::new();
        while rope.chunks().count() < 10 {
            rope.append(Rope::from("hello"));
        }
        let mut length = rope.len_chars();
        let char_index = length / 2;
        rope.insert(char_index, "!");
        length += 1;
        assert_eq!(rope.find_prev_byte(.., b'!'), Some(char_index));
        assert_eq!(
            rope.find_prev_byte(0..(length / 3) * 2, b'!'),
            Some(char_index)
        );
        assert_eq!(rope.find_prev_byte(0..length / 3, b'!'), None);
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
}
