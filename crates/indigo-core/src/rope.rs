use crate::{
    display_width::DisplayWidth as _,
    unicode::{self, Graphemes},
};
use regex_cursor::{Cursor, IntoCursor};
use ropey::{LineType, Rope, RopeSlice};
use std::ops::{Bound, RangeBounds};

pub const LINE_TYPE: LineType = LineType::LF_CR;

pub trait RopeExt {
    fn as_slice(&self) -> RopeSlice<'_>;

    // `ropey` counts lines in a non-intuitive way, at least for my purposes. This method provides
    // an alternative, Indigo-specific line count.
    //
    // See the unit tests below for examples, and this GitHub issue for more info:
    // https://github.com/cessen/ropey/issues/60
    fn len_lines_indigo(&self) -> usize {
        let rope = self.as_slice();
        if rope.len() == 0 {
            return 0;
        }
        let last_byte = rope.byte(rope.len() - 1);
        rope.len_lines(LINE_TYPE) - if last_byte == b'\n' { 1 } else { 0 }
    }

    fn find_next_byte<R>(&self, byte_range: R, needles: &[u8]) -> Option<usize>
    where
        R: RangeBounds<usize> + Clone,
    {
        if needles.is_empty() {
            return None;
        }

        let rope = self.as_slice().slice(byte_range.clone());

        let start = match byte_range.start_bound() {
            Bound::Included(n) => *n,
            Bound::Excluded(_) => unreachable!("wtf"),
            Bound::Unbounded => 0,
        };

        let mut needle_index: Option<usize> = None;

        for chunk in needles.chunks(3) {
            match chunk.len() {
                3 => {
                    let haystack = rope.chunks().map(|c| c.as_bytes());
                    if let Some(pos) = memchr3(chunk[0], chunk[1], chunk[2], haystack) {
                        needle_index = Some(needle_index.map_or(pos, |current| current.min(pos)));
                    }
                }
                _ => {
                    for &needle in chunk {
                        let haystack = rope.chunks().map(|c| c.as_bytes());
                        if let Some(pos) = memchr(needle, haystack) {
                            needle_index =
                                Some(needle_index.map_or(pos, |current| current.min(pos)));
                        }
                    }
                }
            }
        }

        needle_index.map(|byte_index| start + byte_index)
    }

    fn find_prev_byte<R>(&self, byte_range: R, needles: &[u8]) -> Option<usize>
    where
        R: RangeBounds<usize> + Clone,
    {
        if needles.is_empty() {
            return None;
        }

        let rope = self.as_slice().slice(byte_range.clone());

        let end = match byte_range.end_bound() {
            Bound::Included(n) => *n,
            Bound::Excluded(n) => n.saturating_sub(1),
            Bound::Unbounded => rope.len().saturating_sub(1),
        };

        let mut needle_index: Option<usize> = None;

        for chunk in needles.chunks(3) {
            match chunk.len() {
                3 => {
                    let haystack = rope
                        .chunks_at(rope.len())
                        .0
                        .reversed()
                        .map(|c| c.as_bytes());
                    if let Some(pos) = memrchr3(chunk[0], chunk[1], chunk[2], haystack) {
                        needle_index = Some(needle_index.map_or(pos, |current| current.min(pos)));
                    }
                }
                _ => {
                    for &needle in chunk {
                        let haystack = rope
                            .chunks_at(rope.len())
                            .0
                            .reversed()
                            .map(|c| c.as_bytes());
                        if let Some(pos) = memrchr(needle, haystack) {
                            needle_index =
                                Some(needle_index.map_or(pos, |current| current.min(pos)));
                        }
                    }
                }
            }
        }

        needle_index.map(|byte_index| end - byte_index)
    }

    fn get_grapheme(&self, byte_index: usize) -> Option<RopeSlice<'_>> {
        let rope = self.as_slice();
        rope.next_grapheme_boundary(byte_index)
            .map(|end| rope.slice(byte_index..end))
    }

    fn grapheme(&self, byte_index: usize) -> RopeSlice<'_> {
        self.get_grapheme(byte_index).unwrap()
    }

    fn graphemes(&self) -> Graphemes<'_> {
        Graphemes::new(&self.as_slice())
    }

    fn is_grapheme_boundary(&self, byte_offset: usize) -> bool {
        unicode::is_grapheme_boundary(&self.as_slice(), byte_offset)
    }

    fn prev_grapheme_boundary(&self, byte_offset: usize) -> Option<usize> {
        unicode::prev_grapheme_boundary(&self.as_slice(), byte_offset)
    }

    fn next_grapheme_boundary(&self, byte_offset: usize) -> Option<usize> {
        unicode::next_grapheme_boundary(&self.as_slice(), byte_offset)
    }

    fn floor_grapheme_boundary(&self, byte_offset: usize) -> usize {
        unicode::floor_grapheme_boundary(&self.as_slice(), byte_offset)
    }

    fn ceil_grapheme_boundary(&self, byte_offset: usize) -> usize {
        unicode::ceil_grapheme_boundary(&self.as_slice(), byte_offset)
    }

    fn display_column(&self, byte_index: usize) -> usize {
        let rope = self.as_slice();
        let line_index = rope.byte_to_line_idx(byte_index, LINE_TYPE);
        let line_byte_index = rope.line_to_byte_idx(line_index, LINE_TYPE);
        rope.slice(line_byte_index..byte_index).display_width()
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

#[derive(Clone, Copy)]
enum Pos {
    ChunkStart,
    ChunkEnd,
}

pub struct RegexCursorInput<'a>(RopeSlice<'a>);

impl<'a> From<RopeSlice<'a>> for RegexCursorInput<'a> {
    fn from(slice: RopeSlice<'a>) -> Self {
        Self(slice)
    }
}

impl<'a> From<&'a Rope> for RegexCursorInput<'a> {
    fn from(rope: &'a Rope) -> Self {
        Self(rope.slice(..))
    }
}

impl<'a> IntoCursor for RegexCursorInput<'a> {
    type Cursor = RopeyCursor<'a>;

    fn into_cursor(self) -> Self::Cursor {
        RopeyCursor::new(self.0)
    }
}

#[derive(Clone)]
pub struct RopeyCursor<'a> {
    iter: ropey::iter::Chunks<'a>,
    current: &'a [u8],
    pos: Pos,
    len: usize,
    offset: usize,
}

impl<'a> RopeyCursor<'a> {
    #[must_use]
    pub fn new(slice: RopeSlice<'a>) -> Self {
        let iter = slice.chunks();
        let mut res = Self {
            current: &[],
            iter,
            pos: Pos::ChunkEnd,
            len: slice.len(),
            offset: 0,
        };
        res.advance();
        res
    }

    #[must_use]
    pub fn at(slice: RopeSlice<'a>, at: usize) -> Self {
        let (iter, offset) = slice.chunks_at(at);
        if offset == slice.len() {
            let mut res = Self {
                current: &[],
                iter,
                pos: Pos::ChunkStart,
                len: slice.len(),
                offset,
            };
            res.backtrack();
            res
        } else {
            let mut res = Self {
                current: &[],
                iter,
                pos: Pos::ChunkEnd,
                len: slice.len(),
                offset,
            };
            res.advance();
            res
        }
    }
}

impl Cursor for RopeyCursor<'_> {
    fn chunk(&self) -> &[u8] {
        self.current
    }

    fn advance(&mut self) -> bool {
        match self.pos {
            Pos::ChunkStart => {
                self.iter.next();
                self.pos = Pos::ChunkEnd;
            }
            Pos::ChunkEnd => (),
        }
        for next in self.iter.by_ref() {
            if next.is_empty() {
                continue;
            }
            self.offset += self.current.len();
            self.current = next.as_bytes();
            return true;
        }
        false
    }

    fn backtrack(&mut self) -> bool {
        match self.pos {
            Pos::ChunkStart => {}
            Pos::ChunkEnd => {
                self.iter.prev();
                self.pos = Pos::ChunkStart;
            }
        }
        while let Some(prev) = self.iter.prev() {
            if prev.is_empty() {
                continue;
            }
            self.offset -= prev.len();
            self.current = prev.as_bytes();
            return true;
        }
        false
    }

    fn utf8_aware(&self) -> bool {
        true
    }

    fn total_bytes(&self) -> Option<usize> {
        Some(self.len)
    }

    fn offset(&self) -> usize {
        self.offset
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
        assert_eq!(rope.chars().count(), 0);
        assert_eq!(rope.len_lines(LINE_TYPE), 1);
        assert_eq!(rope.len_lines_indigo(), 0);

        let rope = Rope::from_str("x");
        assert_eq!(rope.chars().count(), 1);
        assert_eq!(rope.len_lines(LINE_TYPE), 1);
        assert_eq!(rope.len_lines_indigo(), 1);

        let rope = Rope::from_str("\n");
        assert_eq!(rope.chars().count(), 1);
        assert_eq!(rope.len_lines(LINE_TYPE), 2);
        assert_eq!(rope.len_lines_indigo(), 1);

        let rope = Rope::from_str("x\n");
        assert_eq!(rope.chars().count(), 2);
        assert_eq!(rope.len_lines(LINE_TYPE), 2);
        assert_eq!(rope.len_lines_indigo(), 1);

        let rope = Rope::from_str("x\ny\nz");
        assert_eq!(rope.chars().count(), 5);
        assert_eq!(rope.len_lines(LINE_TYPE), 3);
        assert_eq!(rope.len_lines_indigo(), 3);

        let rope = Rope::from_str("x\ny\nz\n");
        assert_eq!(rope.chars().count(), 6);
        assert_eq!(rope.len_lines(LINE_TYPE), 4);
        assert_eq!(rope.len_lines_indigo(), 3);
    }

    #[test]
    fn rope_char() {
        assert_eq!(Rope::default().get_char(0).ok(), None);
        assert_eq!(Rope::default().get_char(1).ok(), None);
        assert_eq!(Rope::from_str("xyz").get_char(0).ok(), Some('x'));
        assert_eq!(Rope::from_str("xyz").get_char(1).ok(), Some('y'));
        assert_eq!(Rope::from_str("xyz").get_char(2).ok(), Some('z'));
        assert_eq!(Rope::from_str("xyz").get_char(3).ok(), None);
    }

    #[test]
    fn rope_line() {
        assert_eq!(Rope::default().get_line(0, LINE_TYPE), Some("".into()));
        assert_eq!(Rope::from_str("x\ny\nz").get_line(9, LINE_TYPE), None);
        assert_eq!(Rope::from_str("x").get_line(0, LINE_TYPE), Some("x".into()));
        assert_eq!(
            Rope::from_str("x\n").get_line(0, LINE_TYPE),
            Some("x\n".into())
        );
        assert_eq!(
            Rope::from_str("\nx").get_line(0, LINE_TYPE),
            Some("\n".into())
        );
        assert_eq!(
            Rope::from_str("\nx").get_line(1, LINE_TYPE),
            Some("x".into())
        );
        assert_eq!(
            Rope::from_str("x\ny\r\nz").get_line(0, LINE_TYPE),
            Some("x\n".into())
        );
        assert_eq!(
            Rope::from_str("x\ny\r\nz").get_line(1, LINE_TYPE),
            Some("y\r\n".into())
        );
        assert_eq!(
            Rope::from_str("x\ny\r\nz").get_line(2, LINE_TYPE),
            Some("z".into())
        );
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
            rope.insert(rope.len(), "hello");
        }
        let mut length = rope.len();
        let byte_index = length / 2;
        rope.insert(byte_index, "!?");
        length += "!?".len();
        assert_eq!(rope.find_next_byte(.., b"!?"), Some(byte_index));
        assert_eq!(
            rope.find_next_byte(0..(length / 3) * 2, b"!?"),
            Some(byte_index)
        );
        assert_eq!(rope.find_next_byte(0..length / 3, b"!?"), None);
        assert_eq!(rope.find_next_byte(0..length, b"!o"), Some(4));
    }

    // TODO: Test finding multiple needles backward.
    #[test]
    fn test_find_prev_byte() {
        let mut rope = Rope::new();
        while rope.chunks().count() < 10 {
            rope.insert(rope.len(), "hello");
        }
        let mut length = rope.len();
        let byte_index = length / 2;
        rope.insert(byte_index, "!");
        length += "!".len();
        assert_eq!(rope.find_prev_byte(.., b"!"), Some(byte_index));
        assert_eq!(
            rope.find_prev_byte(0..(length / 3) * 2, b"!"),
            Some(byte_index)
        );
        assert_eq!(rope.find_prev_byte(0..length / 3, b"!"), None);
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
