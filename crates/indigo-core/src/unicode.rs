//! Unicode scalar values and extended grapheme clusters.
//!
//! <https://unicode.org/reports/tr29/#Grapheme_Cluster_Boundaries>

use ropey::{RopeSlice, iter::Chunks};
use unicode_segmentation::{GraphemeCursor, GraphemeIncomplete};

#[must_use]
fn is_char_boundary(rope: &RopeSlice, byte_index: usize) -> bool {
    if byte_index == 0 || byte_index >= rope.len() {
        return true;
    }
    let (chunk, chunk_byte_index) = rope.chunk(byte_index);
    chunk.is_char_boundary(byte_index - chunk_byte_index)
}

#[must_use]
fn floor_char_boundary(rope: &RopeSlice, mut byte_index: usize) -> usize {
    let length = rope.len();
    if byte_index >= length {
        return length;
    }
    while !is_char_boundary(rope, byte_index) {
        byte_index -= 1;
    }
    byte_index
}

#[must_use]
fn ceil_char_boundary(rope: &RopeSlice, mut byte_index: usize) -> usize {
    let length = rope.len();
    if byte_index >= length {
        return length;
    }
    while !is_char_boundary(rope, byte_index) {
        byte_index += 1;
        if byte_index >= length {
            return length;
        }
    }
    byte_index
}

#[must_use]
pub fn is_grapheme_boundary(rope: &RopeSlice, byte_index: usize) -> bool {
    if byte_index > rope.len() {
        return false;
    }
    let (chunk, chunk_byte_index) = rope.chunk(byte_index);
    let mut cursor = GraphemeCursor::new(byte_index, rope.len(), true);
    loop {
        match cursor.is_boundary(chunk, chunk_byte_index) {
            Ok(is_boundary) => return is_boundary,
            Err(GraphemeIncomplete::PreContext(byte_index)) => {
                let (prev_chunk, prev_chunk_byte_index) = rope.chunk(byte_index - 1);
                cursor.provide_context(prev_chunk, prev_chunk_byte_index);
            }
            _ => unreachable!(),
        }
    }
}

#[must_use]
pub fn prev_grapheme_boundary(rope: &RopeSlice, mut byte_offset: usize) -> Option<usize> {
    if byte_offset == 0 {
        return None;
    }
    let length = rope.len();
    if byte_offset > length {
        return Some(length);
    }
    while byte_offset > 0 {
        byte_offset -= 1;
        while byte_offset > 0 && !is_char_boundary(rope, byte_offset) {
            byte_offset -= 1;
        }
        if is_grapheme_boundary(rope, byte_offset) {
            return Some(byte_offset);
        }
    }
    unreachable!()
}

#[must_use]
pub fn next_grapheme_boundary(rope: &RopeSlice, mut byte_offset: usize) -> Option<usize> {
    let length = rope.len();
    if byte_offset >= length {
        return None;
    }
    while byte_offset < length {
        byte_offset += 1;
        while byte_offset < length && !is_char_boundary(rope, byte_offset) {
            byte_offset += 1;
        }
        if byte_offset >= length {
            return Some(length);
        }
        if is_grapheme_boundary(rope, byte_offset) {
            return Some(byte_offset);
        }
    }
    unreachable!()
}

#[must_use]
pub fn floor_grapheme_boundary(rope: &RopeSlice, byte_index: usize) -> usize {
    let length = rope.len();
    if byte_index > length {
        return length;
    }
    let byte_index = floor_char_boundary(rope, byte_index);
    if is_grapheme_boundary(rope, byte_index) {
        return byte_index;
    }
    prev_grapheme_boundary(rope, byte_index)
        .expect("Only fails at start of text, which is a grapheme boundary")
}

#[must_use]
pub fn ceil_grapheme_boundary(rope: &RopeSlice, byte_index: usize) -> usize {
    let length = rope.len();
    if byte_index > length {
        return length;
    }
    let byte_index = ceil_char_boundary(rope, byte_index);
    if is_grapheme_boundary(rope, byte_index) {
        return byte_index;
    }
    next_grapheme_boundary(rope, byte_index)
        .expect("Only fails at end of text, which is a grapheme boundary")
}

pub struct Graphemes<'a> {
    text: RopeSlice<'a>,
    chunks: Chunks<'a>,
    cur_chunk: &'a str,
    cur_chunk_start: usize,
    cursor: GraphemeCursor,
}

impl Graphemes<'_> {
    #[must_use]
    pub fn new<'b>(slice: &RopeSlice<'b>) -> Graphemes<'b> {
        let mut chunks = slice.chunks();
        let first_chunk = chunks.next().unwrap_or("");
        Graphemes {
            text: *slice,
            chunks,
            cur_chunk: first_chunk,
            cur_chunk_start: 0,
            cursor: GraphemeCursor::new(0, slice.len(), true),
        }
    }
}

impl<'a> Iterator for Graphemes<'a> {
    type Item = RopeSlice<'a>;

    fn next(&mut self) -> Option<RopeSlice<'a>> {
        let a = self.cursor.cur_cursor();
        let b;
        loop {
            match self
                .cursor
                .next_boundary(self.cur_chunk, self.cur_chunk_start)
            {
                Ok(None) => {
                    return None;
                }
                Ok(Some(n)) => {
                    b = n;
                    break;
                }
                Err(GraphemeIncomplete::NextChunk) => {
                    self.cur_chunk_start += self.cur_chunk.len();
                    self.cur_chunk = self.chunks.next().unwrap_or("");
                }
                Err(GraphemeIncomplete::PreContext(idx)) => {
                    let (chunk, byte_idx) = self.text.chunk(idx.saturating_sub(1));
                    self.cursor.provide_context(chunk, byte_idx);
                }
                _ => unreachable!(),
            }
        }

        if a < self.cur_chunk_start {
            Some(self.text.slice(a..b))
        } else {
            let a2 = a - self.cur_chunk_start;
            let b2 = b - self.cur_chunk_start;
            Some((&self.cur_chunk[a2..b2]).into())
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use ropey::Rope;

    #[test]
    fn test_floor_ceil_grapheme_boundary_with_emoji() {
        let text = "👩🏻‍❤️‍💋‍👩🏻";
        let rope = Rope::from_str(text);
        let slice = rope.slice(..);
        for byte_index in 0..=rope.len() {
            // These should not panic, even if byte_index is in the middle of a multi-byte char
            let floor = floor_grapheme_boundary(&slice, byte_index);
            let ceil = ceil_grapheme_boundary(&slice, byte_index);
            // Floor should be <= byte_index
            assert!(
                floor <= byte_index,
                "floor={floor}, byte_index={byte_index}"
            );
            // Ceil should be >= byte_index
            assert!(ceil >= byte_index, "ceil={ceil}, byte_index={byte_index}");
            // Both should be valid grapheme boundaries
            assert!(is_grapheme_boundary(&slice, floor));
            assert!(is_grapheme_boundary(&slice, ceil));
        }
    }

    #[test]
    fn test_next_prev_grapheme_boundary_with_emoji() {
        let text = "a👩🏻‍❤️‍💋‍👩🏻b";
        let rope = Rope::from_str(text);
        let slice = rope.slice(..);
        let mut pos = 0;
        let positions = std::iter::from_fn(|| {
            let current = pos;
            pos = next_grapheme_boundary(&slice, pos)?;
            Some(current)
        })
        .collect::<Vec<_>>();
        // Should have 3 graphemes: "a", the complex emoji, and "b"
        assert_eq!(positions.len(), 3);
        // Each position should be a valid grapheme boundary
        for &pos in &positions {
            assert!(is_grapheme_boundary(&slice, pos));
        }
    }
}
