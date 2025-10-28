use ropey::{RopeSlice, iter::Chunks};
use unicode_segmentation::{GraphemeCursor, GraphemeIncomplete};

// https://unicode.org/reports/tr29/#Grapheme_Cluster_Boundaries

#[must_use]
pub fn is_grapheme_boundary(rope: &RopeSlice, char_index: usize) -> bool {
    if char_index > rope.len_chars() {
        return false;
    }
    let byte_index = rope.char_to_byte(char_index);
    let (chunk, chunk_byte_index, _, _) = rope.chunk_at_byte(byte_index);
    let mut cursor = GraphemeCursor::new(byte_index, rope.len_bytes(), true);
    loop {
        match cursor.is_boundary(chunk, chunk_byte_index) {
            Ok(is_boundary) => return is_boundary,
            Err(GraphemeIncomplete::PreContext(byte_index)) => {
                let (prev_chunk, prev_chunk_byte_index, _, _) = rope.chunk_at_byte(byte_index - 1);
                cursor.provide_context(prev_chunk, prev_chunk_byte_index);
            }
            _ => unreachable!(),
        }
    }
}

#[must_use]
pub fn prev_grapheme_boundary(rope: &RopeSlice, mut char_offset: usize) -> Option<usize> {
    if char_offset == 0 {
        return None;
    }
    let length = rope.len_chars();
    if char_offset > length {
        return Some(length);
    }
    while char_offset > 0 {
        char_offset -= 1;
        if is_grapheme_boundary(rope, char_offset) {
            return Some(char_offset);
        }
    }
    unreachable!()
}

#[must_use]
pub fn next_grapheme_boundary(rope: &RopeSlice, mut char_offset: usize) -> Option<usize> {
    let length = rope.len_chars();
    if char_offset >= length {
        return None;
    }
    while char_offset < length {
        char_offset += 1;
        if is_grapheme_boundary(rope, char_offset) {
            return Some(char_offset);
        }
    }
    unreachable!()
}

#[must_use]
pub fn floor_grapheme_boundary(rope: &RopeSlice, char_index: usize) -> usize {
    let length = rope.len_chars();
    if char_index > length {
        return length;
    }
    if is_grapheme_boundary(rope, char_index) {
        return char_index;
    }
    prev_grapheme_boundary(rope, char_index).unwrap()
}

#[must_use]
pub fn ceil_grapheme_boundary(rope: &RopeSlice, char_index: usize) -> usize {
    let length = rope.len_chars();
    if char_index > length {
        return length;
    }
    if is_grapheme_boundary(rope, char_index) {
        return char_index;
    }
    next_grapheme_boundary(rope, char_index).unwrap()
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
            cursor: GraphemeCursor::new(0, slice.len_bytes(), true),
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
                    let (chunk, byte_idx, _, _) = self.text.chunk_at_byte(idx.saturating_sub(1));
                    self.cursor.provide_context(chunk, byte_idx);
                }
                _ => unreachable!(),
            }
        }

        if a < self.cur_chunk_start {
            let a_char = self.text.byte_to_char(a);
            let b_char = self.text.byte_to_char(b);

            Some(self.text.slice(a_char..b_char))
        } else {
            let a2 = a - self.cur_chunk_start;
            let b2 = b - self.cur_chunk_start;
            Some((&self.cur_chunk[a2..b2]).into())
        }
    }
}
