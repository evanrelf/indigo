//! From <https://github.com/cessen/ropey/blob/master/examples/graphemes_iter.rs>.

use ropey::{iter::Chunks, RopeSlice};
use unicode_segmentation::{GraphemeCursor, GraphemeIncomplete};

pub struct RopeGraphemes<'a> {
    text: RopeSlice<'a>,
    chunks: Chunks<'a>,
    cur_chunk: &'a str,
    cur_chunk_start: usize,
    cursor: GraphemeCursor,
}

impl RopeGraphemes<'_> {
    pub fn new<'b>(slice: &RopeSlice<'b>) -> RopeGraphemes<'b> {
        let mut chunks = slice.chunks();
        let first_chunk = chunks.next().unwrap_or("");
        RopeGraphemes {
            text: *slice,
            chunks,
            cur_chunk: first_chunk,
            cur_chunk_start: 0,
            cursor: GraphemeCursor::new(0, slice.len_bytes(), true),
        }
    }
}

impl<'a> Iterator for RopeGraphemes<'a> {
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

pub struct RopeGraphemeBoundaries<'a> {
    rope: RopeSlice<'a>,
    chunk: &'a str,
    chunk_byte_index: usize,
    cursor: GraphemeCursor,
    zero: bool,
}

// TODO: Make a double-ended grapheme boundary iterator, rewrite next and prev step functions in
// terms of iterator to reduce duplicated code?
// TODO: Allow starting iterator from a provided character index?

impl<'a> RopeGraphemeBoundaries<'a> {
    pub fn new(rope: &RopeSlice<'a>) -> Self {
        let (chunk, chunk_byte_index, _, _) = rope.chunk_at_byte(0);
        Self {
            rope: *rope,
            chunk,
            chunk_byte_index,
            cursor: GraphemeCursor::new(0, rope.len_bytes(), true),
            zero: true,
        }
    }
}

impl Iterator for RopeGraphemeBoundaries<'_> {
    type Item = usize;

    fn next(&mut self) -> Option<Self::Item> {
        if self.zero {
            self.zero = false;
            return Some(0);
        }

        loop {
            match self.cursor.next_boundary(self.chunk, self.chunk_byte_index) {
                Ok(None) => return None,
                Ok(Some(byte_index)) => return Some(self.rope.byte_to_char(byte_index)),
                Err(GraphemeIncomplete::NextChunk) => {
                    let (next_chunk, next_chunk_byte_index, _, _) = self
                        .rope
                        .chunk_at_byte(self.chunk_byte_index + self.chunk.len());
                    self.chunk = next_chunk;
                    self.chunk_byte_index = next_chunk_byte_index;
                }
                Err(GraphemeIncomplete::PreContext(byte_index)) => {
                    let (prev_chunk, prev_chunk_byte_index, _, _) =
                        self.rope.chunk_at_byte(byte_index - 1);
                    self.cursor
                        .provide_context(prev_chunk, prev_chunk_byte_index);
                }
                _ => unreachable!(),
            }
        }
    }
}
