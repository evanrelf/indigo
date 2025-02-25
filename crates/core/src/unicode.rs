use ropey::{RopeSlice, iter::Chunks};
use unicode_segmentation::{GraphemeCursor, GraphemeIncomplete};

// Unicode Text Segmentation: https://unicode.org/reports/tr29/

// TODO: Move grapheme snapping functions from `rope` module.
// TODO: Change `prev`/`next` to `floor`/`ceil` to mirror `std`.
// TODO: Add char boundary functions.
// See https://github.com/rust-lang/rust/issues/93743 for the last two ^

#[derive(Clone, Copy)]
pub enum SnapBias {
    Before,
    After,
}

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
pub fn prev_grapheme_boundary(rope: &RopeSlice, char_index: usize) -> Option<usize> {
    if char_index > rope.len_chars() {
        return Some(rope.len_chars());
    }
    let byte_index = rope.char_to_byte(char_index);
    let (mut chunk, mut chunk_byte_index, _, _) = rope.chunk_at_byte(byte_index);
    let mut cursor = GraphemeCursor::new(byte_index, rope.len_bytes(), true);
    loop {
        match cursor.prev_boundary(chunk, chunk_byte_index) {
            Ok(None) => return None,
            Ok(Some(byte_index)) => return Some(rope.byte_to_char(byte_index)),
            Err(GraphemeIncomplete::PrevChunk) => {
                let (prev_chunk, prev_chunk_byte_index, _, _) =
                    rope.chunk_at_byte(chunk_byte_index - 1);
                chunk = prev_chunk;
                chunk_byte_index = prev_chunk_byte_index;
            }
            Err(GraphemeIncomplete::PreContext(byte_index)) => {
                let (chunk, chunk_byte_index, _, _) = rope.chunk_at_byte(byte_index - 1);
                cursor.provide_context(chunk, chunk_byte_index);
            }
            _ => unreachable!(),
        }
    }
}

#[must_use]
pub fn next_grapheme_boundary(rope: &RopeSlice, char_index: usize) -> Option<usize> {
    if char_index > rope.len_chars() {
        return None;
    }
    let byte_index = rope.char_to_byte(char_index);
    let (mut chunk, mut chunk_byte_index, _, _) = rope.chunk_at_byte(byte_index);
    let mut cursor = GraphemeCursor::new(byte_index, rope.len_bytes(), true);
    loop {
        match cursor.next_boundary(chunk, chunk_byte_index) {
            Ok(None) => return None,
            Ok(Some(byte_index)) => return Some(rope.byte_to_char(byte_index)),
            Err(GraphemeIncomplete::NextChunk) => {
                let (next_chunk, next_chunk_byte_index, _, _) =
                    rope.chunk_at_byte(chunk_byte_index + chunk.len());
                chunk = next_chunk;
                chunk_byte_index = next_chunk_byte_index;
            }
            Err(GraphemeIncomplete::PreContext(byte_index)) => {
                let (prev_chunk, prev_chunk_byte_index, _, _) = rope.chunk_at_byte(byte_index - 1);
                cursor.provide_context(prev_chunk, prev_chunk_byte_index);
            }
            _ => unreachable!(),
        }
    }
}

#[must_use]
pub fn snap_to_grapheme_boundary(rope: &RopeSlice, char_index: usize, bias: SnapBias) -> usize {
    let length = rope.len_chars();
    if char_index > length {
        return length;
    }
    if is_grapheme_boundary(rope, char_index) {
        return char_index;
    }
    match bias {
        SnapBias::Before => prev_grapheme_boundary(rope, char_index).unwrap(),
        SnapBias::After => next_grapheme_boundary(rope, char_index).unwrap(),
    }
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

pub struct GraphemeBoundaries<'a> {
    rope: RopeSlice<'a>,
    chunk: &'a str,
    chunk_byte_index: usize,
    cursor: GraphemeCursor,
    zero: bool,
}

// TODO: Make a double-ended grapheme boundary iterator, rewrite next and prev step functions in
// terms of iterator to reduce duplicated code?
// TODO: Allow starting iterator from a provided character index?

impl<'a> GraphemeBoundaries<'a> {
    #[must_use]
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

impl Iterator for GraphemeBoundaries<'_> {
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
