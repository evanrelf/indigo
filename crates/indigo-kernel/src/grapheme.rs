use ropey::{RopeSlice, iter::Chunks};
use std::iter::FusedIterator;
use unicode_segmentation::{GraphemeCursor, GraphemeIncomplete};

#[must_use]
pub fn is_char_boundary(rope: &RopeSlice, byte_offset: usize) -> bool {
    assert!(
        byte_offset <= rope.len(),
        "byte offset {byte_offset} is out of bounds (rope length is {})",
        rope.len()
    );
    if byte_offset == 0 || byte_offset == rope.len() {
        return true;
    }
    let (chunk, chunk_byte_offset) = rope.chunk(byte_offset);
    chunk.is_char_boundary(byte_offset - chunk_byte_offset)
}

#[must_use]
pub fn floor_char_boundary(rope: &RopeSlice, byte_offset: usize) -> usize {
    assert!(
        byte_offset <= rope.len(),
        "byte offset {byte_offset} is out of bounds (rope length is {})",
        rope.len()
    );
    if byte_offset == rope.len() {
        return byte_offset;
    }
    let (chunk, chunk_byte_offset) = rope.chunk(byte_offset);
    chunk_byte_offset + chunk.floor_char_boundary(byte_offset - chunk_byte_offset)
}

#[must_use]
pub fn ceil_char_boundary(rope: &RopeSlice, byte_offset: usize) -> usize {
    assert!(
        byte_offset <= rope.len(),
        "byte offset {byte_offset} is out of bounds (rope length is {})",
        rope.len()
    );
    if byte_offset == rope.len() {
        return byte_offset;
    }
    let (chunk, chunk_byte_offset) = rope.chunk(byte_offset);
    chunk_byte_offset + chunk.ceil_char_boundary(byte_offset - chunk_byte_offset)
}

#[must_use]
pub fn is_grapheme_boundary(rope: &RopeSlice, byte_offset: usize) -> bool {
    assert!(
        byte_offset <= rope.len(),
        "byte offset {byte_offset} is out of bounds (rope length is {})",
        rope.len()
    );
    if !is_char_boundary(rope, byte_offset) {
        return false;
    }
    let (chunk, chunk_byte_offset) = rope.chunk(byte_offset);
    let mut cursor = GraphemeCursor::new(byte_offset, rope.len(), true);
    loop {
        match cursor.is_boundary(chunk, chunk_byte_offset) {
            Ok(is_boundary) => return is_boundary,
            Err(GraphemeIncomplete::PreContext(byte_offset)) => {
                let (prev_chunk, prev_chunk_byte_offset) = rope.chunk(byte_offset - 1);
                cursor.provide_context(prev_chunk, prev_chunk_byte_offset);
            }
            _ => unreachable!(),
        }
    }
}

#[must_use]
pub fn is_grapheme_start(rope: &RopeSlice, byte_index: usize) -> bool {
    assert!(
        byte_index <= rope.len(),
        "byte index {byte_index} is out of bounds (rope length is {})",
        rope.len()
    );
    if byte_index == rope.len() {
        return false;
    }
    is_grapheme_boundary(rope, byte_index)
}

#[must_use]
pub fn prev_grapheme_boundary(rope: &RopeSlice, byte_offset: usize) -> Option<usize> {
    assert!(
        byte_offset <= rope.len(),
        "byte offset {byte_offset} is out of bounds (rope length is {})",
        rope.len()
    );
    if byte_offset == 0 {
        return None;
    }
    // `GraphemeCursor` requires a `char` boundary to start from
    let start = floor_char_boundary(rope, byte_offset);
    if start < byte_offset && is_grapheme_boundary(rope, start) {
        return Some(start);
    }
    let (mut chunk, mut chunk_byte_offset) = rope.chunk(start);
    let mut cursor = GraphemeCursor::new(start, rope.len(), true);
    loop {
        match cursor.prev_boundary(chunk, chunk_byte_offset) {
            Ok(boundary) => return boundary,
            Err(GraphemeIncomplete::PrevChunk) => {
                (chunk, chunk_byte_offset) = rope.chunk(chunk_byte_offset - 1);
            }
            Err(GraphemeIncomplete::PreContext(byte_offset)) => {
                let (prev_chunk, prev_chunk_byte_offset) = rope.chunk(byte_offset - 1);
                cursor.provide_context(prev_chunk, prev_chunk_byte_offset);
            }
            _ => unreachable!(),
        }
    }
}

#[must_use]
pub fn next_grapheme_boundary(rope: &RopeSlice, byte_offset: usize) -> Option<usize> {
    assert!(
        byte_offset <= rope.len(),
        "byte offset {byte_offset} is out of bounds (rope length is {})",
        rope.len()
    );
    if byte_offset == rope.len() {
        return None;
    }
    // `GraphemeCursor` requires a `char` boundary to start from
    let start = ceil_char_boundary(rope, byte_offset);
    if start > byte_offset && is_grapheme_boundary(rope, start) {
        return Some(start);
    }
    let (mut chunk, mut chunk_byte_offset) = rope.chunk(start);
    let mut cursor = GraphemeCursor::new(start, rope.len(), true);
    loop {
        match cursor.next_boundary(chunk, chunk_byte_offset) {
            Ok(boundary) => return boundary,
            Err(GraphemeIncomplete::NextChunk) => {
                (chunk, chunk_byte_offset) = rope.chunk(chunk_byte_offset + chunk.len());
            }
            Err(GraphemeIncomplete::PreContext(byte_offset)) => {
                let (prev_chunk, prev_chunk_byte_offset) = rope.chunk(byte_offset - 1);
                cursor.provide_context(prev_chunk, prev_chunk_byte_offset);
            }
            _ => unreachable!(),
        }
    }
}

#[must_use]
pub fn floor_grapheme_boundary(rope: &RopeSlice, byte_offset: usize) -> usize {
    assert!(
        byte_offset <= rope.len(),
        "byte offset {byte_offset} is out of bounds (rope length is {})",
        rope.len()
    );
    let byte_offset = floor_char_boundary(rope, byte_offset);
    if is_grapheme_boundary(rope, byte_offset) {
        return byte_offset;
    }
    prev_grapheme_boundary(rope, byte_offset)
        .expect("only fails at start of text, which is a grapheme boundary")
}

#[must_use]
pub fn ceil_grapheme_boundary(rope: &RopeSlice, byte_offset: usize) -> usize {
    assert!(
        byte_offset <= rope.len(),
        "byte offset {byte_offset} is out of bounds (rope length is {})",
        rope.len()
    );
    let byte_offset = ceil_char_boundary(rope, byte_offset);
    if is_grapheme_boundary(rope, byte_offset) {
        return byte_offset;
    }
    next_grapheme_boundary(rope, byte_offset)
        .expect("only fails at end of text, which is a grapheme boundary")
}

#[derive(Clone, Debug)]
pub struct Graphemes<'a> {
    rope: RopeSlice<'a>,
    chunks: Chunks<'a>,
    chunk: &'a str,
    chunk_byte_offset: usize,
    cursor: GraphemeCursor,
}

impl<'a> Graphemes<'a> {
    #[must_use]
    pub fn new(rope: &RopeSlice<'a>) -> Self {
        let mut chunks = rope.chunks();
        let chunk = chunks.next().unwrap_or("");
        Graphemes {
            rope: *rope,
            chunks,
            chunk,
            chunk_byte_offset: 0,
            cursor: GraphemeCursor::new(0, rope.len(), true),
        }
    }
}

impl<'a> Iterator for Graphemes<'a> {
    type Item = RopeSlice<'a>;

    fn next(&mut self) -> Option<RopeSlice<'a>> {
        let start = self.cursor.cur_cursor();
        let end;
        loop {
            match self
                .cursor
                .next_boundary(self.chunk, self.chunk_byte_offset)
            {
                Ok(None) => return None,
                Ok(Some(boundary)) => {
                    end = boundary;
                    break;
                }
                Err(GraphemeIncomplete::NextChunk) => {
                    self.chunk_byte_offset += self.chunk.len();
                    self.chunk = self
                        .chunks
                        .next()
                        .expect("only requested when the cursor is not at the end of the rope");

                    // Note [Fresh cursor at chunk crossings]
                    // BEGIN WORKAROUND
                    let crossing = self.cursor.cur_cursor();
                    self.cursor = GraphemeCursor::new(crossing, self.rope.len(), true);
                    if crossing > start && is_grapheme_boundary(&self.rope, crossing) {
                        end = crossing;
                        break;
                    }
                    // END WORKAROUND
                }
                Err(GraphemeIncomplete::PreContext(byte_offset)) => {
                    let (prev_chunk, prev_chunk_byte_offset) = self.rope.chunk(byte_offset - 1);
                    self.cursor
                        .provide_context(prev_chunk, prev_chunk_byte_offset);
                }
                _ => unreachable!(),
            }
        }
        if start < self.chunk_byte_offset {
            // Grapheme spans multiple chunks, so requires going through the rope
            Some(self.rope.slice(start..end))
        } else {
            // Grapheme fits within the current chunk, so requires no copying
            let start_in_chunk = start - self.chunk_byte_offset;
            let end_in_chunk = end - self.chunk_byte_offset;
            Some((&self.chunk[start_in_chunk..end_in_chunk]).into())
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        // Graphemes are at least one byte long
        let remaining_bytes = self.rope.len() - self.cursor.cur_cursor();
        (usize::from(remaining_bytes > 0), Some(remaining_bytes))
    }
}

impl FusedIterator for Graphemes<'_> {}

/*
Note [Fresh cursor at chunk crossings]
--------------------------------------

Claude: `unicode-segmentation` 1.13.3's `GraphemeCursor` produces false grapheme boundaries in
flag-emoji (regional indicator) runs when a long-lived cursor crosses a chunk boundary: at the
hand-off, `handle_regional`'s backward pre-context scan is seeded with the forward-accumulated
`ris_count`, counting the same indicators twice and flipping the pairing parity.

The workaround decides each chunk crossing with a fresh cursor (via `is_grapheme_boundary`) and
rebuilds the iterator's cursor there, so no stale state survives the hand-off.
*/

#[cfg(test)]
mod tests {
    use super::*;
    use ropey::Rope;
    use unicode_segmentation::UnicodeSegmentation as _;

    // Note [Fresh cursor at chunk crossings]
    #[test]
    fn flag_run_across_chunks() {
        let text = "\u{1f1e6}\u{1f1e7}".repeat(1024 / 8 + 1); // should be more than 1 chunk
        let rope = Rope::from_str(&text);
        for start in [0, 4] {
            let expected: Vec<&str> = text[start..].graphemes(true).collect();
            let actual: Vec<String> = Graphemes::new(&rope.slice(start..))
                .map(|grapheme| grapheme.to_string())
                .collect();
            assert_eq!(actual, expected, "slice starting at {start}");
        }
    }
}
