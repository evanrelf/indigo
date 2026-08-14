use crate::grapheme::is_grapheme_boundary;
use ropey::{RopeSlice, iter::Chunks};
use std::iter::FusedIterator;
use unicode_segmentation::{GraphemeCursor, GraphemeIncomplete};

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
