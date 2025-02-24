//! From <https://github.com/cessen/ropey/blob/master/examples/graphemes_step.rs>.

use ropey::RopeSlice;
use unicode_segmentation::{GraphemeCursor, GraphemeIncomplete};

pub fn char_prev_grapheme_boundary(rope: &RopeSlice, char_index: usize) -> Option<usize> {
    let byte_index = rope.try_char_to_byte(char_index).ok()?;
    let prev_byte_index = byte_prev_grapheme_boundary(rope, byte_index)?;
    let prev_char_index = rope
        .try_byte_to_char(prev_byte_index)
        .expect("Previous grapheme boundary is valid char index");
    Some(prev_char_index)
}

pub fn byte_prev_grapheme_boundary(rope: &RopeSlice, byte_index: usize) -> Option<usize> {
    if byte_index > rope.len_bytes() {
        return Some(rope.len_bytes());
    }
    let (mut chunk, mut chunk_byte_index, _, _) = rope.chunk_at_byte(byte_index);
    let mut cursor = GraphemeCursor::new(byte_index, rope.len_bytes(), true);
    loop {
        match cursor.prev_boundary(chunk, chunk_byte_index) {
            Ok(result) => return result,
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

pub fn char_next_grapheme_boundary(rope: &RopeSlice, char_index: usize) -> Option<usize> {
    let byte_index = rope.try_char_to_byte(char_index).ok()?;
    let next_byte_index = byte_next_grapheme_boundary(rope, byte_index)?;
    let next_char_index = rope
        .try_byte_to_char(next_byte_index)
        .expect("Next grapheme boundary is valid char index");
    Some(next_char_index)
}

pub fn byte_next_grapheme_boundary(rope: &RopeSlice, byte_index: usize) -> Option<usize> {
    if byte_index > rope.len_bytes() {
        return None;
    }
    let (mut chunk, mut chunk_byte_index, _, _) = rope.chunk_at_byte(byte_index);
    let mut cursor = GraphemeCursor::new(byte_index, rope.len_bytes(), true);
    loop {
        match cursor.next_boundary(chunk, chunk_byte_index) {
            Ok(result) => return result,
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

pub fn char_is_grapheme_boundary(rope: &RopeSlice, char_index: usize) -> bool {
    let Ok(byte_index) = rope.try_char_to_byte(char_index) else {
        return false;
    };
    byte_is_grapheme_boundary(rope, byte_index)
}

pub fn byte_is_grapheme_boundary(rope: &RopeSlice, byte_index: usize) -> bool {
    if byte_index == 0 {
        return true;
    }
    if byte_index > rope.len_bytes() {
        return false;
    }
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
