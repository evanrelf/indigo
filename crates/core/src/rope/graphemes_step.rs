//! From <https://github.com/cessen/ropey/blob/master/examples/graphemes_step.rs>.

use ropey::RopeSlice;
use unicode_segmentation::{GraphemeCursor, GraphemeIncomplete};

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
