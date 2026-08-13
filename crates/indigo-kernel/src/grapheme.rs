use ropey::RopeSlice;
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
