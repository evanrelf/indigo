use crate::{position::Position, range::Range};
use regex::Regex;
use ropey::Rope;
use std::borrow::Cow;

pub fn select(rope: &Rope, range: &Range, regex: &Regex) -> Vec<Range> {
    let offset = if range.is_forwards() {
        range.anchor.to_rope_index_corrected(rope)
    } else {
        range.head.to_rope_index_corrected(rope)
    };

    let rope_slice = range.to_rope_slice_corrected(rope);

    let cow = Cow::<str>::from(rope_slice);

    let str = if let Some(str) = rope_slice.as_str() {
        str
    } else {
        cow.as_ref()
    };

    regex
        .find_iter(str)
        .map(|m| {
            let start =
                Position::from_rope_index(rope, offset + rope.byte_to_char(m.start())).unwrap();
            let end = Position::from_rope_index(
                rope,
                offset + rope.byte_to_char(m.end()).saturating_sub(1),
            )
            .unwrap();

            if range.is_forwards() {
                Range {
                    anchor: start,
                    head: end,
                    target_column: None,
                }
            } else {
                Range {
                    anchor: end,
                    head: start,
                    target_column: None,
                }
            }
        })
        .collect()
}
