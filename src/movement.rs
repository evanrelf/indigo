use crate::{position::Position, range::Range};
use ropey::Rope;
use std::cmp::{max, min};

pub fn move_up(range: &Range, rope: &Rope, distance: usize) -> Range {
    // Prevent `corrected` from moving us to the first index in the rope if
    // we try to go above the first line
    let desired_head = Position {
        line: max(0, range.head.line.saturating_sub(distance)),
        column: range.target_column.unwrap_or(range.head.column),
    };

    let (corrected_head, _) = desired_head.corrected(rope);

    let target_column = if corrected_head.column == desired_head.column {
        None
    } else {
        Some(desired_head.column)
    };

    Range {
        anchor: range.anchor.clone(),
        head: corrected_head,
        target_column,
    }
}

pub fn move_down(range: &Range, rope: &Rope, distance: usize) -> Range {
    let last_line = rope.len_lines().saturating_sub(2);

    let desired_head = Position {
        // Prevent `corrected` from moving us to the last index in the rope
        // if we try to go below the last line
        line: min(range.head.line + distance, last_line),
        column: range.target_column.unwrap_or(range.head.column),
    };

    let (corrected_head, _) = desired_head.corrected(rope);

    let target_column = if corrected_head.column == desired_head.column {
        None
    } else {
        Some(desired_head.column)
    };

    Range {
        anchor: range.anchor.clone(),
        head: corrected_head,
        target_column,
    }
}

pub fn move_left(range: &Range, rope: &Rope, distance: usize) -> Range {
    let (index, _) = range.head.to_rope_index_lossy(rope);

    let (new_head, _) = Position::from_rope_index_lossy(rope, index.saturating_sub(distance));

    Range {
        anchor: range.anchor.clone(),
        head: new_head,
        target_column: None,
    }
}

pub fn move_right(range: &Range, rope: &Rope, distance: usize) -> Range {
    let (index, _) = range.head.to_rope_index_lossy(rope);

    let (new_head, _) = Position::from_rope_index_lossy(rope, index + distance);

    Range {
        anchor: range.anchor.clone(),
        head: new_head,
        target_column: None,
    }
}
