use crate::{position::Position, range::Range};
use ropey::Rope;
use std::cmp::{max, min};

enum Direction {
    Backward,
    Forward,
}

pub fn move_up(range: &Range, rope: &Rope, distance: usize) -> Range {
    move_vertically(range, rope, Direction::Backward, distance)
}

pub fn move_down(range: &Range, rope: &Rope, distance: usize) -> Range {
    move_vertically(range, rope, Direction::Forward, distance)
}

pub fn move_left(range: &Range, rope: &Rope, distance: usize) -> Range {
    move_horizontally(range, rope, Direction::Backward, distance)
}

pub fn move_right(range: &Range, rope: &Rope, distance: usize) -> Range {
    move_horizontally(range, rope, Direction::Forward, distance)
}

fn move_vertically(range: &Range, rope: &Rope, direction: Direction, distance: usize) -> Range {
    let desired_head = match direction {
        Direction::Backward => Position {
            // Prevent `corrected` from moving us to the first index in the rope if
            // we try to go above the first line
            line: max(0, range.head.line.saturating_sub(distance)),
            column: range.target_column.unwrap_or(range.head.column),
        },
        Direction::Forward => {
            // Subtracting 1 to convert to zero-based index, subtracting another 1 to remove ropey's
            // mysterious empty final line
            let last_line = rope.len_lines().saturating_sub(2);
            Position {
                // Prevent `corrected` from moving us to the last index in the rope
                // if we try to go below the last line
                line: min(range.head.line + distance, last_line),
                column: range.target_column.unwrap_or(range.head.column),
            }
        }
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

fn move_horizontally(range: &Range, rope: &Rope, direction: Direction, distance: usize) -> Range {
    let index = match direction {
        Direction::Backward => range
            .head
            .to_rope_index_lossy(rope)
            .0
            .saturating_sub(distance),
        Direction::Forward => range.head.to_rope_index_lossy(rope).0 + distance,
    };

    let (new_head, _) = Position::from_rope_index_lossy(rope, index);

    Range {
        anchor: range.anchor.clone(),
        head: new_head,
        target_column: None,
    }
}
