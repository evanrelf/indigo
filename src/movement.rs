use crate::{position::Position, range::Range};
use ropey::Rope;
use std::cmp::{max, min};

pub fn move_up(range: &Range, rope: &Rope, distance: usize) -> Range {
    vertically(range, rope, Behavior::Move, Direction::Backward, distance)
}

pub fn move_down(range: &Range, rope: &Rope, distance: usize) -> Range {
    vertically(range, rope, Behavior::Move, Direction::Forward, distance)
}

pub fn move_left(range: &Range, rope: &Rope, distance: usize) -> Range {
    horizontally(range, rope, Behavior::Move, Direction::Backward, distance)
}

pub fn move_right(range: &Range, rope: &Rope, distance: usize) -> Range {
    horizontally(range, rope, Behavior::Move, Direction::Forward, distance)
}

pub fn extend_up(range: &Range, rope: &Rope, distance: usize) -> Range {
    vertically(range, rope, Behavior::Extend, Direction::Backward, distance)
}

pub fn extend_down(range: &Range, rope: &Rope, distance: usize) -> Range {
    vertically(range, rope, Behavior::Extend, Direction::Forward, distance)
}

pub fn extend_left(range: &Range, rope: &Rope, distance: usize) -> Range {
    horizontally(range, rope, Behavior::Extend, Direction::Backward, distance)
}

pub fn extend_right(range: &Range, rope: &Rope, distance: usize) -> Range {
    horizontally(range, rope, Behavior::Extend, Direction::Forward, distance)
}

enum Direction {
    Backward,
    Forward,
}

enum Behavior {
    Move,
    Extend,
}

fn vertically(
    range: &Range,
    rope: &Rope,
    behavior: Behavior,
    direction: Direction,
    distance: usize,
) -> Range {
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

    let (head, _) = desired_head.corrected(rope);

    let anchor = match behavior {
        Behavior::Move => head.clone(),
        Behavior::Extend => range.anchor.clone(),
    };

    let target_column = if head.column == desired_head.column {
        None
    } else {
        Some(desired_head.column)
    };

    Range {
        anchor,
        head,
        target_column,
    }
}

fn horizontally(
    range: &Range,
    rope: &Rope,
    behavior: Behavior,
    direction: Direction,
    distance: usize,
) -> Range {
    let index = match direction {
        Direction::Backward => range
            .head
            .to_rope_index_lossy(rope)
            .0
            .saturating_sub(distance),
        Direction::Forward => range.head.to_rope_index_lossy(rope).0 + distance,
    };

    let (head, _) = Position::from_rope_index_lossy(rope, index);

    let anchor = match behavior {
        Behavior::Move => head.clone(),
        Behavior::Extend => range.anchor.clone(),
    };

    Range {
        anchor,
        head,
        target_column: None,
    }
}
