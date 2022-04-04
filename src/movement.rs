use crate::{position::Position, range::Range};
use ropey::Rope;
use std::cmp::{max, min};

pub fn move_up(rope: &Rope, range: &Range, distance: usize) -> Range {
    let mut range = extend_up(rope, range, distance);
    range.reduce();
    range
}

pub fn move_down(rope: &Rope, range: &Range, distance: usize) -> Range {
    let mut range = extend_down(rope, range, distance);
    range.reduce();
    range
}

pub fn move_left(rope: &Rope, range: &Range, distance: usize) -> Range {
    let mut range = extend_left(rope, range, distance);
    range.reduce();
    range
}

pub fn move_right(rope: &Rope, range: &Range, distance: usize) -> Range {
    let mut range = extend_right(rope, range, distance);
    range.reduce();
    range
}

pub fn extend_up(rope: &Rope, range: &Range, distance: usize) -> Range {
    vertically(rope, range, Direction::Backward, distance)
}

pub fn extend_down(rope: &Rope, range: &Range, distance: usize) -> Range {
    vertically(rope, range, Direction::Forward, distance)
}

pub fn extend_left(rope: &Rope, range: &Range, distance: usize) -> Range {
    horizontally(rope, range, Direction::Backward, distance)
}

pub fn extend_right(rope: &Rope, range: &Range, distance: usize) -> Range {
    horizontally(rope, range, Direction::Forward, distance)
}

enum Direction {
    Backward,
    Forward,
}

fn vertically(
    rope: &Rope,
    range: &Range,
    direction: Direction,
    distance: usize,
) -> Range {
    let anchor = range.anchor.clone();

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
    rope: &Rope,
    range: &Range,
    direction: Direction,
    distance: usize,
) -> Range {
    let anchor = range.anchor.clone();

    let index = match direction {
        Direction::Backward => range
            .head
            .to_rope_index_lossy(rope)
            .0
            .saturating_sub(distance),
        Direction::Forward => range.head.to_rope_index_lossy(rope).0 + distance,
    };

    let (head, _) = Position::from_rope_index_lossy(rope, index);

    Range {
        anchor,
        head,
        target_column: None,
    }
}

pub fn move_top(range: &Range) -> Range {
    let mut range = top(range);
    range.reduce();
    range
}

pub fn move_bottom(rope: &Rope, range: &Range) -> Range {
    let mut range = bottom(rope, range);
    range.reduce();
    range
}

pub fn extend_top(range: &Range) -> Range {
    top(range)
}

pub fn extend_bottom(rope: &Rope, range: &Range) -> Range {
    bottom(rope, range)
}

fn top(range: &Range) -> Range {
    Range {
        anchor: range.anchor.clone(),
        head: (0, 0).into(),
        target_column: None,
    }
}

fn bottom(rope: &Rope, range: &Range) -> Range {
    Range {
        anchor: range.anchor.clone(),
        head: Position::from_rope_index(rope, rope.len_chars().saturating_sub(1)).unwrap(),
        target_column: None,
    }
}
