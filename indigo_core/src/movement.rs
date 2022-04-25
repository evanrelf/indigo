use crate::{position::Position, range::Range};
use ropey::Rope;
use std::{
    cmp::{max, min},
    num::NonZeroUsize,
};

#[must_use]
pub fn extend_up(rope: &Rope, range: &Range, distance: usize) -> Range {
    vertically(rope, range, Direction::Backward, distance)
}

#[must_use]
pub fn extend_down(rope: &Rope, range: &Range, distance: usize) -> Range {
    vertically(rope, range, Direction::Forward, distance)
}

#[must_use]
pub fn extend_left(rope: &Rope, range: &Range, distance: usize) -> Range {
    horizontally(rope, range, Direction::Backward, distance)
}

#[must_use]
pub fn extend_right(rope: &Rope, range: &Range, distance: usize) -> Range {
    horizontally(rope, range, Direction::Forward, distance)
}

#[must_use]
pub fn move_up(rope: &Rope, range: &Range, distance: usize) -> Range {
    extend_up(rope, range, distance).reduce()
}

#[must_use]
pub fn move_down(rope: &Rope, range: &Range, distance: usize) -> Range {
    extend_down(rope, range, distance).reduce()
}

#[must_use]
pub fn move_left(rope: &Rope, range: &Range, distance: usize) -> Range {
    extend_left(rope, range, distance).reduce()
}

#[must_use]
pub fn move_right(rope: &Rope, range: &Range, distance: usize) -> Range {
    extend_right(rope, range, distance).reduce()
}

enum Direction {
    Backward,
    Forward,
}

#[must_use]
fn vertically(rope: &Rope, range: &Range, direction: Direction, distance: usize) -> Range {
    let desired_head = Position {
        line: NonZeroUsize::new(max(1, {
            match direction {
                Direction::Backward => range.head().line.get().saturating_sub(distance),
                Direction::Forward => {
                    // Subtracting 1 to remove ropey's mysterious empty final line
                    let last_line = rope.len_lines().saturating_sub(1);
                    // Prevent `corrected` from moving us to the last index in the rope if we try to go
                    // below the last line
                    min(range.head().line.get() + distance, last_line)
                }
            }
        }))
        .unwrap(),
        column: range.target_column().unwrap_or(range.head().column),
    };

    let head = desired_head.corrected(rope);

    let target_column = if head.column == desired_head.column {
        None
    } else {
        Some(desired_head.column)
    };

    Range::try_from((range.anchor(), head, target_column)).unwrap()
}

#[must_use]
fn horizontally(rope: &Rope, range: &Range, direction: Direction, distance: usize) -> Range {
    let index = range.head().to_rope_index_corrected(rope);

    let desired_index = match direction {
        Direction::Backward => index.saturating_sub(distance),
        Direction::Forward => index + distance,
    };

    let head = Position::from_rope_index_corrected(rope, desired_index);

    Range::from((range.anchor(), head))
}

#[must_use]
pub fn extend_buffer_top(range: &Range) -> Range {
    Range::from((range.anchor(), (1, 1).try_into().unwrap()))
}

#[must_use]
pub fn extend_buffer_bottom(rope: &Rope, range: &Range) -> Range {
    let index = rope.line_to_char(rope.len_lines().saturating_sub(2));
    let head = Position::from_rope_index(rope, index).unwrap();
    Range::from((range.anchor(), head))
}

#[must_use]
pub fn extend_buffer_end(rope: &Rope, range: &Range) -> Range {
    let index = rope.len_chars().saturating_sub(1);
    let head = Position::from_rope_index(rope, index).unwrap();
    Range::from((range.anchor(), head))
}

#[must_use]
pub fn move_buffer_top(range: &Range) -> Range {
    extend_buffer_top(range).reduce()
}

#[must_use]
pub fn move_buffer_bottom(rope: &Rope, range: &Range) -> Range {
    extend_buffer_bottom(rope, range).reduce()
}

#[must_use]
pub fn move_buffer_end(rope: &Rope, range: &Range) -> Range {
    extend_buffer_end(rope, range).reduce()
}

#[must_use]
pub fn extend_line_begin(range: &Range) -> Range {
    let mut head = range.head();
    head.column = NonZeroUsize::new(1).unwrap();
    Range::from((range.anchor(), head))
}

#[must_use]
pub fn extend_line_first_non_blank(rope: &Rope, range: &Range) -> Range {
    let blanks = [' '];

    let first_non_blank = rope
        .line(range.head().line.get())
        .chars()
        .enumerate()
        .find(|(_, c)| !blanks.contains(c));

    let mut head = range.head();
    head.column = NonZeroUsize::new(match first_non_blank {
        // Behave like `extend_line_end` if there are no non-blank characters on this line
        None => rope.line(head.line.get()).len_chars().saturating_sub(1),
        Some((i, _)) => i,
    })
    .unwrap();

    Range::from((range.anchor(), head))
}

#[must_use]
pub fn extend_line_end(rope: &Rope, range: &Range) -> Range {
    let mut head = range.head().corrected(rope);
    head.column = NonZeroUsize::new(rope.line(head.line.get()).len_chars()).unwrap();
    Range::from((range.anchor(), head))
}

#[must_use]
pub fn move_line_begin(range: &Range) -> Range {
    extend_line_begin(range).reduce()
}

#[must_use]
pub fn move_line_first_non_blank(rope: &Rope, range: &Range) -> Range {
    extend_line_first_non_blank(rope, range).reduce()
}

#[must_use]
pub fn move_line_end(rope: &Rope, range: &Range) -> Range {
    extend_line_end(rope, range).reduce()
}
