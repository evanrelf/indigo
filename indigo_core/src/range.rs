use crate::position::{self, Position};
use regex::Regex;
use ropey::{Rope, RopeSlice};
use std::{
    borrow::Cow,
    cmp::{max, min},
};

#[derive(Copy, Clone, Debug, Default, PartialEq)]
pub struct Range {
    anchor: Position,
    head: Position,
    target_column: Option<usize>,
}

enum Direction {
    Backward,
    Forward,
}

impl Range {
    #[must_use]
    pub fn anchor(&self) -> Position {
        self.anchor
    }

    #[must_use]
    pub fn head(&self) -> Position {
        self.head
    }

    #[must_use]
    pub fn target_column(&self) -> Option<usize> {
        self.target_column
    }

    #[must_use]
    pub fn is_forwards(&self) -> bool {
        self.anchor <= self.head
    }

    #[must_use]
    pub fn is_backwards(&self) -> bool {
        self.anchor > self.head
    }

    #[must_use]
    pub fn is_reduced(&self) -> bool {
        self.anchor == self.head
    }

    #[must_use]
    pub fn is_overlapping(&self, other: &Self) -> bool {
        let (self_start, self_end) = if self.is_forwards() {
            (&self.anchor, &self.head)
        } else {
            (&self.head, &self.anchor)
        };

        let (other_start, other_end) = if other.is_forwards() {
            (&other.anchor, &other.head)
        } else {
            (&other.head, &other.anchor)
        };

        (self_start <= other_start && other_start <= self_end)
            || (other_start <= self_start && self_start <= other_end)
    }

    #[must_use]
    pub fn flip(&self) -> Self {
        Self {
            anchor: self.head,
            head: self.anchor,
            target_column: None,
        }
    }

    #[must_use]
    pub fn flip_forwards(&self) -> Self {
        if self.is_backwards() {
            self.flip()
        } else {
            *self
        }
    }

    #[must_use]
    pub fn flip_backwards(&self) -> Self {
        if self.is_forwards() {
            self.flip()
        } else {
            *self
        }
    }

    #[must_use]
    pub fn reduce(&self) -> Self {
        Self {
            anchor: self.head,
            ..*self
        }
    }

    #[must_use]
    pub fn merge(&self, other: &Self) -> Self {
        match (self.is_forwards(), other.is_forwards()) {
            (true, true) => {
                // Forwards
                Self {
                    anchor: min(self.anchor, other.anchor),
                    head: max(self.head, other.head),
                    target_column: if other.head > self.head {
                        other.target_column
                    } else {
                        self.target_column
                    },
                }
            }
            (false, false) => {
                // Backwards
                Self {
                    anchor: max(self.anchor, other.anchor),
                    head: min(self.head, other.head),
                    target_column: if other.head < self.head {
                        other.target_column
                    } else {
                        self.target_column
                    },
                }
            }
            _ => {
                // Mixed
                self.merge(&other.flip())
            }
        }
    }

    #[must_use]
    pub fn select(&self, rope: &Rope, regex: &Regex) -> Vec<Self> {
        self.select_c(rope, regex).0
    }

    #[doc(hidden)]
    #[must_use]
    pub fn select_c(&self, rope: &Rope, regex: &Regex) -> (Vec<Self>, bool) {
        let mut corrected = false;

        let offset = if self.is_forwards() {
            let (anchor_index, anchor_corrected) = self.anchor.to_rope_index_c(rope);
            corrected |= anchor_corrected;
            anchor_index
        } else {
            let (head_index, head_corrected) = self.head.to_rope_index_c(rope);
            corrected |= head_corrected;
            head_index
        };

        let rope_slice = {
            let (slice, slice_corrected) = self.to_rope_slice_c(rope);
            corrected |= slice_corrected;
            slice
        };

        let cow = Cow::<str>::from(rope_slice);

        let str = if let Some(str) = rope_slice.as_str() {
            str
        } else {
            cow.as_ref()
        };

        let ranges = regex
            .find_iter(str)
            .map(|match_| {
                let start_index = offset + rope.byte_to_char(match_.start());
                let (start_position, start_corrected) =
                    Position::from_rope_index_c(rope, start_index);
                debug_assert!(!start_corrected);

                let end_index = offset + rope.byte_to_char(match_.end()).saturating_sub(1);
                let (end_position, end_corrected) = Position::from_rope_index_c(rope, end_index);
                debug_assert!(!end_corrected);

                if self.is_forwards() {
                    Self::from((start_position, end_position))
                } else {
                    Self::from((end_position, start_position))
                }
            })
            .collect();

        (ranges, corrected)
    }

    #[must_use]
    pub fn corrected(&self, rope: &Rope) -> Self {
        let anchor = self.anchor.corrected(rope);
        let head = self.head.corrected(rope);
        Self {
            anchor,
            head,
            target_column: None,
        }
    }

    #[must_use]
    pub fn to_rope_slice<'rope>(&self, rope: &'rope Rope) -> RopeSlice<'rope> {
        self.to_rope_slice_c(rope).0
    }

    #[must_use]
    pub fn to_rope_slice_c<'rope>(&self, rope: &'rope Rope) -> (RopeSlice<'rope>, bool) {
        let (anchor_index, anchor_corrected) = self.anchor.to_rope_index_c(rope);
        let (head_index, head_corrected) = self.head.to_rope_index_c(rope);

        let slice = if self.is_forwards() {
            rope.get_slice(anchor_index..=head_index).unwrap()
        } else {
            rope.get_slice(head_index..=anchor_index).unwrap()
        };

        (slice, anchor_corrected || head_corrected)
    }

    pub fn assert_invariants(&self) {
        // Target column must be greater than head's column
        if let Some(column) = self.target_column {
            assert!(column > self.head.column);
        }
    }
}

#[must_use]
pub fn range_is_valid(range: &Range, rope: Option<&Rope>) -> bool {
    position::position_is_valid(&range.anchor, rope)
        && position::position_is_valid(&range.head, rope)
}

impl From<Position> for Range {
    fn from(position: Position) -> Self {
        Self {
            anchor: position,
            head: position,
            target_column: None,
        }
    }
}

impl From<(Position, Position)> for Range {
    fn from((anchor, head): (Position, Position)) -> Self {
        Self {
            anchor,
            head,
            target_column: None,
        }
    }
}

impl TryFrom<(Position, Position, Option<usize>)> for Range {
    type Error = ();

    fn try_from(
        (anchor, head, target_column): (Position, Position, Option<usize>),
    ) -> Result<Self, Self::Error> {
        match target_column {
            Some(column) if column <= head.column => Err(()),
            _ => Ok(Self {
                anchor,
                head,
                target_column,
            }),
        }
    }
}

impl TryFrom<(Position, Position, usize)> for Range {
    type Error = ();

    fn try_from(
        (anchor, head, target_column): (Position, Position, usize),
    ) -> Result<Self, Self::Error> {
        Self::try_from((anchor, head, Some(target_column)))
    }
}

#[must_use]
fn vertically(range: &Range, rope: &Rope, direction: Direction, distance: usize) -> Range {
    let desired_head = Position {
        line: match direction {
            Direction::Backward => range.head().line.saturating_sub(distance),
            Direction::Forward => {
                // Subtracting 1 to remove ropey's mysterious empty final line
                let last_line = rope.len_lines().saturating_sub(1);
                // Prevent `corrected` from moving us to the last index in the rope if we try to go
                // below the last line
                min(range.head().line + distance, last_line)
            }
        },
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
fn horizontally(range: &Range, rope: &Rope, direction: Direction, distance: usize) -> Range {
    let index = range.head().to_rope_index(rope);

    let desired_index = match direction {
        Direction::Backward => index.saturating_sub(distance),
        Direction::Forward => index + distance,
    };

    let head = Position::from_rope_index(rope, desired_index);

    Range::from((range.anchor(), head))
}

#[must_use]
pub fn move_up(range: &Range, rope: &Rope, distance: usize) -> Range {
    extend_up(range, rope, distance).reduce()
}

#[must_use]
pub fn move_down(range: &Range, rope: &Rope, distance: usize) -> Range {
    extend_down(range, rope, distance).reduce()
}

#[must_use]
pub fn move_left(range: &Range, rope: &Rope, distance: usize) -> Range {
    extend_left(range, rope, distance).reduce()
}

#[must_use]
pub fn move_right(range: &Range, rope: &Rope, distance: usize) -> Range {
    extend_right(range, rope, distance).reduce()
}

#[must_use]
pub fn move_top(range: &Range) -> Range {
    extend_top(range).reduce()
}

#[must_use]
pub fn move_bottom(range: &Range, rope: &Rope) -> Range {
    extend_bottom(range, rope).reduce()
}

#[must_use]
pub fn move_end(range: &Range, rope: &Rope) -> Range {
    extend_end(range, rope).reduce()
}

#[must_use]
pub fn move_line_begin(range: &Range, rope: &Rope) -> Range {
    extend_line_begin(range, rope).reduce()
}

#[must_use]
pub fn move_line_first_non_blank(range: &Range, rope: &Rope) -> Range {
    extend_line_first_non_blank(range, rope).reduce()
}

#[must_use]
pub fn move_line_end(range: &Range, rope: &Rope) -> Range {
    extend_line_end(range, rope).reduce()
}

#[must_use]
pub fn extend_up(range: &Range, rope: &Rope, distance: usize) -> Range {
    vertically(range, rope, Direction::Backward, distance)
}

#[must_use]
pub fn extend_down(range: &Range, rope: &Rope, distance: usize) -> Range {
    vertically(range, rope, Direction::Forward, distance)
}

#[must_use]
pub fn extend_left(range: &Range, rope: &Rope, distance: usize) -> Range {
    horizontally(range, rope, Direction::Backward, distance)
}

#[must_use]
pub fn extend_right(range: &Range, rope: &Rope, distance: usize) -> Range {
    horizontally(range, rope, Direction::Forward, distance)
}

#[must_use]
pub fn extend_top(range: &Range) -> Range {
    Range::from((range.anchor(), position::top()))
}

#[must_use]
pub fn extend_bottom(range: &Range, rope: &Rope) -> Range {
    Range::from((range.anchor(), position::bottom(rope)))
}

#[must_use]
pub fn extend_end(range: &Range, rope: &Rope) -> Range {
    Range::from((range.anchor(), position::end(rope)))
}

#[must_use]
pub fn extend_line_begin(range: &Range, rope: &Rope) -> Range {
    let mut head = range.head();
    head.column = 0;

    Range::from((range.anchor(), head)).corrected(rope)
}

#[must_use]
pub fn extend_line_first_non_blank(range: &Range, rope: &Rope) -> Range {
    let blanks = [' ', '\t'];

    let first_non_blank = rope
        .line(range.head().line)
        .chars()
        .enumerate()
        .find(|(_, c)| !blanks.contains(c));

    let mut head = range.head();
    head.column = match first_non_blank {
        // Behave like `extend_line_end` if there are no non-blank characters on this line
        None => rope.line(head.line).len_chars().saturating_sub(1),
        Some((i, _)) => i,
    };

    Range::from((range.anchor(), head))
}

#[must_use]
pub fn extend_line_end(range: &Range, rope: &Rope) -> Range {
    let mut head = range.head().corrected(rope);
    head.column = rope.line(head.line).len_chars();

    Range::from((range.anchor(), head))
}
