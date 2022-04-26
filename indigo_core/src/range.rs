use crate::{
    position::Position,
    valid::{Valid, ValidFor as _},
};
use regex::Regex;
use ropey::{Rope, RopeSlice};
use std::{
    borrow::Cow,
    cmp::{max, min},
    num::NonZeroUsize,
};

#[derive(Copy, Clone, Default, PartialEq)]
pub struct Range {
    anchor: Position,
    head: Position,
    target_column: Option<NonZeroUsize>,
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
    pub fn target_column(&self) -> Option<NonZeroUsize> {
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
    pub fn select(&self, rope: &Rope, regex: &Regex) -> (Vec<Self>, bool) {
        let mut corrected = false;

        let offset = if self.is_forwards() {
            let (anchor_index, anchor_corrected) = self.anchor.to_rope_index(rope);
            corrected |= anchor_corrected;
            *anchor_index
        } else {
            let (head_index, head_corrected) = self.head.to_rope_index(rope);
            corrected |= head_corrected;
            *head_index
        };

        let rope_slice = {
            let (slice, slice_corrected) = self.to_rope_slice(rope);
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
                    Position::from_rope_index(rope, start_index);
                debug_assert!(!start_corrected);

                let end_index = offset + rope.byte_to_char(match_.end()).saturating_sub(1);
                let (end_position, end_corrected) = Position::from_rope_index(rope, end_index);
                debug_assert!(!end_corrected);

                if self.is_forwards() {
                    Self::from((*start_position, *end_position))
                } else {
                    Self::from((*end_position, *start_position))
                }
            })
            .collect();

        (ranges, corrected)
    }

    #[must_use]
    pub fn is_valid(&self, rope: &Rope) -> bool {
        self.anchor.is_valid(rope) && self.head.is_valid(rope)
    }

    #[must_use]
    pub fn corrected<'rope>(&self, rope: &'rope Rope) -> Valid<'rope, Self> {
        let anchor = *self.anchor.corrected(rope);
        let head = *self.head.corrected(rope);
        Self {
            anchor,
            head,
            target_column: None,
        }
        .valid_for(rope)
    }

    #[must_use]
    pub fn to_rope_slice<'rope>(&self, rope: &'rope Rope) -> (RopeSlice<'rope>, bool) {
        let (anchor_index, anchor_corrected) = self.anchor.to_rope_index(rope);
        let (head_index, head_corrected) = self.head.to_rope_index(rope);

        let slice = if self.is_forwards() {
            rope.get_slice(*anchor_index..=*head_index).unwrap()
        } else {
            rope.get_slice(*head_index..=*anchor_index).unwrap()
        };

        (slice, anchor_corrected || head_corrected)
    }

    #[must_use]
    fn vertically(&self, rope: &Rope, direction: Direction, distance: usize) -> Self {
        let desired_head = Position {
            line: NonZeroUsize::new(max(1, {
                match direction {
                    Direction::Backward => self.head().line.get().saturating_sub(distance),
                    Direction::Forward => {
                        // Subtracting 1 to remove ropey's mysterious empty final line
                        let last_line = rope.len_lines().saturating_sub(1);
                        // Prevent `corrected` from moving us to the last index in the rope if we try to go
                        // below the last line
                        min(self.head().line.get() + distance, last_line)
                    }
                }
            }))
            .unwrap(),
            column: self.target_column().unwrap_or(self.head().column),
        };

        let head = *desired_head.corrected(rope);

        let target_column = if head.column == desired_head.column {
            None
        } else {
            Some(desired_head.column)
        };

        Self::try_from((self.anchor(), head, target_column)).unwrap()
    }

    #[must_use]
    fn horizontally(&self, rope: &Rope, direction: Direction, distance: usize) -> Self {
        let index = *self.head().to_rope_index(rope).0;

        let desired_index = match direction {
            Direction::Backward => index.saturating_sub(distance),
            Direction::Forward => index + distance,
        };

        let head = *Position::from_rope_index(rope, desired_index).0;

        Self::from((self.anchor(), head))
    }

    #[must_use]
    pub fn move_up(&self, rope: &Rope, distance: usize) -> Self {
        self.extend_up(rope, distance).reduce()
    }

    #[must_use]
    pub fn move_down(&self, rope: &Rope, distance: usize) -> Self {
        self.extend_down(rope, distance).reduce()
    }

    #[must_use]
    pub fn move_left(&self, rope: &Rope, distance: usize) -> Self {
        self.extend_left(rope, distance).reduce()
    }

    #[must_use]
    pub fn move_right(&self, rope: &Rope, distance: usize) -> Self {
        self.extend_right(rope, distance).reduce()
    }

    #[must_use]
    pub fn move_top(&self) -> Self {
        self.extend_top().reduce()
    }

    #[must_use]
    pub fn move_bottom(&self, rope: &Rope) -> Self {
        self.extend_bottom(rope).reduce()
    }

    #[must_use]
    pub fn move_end(&self, rope: &Rope) -> Self {
        self.extend_end(rope).reduce()
    }

    #[must_use]
    pub fn move_line_begin(&self, rope: &Rope) -> Self {
        self.extend_line_begin(rope).reduce()
    }

    #[must_use]
    pub fn move_line_first_non_blank(&self, rope: &Rope) -> Self {
        self.extend_line_first_non_blank(rope).reduce()
    }

    #[must_use]
    pub fn move_line_end(&self, rope: &Rope) -> Self {
        self.extend_line_end(rope).reduce()
    }

    #[must_use]
    pub fn extend_up(&self, rope: &Rope, distance: usize) -> Self {
        self.vertically(rope, Direction::Backward, distance)
    }

    #[must_use]
    pub fn extend_down(&self, rope: &Rope, distance: usize) -> Self {
        self.vertically(rope, Direction::Forward, distance)
    }

    #[must_use]
    pub fn extend_left(&self, rope: &Rope, distance: usize) -> Self {
        self.horizontally(rope, Direction::Backward, distance)
    }

    #[must_use]
    pub fn extend_right(&self, rope: &Rope, distance: usize) -> Self {
        self.horizontally(rope, Direction::Forward, distance)
    }

    #[must_use]
    pub fn extend_top(&self) -> Self {
        Self::from((self.anchor(), (1, 1).try_into().unwrap()))
    }

    #[must_use]
    pub fn extend_bottom(&self, rope: &Rope) -> Self {
        // Subtracting 1 to remove ropey's mysterious empty final line
        let index = rope.line_to_char(rope.len_lines().saturating_sub(2));
        let head = *Position::from_rope_index(rope, index).0;
        Self::from((self.anchor(), head))
    }

    #[must_use]
    pub fn extend_end(&self, rope: &Rope) -> Self {
        let index = rope.len_chars().saturating_sub(1);
        let head = *Position::from_rope_index(rope, index).0;
        Self::from((self.anchor(), head))
    }

    #[must_use]
    pub fn extend_line_begin(&self, rope: &Rope) -> Self {
        let mut head = self.head();
        head.column = NonZeroUsize::new(1).unwrap();
        *Self::from((self.anchor(), head)).corrected(rope)
    }

    #[must_use]
    pub fn extend_line_first_non_blank(&self, rope: &Rope) -> Self {
        let blanks = [' ', '\t'];

        let first_non_blank = rope
            .line(self.head().line.get())
            .chars()
            .enumerate()
            .find(|(_, c)| !blanks.contains(c));

        let mut head = self.head();
        head.column = NonZeroUsize::new(match first_non_blank {
            // Behave like `extend_line_end` if there are no non-blank characters on this line
            None => rope.line(head.line.get()).len_chars().saturating_sub(1),
            Some((i, _)) => i,
        })
        .unwrap();

        Self::from((self.anchor(), head))
    }

    #[must_use]
    pub fn extend_line_end(&self, rope: &Rope) -> Self {
        let mut head = *self.head().corrected(rope);
        head.column = NonZeroUsize::new(rope.line(head.line.get()).len_chars()).unwrap();
        Self::from((self.anchor(), head))
    }

    #[cfg(debug_assertions)]
    pub fn assert_invariants(&self) {
        // Target column must be greater than head's column
        if let Some(column) = self.target_column {
            debug_assert!(column > self.head.column);
        }
    }
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

impl TryFrom<(Position, Position, Option<NonZeroUsize>)> for Range {
    type Error = ();

    fn try_from(
        (anchor, head, target_column): (Position, Position, Option<NonZeroUsize>),
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

impl TryFrom<(Position, Position, NonZeroUsize)> for Range {
    type Error = ();

    fn try_from(
        (anchor, head, target_column): (Position, Position, NonZeroUsize),
    ) -> Result<Self, Self::Error> {
        Self::try_from((anchor, head, Some(target_column)))
    }
}
