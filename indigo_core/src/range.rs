use crate::position::Position;
use ropey::{Rope, RopeSlice};
use std::{
    cmp::{max, min},
    num::NonZeroUsize,
};

#[derive(Copy, Clone, Default, PartialEq)]
pub struct Range {
    anchor: Position,
    head: Position,
    target_column: Option<NonZeroUsize>,
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

    pub fn flip(&self) -> Self {
        Self {
            anchor: self.head,
            head: self.anchor,
            target_column: None,
        }
    }

    pub fn flip_forwards(&self) -> Self {
        if self.is_backwards() {
            self.flip()
        } else {
            *self
        }
    }

    pub fn flip_backwards(&self) -> Self {
        if self.is_forwards() {
            self.flip()
        } else {
            *self
        }
    }

    pub fn reduce(&self) -> Self {
        Self {
            anchor: self.head,
            ..*self
        }
    }

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
    pub fn to_rope_slice<'rope>(&self, rope: &'rope Rope) -> Option<RopeSlice<'rope>> {
        let anchor_index = self.anchor.to_rope_index(rope)?;
        let head_index = self.head.to_rope_index(rope)?;
        if self.is_forwards() {
            rope.get_slice(anchor_index..=head_index)
        } else {
            rope.get_slice(head_index..=anchor_index)
        }
    }

    #[must_use]
    pub fn to_rope_slice_corrected<'rope>(&self, rope: &'rope Rope) -> RopeSlice<'rope> {
        let anchor_index = self.anchor.to_rope_index_corrected(rope);
        let head_index = self.head.to_rope_index_corrected(rope);
        let rope_slice = if self.is_forwards() {
            rope.slice(anchor_index..=head_index)
        } else {
            rope.slice(head_index..=anchor_index)
        };
        rope_slice
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
