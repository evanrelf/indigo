use crate::position::Position;
use ropey::{Rope, RopeSlice};
use std::num::NonZeroUsize;

// INVARIANTS:
// - Target column must be greater than head's column
// - Target column must be cleared when swapping anchor and head
#[derive(Copy, Clone, Default)]
pub struct Range {
    anchor: Position,
    head: Position,
    target_column: Option<NonZeroUsize>,
}

impl Range {
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

    pub fn flip(&mut self) {
        std::mem::swap(&mut self.anchor, &mut self.head);
        self.target_column = None;
    }

    pub fn flip_forwards(&mut self) {
        if self.is_backwards() {
            self.flip();
        }
    }

    pub fn flip_backwards(&mut self) {
        if self.is_forwards() {
            self.flip();
        }
    }

    pub fn reduce(&mut self) {
        self.anchor = self.head;
    }

    pub fn merge(&mut self, mut other: Self) -> bool {
        if !self.is_overlapping(&other) {
            return false;
        }

        match (self.is_forwards(), other.is_forwards()) {
            (true, true) => {
                // Forwards
                if other.anchor < self.anchor {
                    self.anchor = other.anchor;
                }
                if other.head > self.head {
                    self.head = other.head;
                    self.target_column = other.target_column;
                }
            }
            (false, false) => {
                // Backwards
                if other.anchor > self.anchor {
                    self.anchor = other.anchor;
                }
                if other.head < self.head {
                    self.head = other.head;
                    self.target_column = other.target_column;
                }
            }
            _ => {
                // Mixed
                other.flip();
                self.merge(other);
            }
        }

        true
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
