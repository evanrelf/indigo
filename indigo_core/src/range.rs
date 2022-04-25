use crate::position::Position;
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
    pub fn select(&self, rope: &Rope, regex: &Regex) -> Option<Vec<Self>> {
        let offset = if self.is_forwards() {
            self.anchor.to_rope_index(rope)?
        } else {
            self.head.to_rope_index(rope)?
        };

        let rope_slice = self.to_rope_slice(rope)?;

        let cow = Cow::<str>::from(rope_slice);

        let str = if let Some(str) = rope_slice.as_str() {
            str
        } else {
            cow.as_ref()
        };

        Some(
            regex
                .find_iter(str)
                .map(|match_| {
                    let start_index = offset + rope.byte_to_char(match_.start());
                    let start_position = Position::from_rope_index(rope, start_index).unwrap();

                    let end_index = offset + rope.byte_to_char(match_.end()).saturating_sub(1);
                    let end_position = Position::from_rope_index(rope, end_index).unwrap();

                    if self.is_forwards() {
                        Self::from((start_position, end_position))
                    } else {
                        Self::from((end_position, start_position))
                    }
                })
                .collect(),
        )
    }

    #[must_use]
    pub fn select_corrected(&self, rope: &Rope, regex: &Regex) -> Vec<Self> {
        let offset = if self.is_forwards() {
            self.anchor.to_rope_index_corrected(rope)
        } else {
            self.head.to_rope_index_corrected(rope)
        };

        let rope_slice = self.to_rope_slice_corrected(rope);

        let cow = Cow::<str>::from(rope_slice);

        let str = if let Some(str) = rope_slice.as_str() {
            str
        } else {
            cow.as_ref()
        };

        regex
            .find_iter(str)
            .map(|match_| {
                let start_index = offset + rope.byte_to_char(match_.start());
                let start_position = Position::from_rope_index(rope, start_index).unwrap();

                let end_index = offset + rope.byte_to_char(match_.end()).saturating_sub(1);
                let end_position = Position::from_rope_index(rope, end_index).unwrap();

                if self.is_forwards() {
                    Self::from((start_position, end_position))
                } else {
                    Self::from((end_position, start_position))
                }
            })
            .collect()
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
