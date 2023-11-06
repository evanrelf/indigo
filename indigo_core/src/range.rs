use crate::{conversion::Conversion, direction::Direction, position::Position};
use ropey::{Rope, RopeSlice};
use std::cmp::{max, min};

#[derive(Clone, Copy, Debug, Default, Eq, Ord, PartialEq, PartialOrd)]
pub struct Range {
    anchor: Position,
    cursor: Position,
    target_column: Option<usize>,
}

impl Range {
    #[must_use]
    pub fn anchor(&self) -> Position {
        self.anchor
    }

    #[must_use]
    pub fn cursor(&self) -> Position {
        self.cursor
    }

    #[must_use]
    pub fn target_column(&self) -> Option<usize> {
        self.target_column
    }

    #[must_use]
    pub fn start(&self) -> Position {
        min(self.anchor, self.cursor)
    }

    #[must_use]
    pub fn end(&self) -> Position {
        max(self.anchor, self.cursor)
    }

    #[must_use]
    pub fn direction(&self) -> Direction {
        if self.is_forward() {
            Direction::Forward
        } else {
            Direction::Backward
        }
    }

    #[must_use]
    pub fn is_forward(&self) -> bool {
        self.anchor <= self.cursor
    }

    #[must_use]
    pub fn is_backward(&self) -> bool {
        self.anchor > self.cursor
    }

    #[must_use]
    pub fn is_reduced(&self) -> bool {
        self.anchor == self.cursor
    }

    #[must_use]
    pub fn is_overlapping(&self, other: &Self) -> bool {
        self.start() <= other.end() && other.start() <= self.end()
    }

    #[must_use]
    pub fn with_target_column(&self, target_column: usize) -> Option<Self> {
        if self.cursor.column < target_column {
            Some(Self {
                target_column: Some(target_column),
                ..*self
            })
        } else {
            None
        }
    }

    #[must_use]
    pub fn without_target_column(&self) -> Self {
        Self {
            target_column: None,
            ..*self
        }
    }

    #[must_use]
    pub fn flip(&self) -> Self {
        if self.is_reduced() {
            *self
        } else {
            Self {
                anchor: self.cursor,
                cursor: self.anchor,
                target_column: None,
            }
        }
    }

    #[must_use]
    pub fn flip_forward(&self) -> Self {
        if self.is_backward() {
            self.flip()
        } else {
            *self
        }
    }

    #[must_use]
    pub fn flip_backward(&self) -> Self {
        if self.is_forward() {
            self.flip()
        } else {
            *self
        }
    }

    #[must_use]
    pub fn reduce(&self) -> Self {
        Self {
            anchor: self.cursor,
            ..*self
        }
    }

    #[must_use]
    pub fn merge(&self, other: &Self) -> Self {
        match (self.is_forward(), other.is_forward()) {
            (true, true) => {
                // Forward
                let anchor = min(self.anchor, other.anchor);
                let (cursor, target_column) = if self.cursor > other.cursor {
                    (self.cursor, self.target_column)
                } else {
                    (other.cursor, other.target_column)
                };
                Self {
                    anchor,
                    cursor,
                    target_column,
                }
            }
            (false, false) => {
                // Backward
                let anchor = max(self.anchor, other.anchor);
                let (cursor, target_column) = if self.cursor < other.cursor {
                    (self.cursor, self.target_column)
                } else {
                    (other.cursor, other.target_column)
                };
                Self {
                    anchor,
                    cursor,
                    target_column,
                }
            }
            _ => {
                // Mixed
                self.merge(&other.flip())
            }
        }
    }

    #[must_use]
    pub fn to_rope_slice<'rope>(&self, rope: &'rope Rope) -> Conversion<RopeSlice<'rope>, ()> {
        let (anchor_index, anchor_corrected) = match self.anchor.to_char_index(rope) {
            Conversion::Invalid(()) => return Conversion::Invalid(()),
            Conversion::Corrected(index) => (index, true),
            Conversion::Valid(index) => (index, false),
        };
        let (cursor_index, cursor_corrected) = match self.cursor.to_char_index(rope) {
            Conversion::Invalid(()) => return Conversion::Invalid(()),
            Conversion::Corrected(index) => (index, true),
            Conversion::Valid(index) => (index, false),
        };

        let slice = match self.direction() {
            Direction::Forward => rope.get_slice(anchor_index..=cursor_index).unwrap(),
            Direction::Backward => rope.get_slice(cursor_index..=anchor_index).unwrap(),
        };

        if anchor_corrected || cursor_corrected {
            Conversion::Corrected(slice)
        } else {
            Conversion::Valid(slice)
        }
    }

    #[must_use]
    pub fn is_valid(&self) -> bool {
        match self.target_column {
            None => true,
            Some(target_column) => self.cursor.column < target_column,
        }
    }
}

impl From<Position> for Range {
    fn from(position: Position) -> Self {
        Self {
            anchor: position,
            cursor: position,
            target_column: None,
        }
    }
}

impl From<(Position, Position)> for Range {
    fn from((anchor, cursor): (Position, Position)) -> Self {
        Self {
            anchor,
            cursor,
            target_column: None,
        }
    }
}

impl From<Range> for (Position, Position) {
    fn from(range: Range) -> Self {
        (range.anchor, range.cursor)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use quickcheck_macros::quickcheck;

    #[test]
    fn range_default() {
        assert_eq!(
            Range::default(),
            Range {
                anchor: Position::from((0, 0)),
                cursor: Position::from((0, 0)),
                target_column: None,
            }
        );
    }

    #[quickcheck]
    fn range_start(line1: usize, column1: usize, line2: usize, column2: usize) -> bool {
        let anchor = Position::from((line1, column1));
        let cursor = Position::from((line2, column2));
        let range = Range::from((anchor, cursor));
        range.start() == range.flip().start()
    }

    #[quickcheck]
    fn range_end(line1: usize, column1: usize, line2: usize, column2: usize) -> bool {
        let anchor = Position::from((line1, column1));
        let cursor = Position::from((line2, column2));
        let range = Range::from((anchor, cursor));
        range.end() == range.flip().end()
    }
}
