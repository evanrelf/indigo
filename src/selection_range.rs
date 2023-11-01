use crate::{position::Position, range::Range};
use std::cmp::{max, min};

#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct SelectionRange {
    start: Position,
    end: Position,
}

impl SelectionRange {
    pub fn start(&self) -> &Position {
        &self.start
    }

    pub fn end(&self) -> &Position {
        &self.end
    }

    pub fn is_reduced(&self) -> bool {
        self.start == self.end
    }

    pub fn is_overlapping(&self, other: &Self) -> bool {
        self.start <= other.end && other.start <= self.end
    }

    pub fn merge(&self, other: &Self) -> Self {
        Self {
            start: min(self.start.clone(), other.start.clone()),
            end: max(self.end.clone(), other.end.clone()),
        }
    }

    pub fn is_valid(&self) -> bool {
        self.start <= self.end
    }
}

impl From<Position> for SelectionRange {
    fn from(position: Position) -> Self {
        Self {
            start: position.clone(),
            end: position.clone(),
        }
    }
}

impl From<(Position, Position)> for SelectionRange {
    fn from((position1, position2): (Position, Position)) -> Self {
        Self {
            start: min(position1.clone(), position2.clone()),
            end: max(position1.clone(), position2.clone()),
        }
    }
}

impl From<Range> for SelectionRange {
    fn from(range: Range) -> Self {
        Self::from((range.anchor().clone(), range.cursor().clone()))
    }
}

impl From<SelectionRange> for (Position, Position) {
    fn from(range: SelectionRange) -> Self {
        (range.start, range.end)
    }
}
