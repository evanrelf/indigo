use crate::position::Position;
use std::cmp::{max, min};

#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct Range {
    anchor: Position,
    cursor: Position,
    target_column: Option<usize>,
}

impl Range {
    pub fn anchor(&self) -> &Position {
        &self.anchor
    }

    pub fn cursor(&self) -> &Position {
        &self.cursor
    }

    pub fn target_column(&self) -> Option<usize> {
        self.target_column
    }

    pub fn is_forward(&self) -> bool {
        self.anchor <= self.cursor
    }

    pub fn is_backward(&self) -> bool {
        self.anchor > self.cursor
    }

    pub fn is_reduced(&self) -> bool {
        self.anchor == self.cursor
    }

    pub fn is_overlapping(&self, other: &Self) -> bool {
        let range1 = self.flip_forward();
        let range2 = other.flip_forward();
        (range1.anchor <= range2.anchor && range2.anchor <= range1.cursor)
            || (range2.anchor <= range1.anchor && range1.anchor <= range2.cursor)
    }

    pub fn set_target_column(&self, target_column: Option<usize>) -> Option<Self> {
        let range = Self {
            target_column,
            ..self.clone()
        };

        if range.is_valid() {
            Some(range)
        } else {
            None
        }
    }

    pub fn forget_target_column(&self) -> Self {
        Self {
            target_column: None,
            ..self.clone()
        }
    }

    pub fn flip(&self) -> Self {
        if self.is_reduced() {
            self.clone()
        } else {
            Self {
                anchor: self.cursor.clone(),
                cursor: self.anchor.clone(),
                target_column: None,
            }
        }
    }

    pub fn flip_forward(&self) -> Self {
        if self.is_backward() {
            self.flip()
        } else {
            self.clone()
        }
    }

    pub fn flip_backward(&self) -> Self {
        if self.is_forward() {
            self.flip()
        } else {
            self.clone()
        }
    }

    pub fn reduce(&self) -> Self {
        Self {
            anchor: self.cursor.clone(),
            ..self.clone()
        }
    }

    pub fn merge(&self, other: &Self) -> Self {
        match (self.is_forward(), other.is_forward()) {
            (true, true) => {
                // Forward
                let anchor = min(self.anchor.clone(), other.anchor.clone());
                let (cursor, target_column) = if self.cursor > other.cursor {
                    (self.cursor.clone(), self.target_column)
                } else {
                    (other.cursor.clone(), other.target_column)
                };
                Self {
                    anchor,
                    cursor,
                    target_column,
                }
            }
            (false, false) => {
                // Backward
                let anchor = max(self.anchor.clone(), other.anchor.clone());
                let (cursor, target_column) = if self.cursor < other.cursor {
                    (self.cursor.clone(), self.target_column)
                } else {
                    (other.cursor.clone(), other.target_column)
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
            anchor: position.clone(),
            cursor: position.clone(),
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
