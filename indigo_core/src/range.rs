use crate::position::Position;
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
