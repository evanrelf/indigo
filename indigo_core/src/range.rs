use crate::position::Position;
use std::num::NonZeroUsize;

// INVARIANTS:
// - Target column must be greater than head's column
// - Target column must be cleared when swapping anchor and head
#[derive(Default)]
pub struct Range {
    anchor: Position,
    head: Position,
    target_column: Option<NonZeroUsize>,
}
