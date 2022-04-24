use crate::range::Range;

// INVARIANTS:
// - Ranges must be sorted
// - Ranges must not overlap
#[derive(Default)]
pub struct Selection {
    ranges: Vec<Range>,
}
