use crate::range::Range;

// INVARIANTS:
// - Must be at least one range
// - Ranges must be sorted
// - Ranges must not overlap
pub struct Selection {
    ranges: Vec<Range>,
}

impl Default for Selection {
    fn default() -> Self {
        Self {
            ranges: vec![Range::default()],
        }
    }
}
