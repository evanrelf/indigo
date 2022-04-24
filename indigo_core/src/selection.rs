use crate::range::Range;

// INVARIANTS:
// - Must be at least one range
// - Ranges must be sorted
// - Ranges must not overlap
pub struct Selection {
    ranges: Vec<Range>,
    primary_range: usize,
}

impl Selection {
    #[must_use]
    pub fn ranges(&self) -> &Vec<Range> {
        &self.ranges
    }

    #[must_use]
    pub fn primary_range(&self) -> usize {
        self.primary_range
    }
}

impl Default for Selection {
    fn default() -> Self {
        Self {
            ranges: vec![Range::default()],
            primary_range: 0,
        }
    }
}

impl From<Range> for Selection {
    fn from(range: Range) -> Self {
        Self {
            ranges: vec![range],
            primary_range: 0,
        }
    }
}
