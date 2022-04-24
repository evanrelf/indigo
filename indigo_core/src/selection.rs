use crate::range::Range;

// INVARIANTS:
// - Must be at least one range
// - Ranges must be sorted
// - Ranges must not overlap
pub struct Selection {
    ranges: Vec<Range>,
}

impl Selection {
    #[must_use]
    pub fn ranges(&self) -> &Vec<Range> {
        &self.ranges
    }
}

impl Default for Selection {
    fn default() -> Self {
        Self {
            ranges: vec![Range::default()],
        }
    }
}

impl From<Range> for Selection {
    fn from(range: Range) -> Self {
        Self {
            ranges: vec![range],
        }
    }
}
