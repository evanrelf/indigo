use crate::range::Range;

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
    pub fn primary_range(&self) -> &Range {
        &self.ranges[self.primary_range]
    }

    #[cfg(debug_assertions)]
    pub fn assert_invariants(&self) {
        debug_assert!(!self.ranges.is_empty(), "must have at least one range");

        debug_assert!(
            self.ranges.get(self.primary_range).is_some(),
            "primary range index must be valid"
        );

        debug_assert!(
            {
                let mut sorted = self.ranges.clone();
                sorted.sort_by(|left, right| {
                    let left = left.flip_forwards();
                    let right = right.flip_forwards();
                    left.anchor().cmp(&right.anchor())
                });
                self.ranges == sorted
            },
            "ranges must be sorted"
        );

        debug_assert!(
            {
                let mut overlapping = false;
                for (outer_index, outer_range) in self.ranges.iter().enumerate() {
                    for (inner_index, inner_range) in self.ranges.iter().enumerate() {
                        if outer_index == inner_index {
                            continue;
                        }
                        if outer_range.is_overlapping(inner_range) {
                            overlapping = true;
                            break;
                        }
                    }
                }
                !overlapping
            },
            "ranges must not overlap"
        );
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
