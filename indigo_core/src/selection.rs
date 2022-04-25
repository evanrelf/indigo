use crate::range::Range;
use regex::Regex;
use ropey::Rope;

#[derive(Clone)]
pub struct Selection {
    ranges: Vec<Range>,
    primary_range_index: usize,
}

impl Selection {
    #[must_use]
    pub fn ranges(&self) -> &Vec<Range> {
        &self.ranges
    }

    #[must_use]
    pub fn primary_range_index(&self) -> usize {
        self.primary_range_index
    }

    #[must_use]
    pub fn primary_range(&self) -> &Range {
        &self.ranges[self.primary_range_index]
    }

    #[must_use]
    pub fn select(&self, rope: &Rope, regex: &Regex) -> Option<Self> {
        let mut ranges = Vec::new();

        for range in &self.ranges {
            ranges.append(&mut range.select(rope, regex)?);
        }

        if ranges.is_empty() {
            None
        } else {
            Some(Self {
                primary_range_index: ranges.len() - 1,
                ranges,
            })
        }
    }

    #[must_use]
    pub fn select_corrected(&self, rope: &Rope, regex: &Regex) -> Option<Self> {
        let mut ranges = Vec::new();

        for range in &self.ranges {
            ranges.append(&mut range.select_corrected(rope, regex));
        }

        if ranges.is_empty() {
            None
        } else {
            Some(Self {
                primary_range_index: ranges.len() - 1,
                ranges,
            })
        }
    }

    pub fn map_primary_range<F>(&self, range_fn: F) -> Self
    where
        F: Fn(&Range) -> Range,
    {
        let mut selection = self.clone();
        let primary_range = &mut selection.ranges[selection.primary_range_index];
        *primary_range = range_fn(primary_range);
        selection

        // TODO: Merge overlapping ranges
    }

    pub fn map_ranges<F>(&self, range_fn: F) -> Self
    where
        F: Fn(&Range) -> Range,
    {
        let mut selection = self.clone();
        for range in &mut selection.ranges {
            *range = range_fn(range);
        }
        selection

        // TODO: Merge overlapping ranges
    }

    #[cfg(debug_assertions)]
    pub fn assert_invariants(&self) {
        debug_assert!(!self.ranges.is_empty(), "must have at least one range");

        debug_assert!(
            self.ranges.get(self.primary_range_index).is_some(),
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
            primary_range_index: 0,
        }
    }
}

impl From<Range> for Selection {
    fn from(range: Range) -> Self {
        Self {
            ranges: vec![range],
            primary_range_index: 0,
        }
    }
}
