use crate::range::Range;
use regex::Regex;
use ropey::Rope;

#[derive(Clone)]
pub struct Selection {
    ranges: Vec<Range>,
    primary_range_index: usize,
}

// TODO: Merge overlapping ranges

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

    #[must_use]
    pub fn move_up(&self, rope: &Rope, distance: usize) -> Self {
        let mut selection = self.clone();
        for range in &mut selection.ranges {
            *range = range.move_up(rope, distance);
        }
        selection
    }

    #[must_use]
    pub fn move_down(&self, rope: &Rope, distance: usize) -> Self {
        let mut selection = self.clone();
        for range in &mut selection.ranges {
            *range = range.move_down(rope, distance);
        }
        selection
    }

    #[must_use]
    pub fn move_left(&self, rope: &Rope, distance: usize) -> Self {
        let mut selection = self.clone();
        for range in &mut selection.ranges {
            *range = range.move_left(rope, distance);
        }
        selection
    }

    #[must_use]
    pub fn move_right(&self, rope: &Rope, distance: usize) -> Self {
        let mut selection = self.clone();
        for range in &mut selection.ranges {
            *range = range.move_right(rope, distance);
        }
        selection
    }

    #[must_use]
    pub fn move_top(&self) -> Self {
        let mut selection = self.clone();
        for range in &mut selection.ranges {
            *range = range.move_top();
        }
        selection
    }

    #[must_use]
    pub fn move_bottom(&self, rope: &Rope) -> Self {
        let mut selection = self.clone();
        for range in &mut selection.ranges {
            *range = range.move_bottom(rope);
        }
        selection
    }

    #[must_use]
    pub fn move_end(&self, rope: &Rope) -> Self {
        let mut selection = self.clone();
        for range in &mut selection.ranges {
            *range = range.move_end(rope);
        }
        selection
    }

    #[must_use]
    pub fn move_line_begin(&self) -> Self {
        let mut selection = self.clone();
        for range in &mut selection.ranges {
            *range = range.move_line_begin();
        }
        selection
    }

    #[must_use]
    pub fn move_line_first_non_blank(&self, rope: &Rope) -> Self {
        let mut selection = self.clone();
        for range in &mut selection.ranges {
            *range = range.move_line_first_non_blank(rope);
        }
        selection
    }

    #[must_use]
    pub fn move_line_end(&self, rope: &Rope) -> Self {
        let mut selection = self.clone();
        for range in &mut selection.ranges {
            *range = range.move_line_end(rope);
        }
        selection
    }

    #[must_use]
    pub fn extend_up(&self, rope: &Rope, distance: usize) -> Self {
        let mut selection = self.clone();
        for range in &mut selection.ranges {
            *range = range.extend_up(rope, distance);
        }
        selection
    }

    #[must_use]
    pub fn extend_down(&self, rope: &Rope, distance: usize) -> Self {
        let mut selection = self.clone();
        for range in &mut selection.ranges {
            *range = range.extend_down(rope, distance);
        }
        selection
    }

    #[must_use]
    pub fn extend_left(&self, rope: &Rope, distance: usize) -> Self {
        let mut selection = self.clone();
        for range in &mut selection.ranges {
            *range = range.extend_left(rope, distance);
        }
        selection
    }

    #[must_use]
    pub fn extend_right(&self, rope: &Rope, distance: usize) -> Self {
        let mut selection = self.clone();
        for range in &mut selection.ranges {
            *range = range.extend_right(rope, distance);
        }
        selection
    }

    #[must_use]
    pub fn extend_top(&self) -> Self {
        let mut selection = self.clone();
        for range in &mut selection.ranges {
            *range = range.extend_top();
        }
        selection
    }

    #[must_use]
    pub fn extend_bottom(&self, rope: &Rope) -> Self {
        let mut selection = self.clone();
        for range in &mut selection.ranges {
            *range = range.extend_bottom(rope);
        }
        selection
    }

    #[must_use]
    pub fn extend_end(&self, rope: &Rope) -> Self {
        let mut selection = self.clone();
        for range in &mut selection.ranges {
            *range = range.extend_end(rope);
        }
        selection
    }

    #[must_use]
    pub fn extend_line_begin(&self) -> Self {
        let mut selection = self.clone();
        for range in &mut selection.ranges {
            *range = range.extend_line_begin();
        }
        selection
    }

    #[must_use]
    pub fn extend_line_first_non_blank(&self, rope: &Rope) -> Self {
        let mut selection = self.clone();
        for range in &mut selection.ranges {
            *range = range.extend_line_first_non_blank(rope);
        }
        selection
    }

    #[must_use]
    pub fn extend_line_end(&self, rope: &Rope) -> Self {
        let mut selection = self.clone();
        for range in &mut selection.ranges {
            *range = range.extend_line_end(rope);
        }
        selection
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
