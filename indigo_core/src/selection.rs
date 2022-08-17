use crate::range::{self, Range};
use regex::Regex;
use ropey::Rope;

#[derive(Clone, Debug, PartialEq)]
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
    pub fn primary_range(&self) -> (usize, &Range) {
        (
            self.primary_range_index,
            &self.ranges[self.primary_range_index],
        )
    }

    #[must_use]
    pub fn select(&self, rope: &Rope, regex: &Regex) -> Option<Self> {
        self.select_c(rope, regex).0
    }

    #[doc(hidden)]
    #[must_use]
    pub fn select_c(&self, rope: &Rope, regex: &Regex) -> (Option<Self>, bool) {
        let mut corrected = false;
        let mut ranges = Vec::new();

        for range in &self.ranges {
            let (mut sub_ranges, sub_ranges_corrected) = range.select_c(rope, regex);
            corrected |= sub_ranges_corrected;
            ranges.append(&mut sub_ranges);
        }

        let selection = if ranges.is_empty() {
            None
        } else {
            Some(Self {
                primary_range_index: ranges.len() - 1,
                ranges,
            })
        };

        (selection, corrected)
    }

    #[must_use]
    pub fn update_ranges(&self, mut range_fn: impl FnMut(usize, &Range) -> Range) -> Self {
        let mut selection = self.clone();

        for (index, range) in selection.ranges.iter_mut().enumerate() {
            *range = range_fn(index, range);
        }

        selection.merge()
    }

    #[must_use]
    pub fn update_primary_range(&self, range_fn: impl Fn(usize, &Range) -> Range) -> Self {
        let mut selection = self.clone();

        let index = selection.primary_range_index;
        let range = &mut selection.ranges[index];

        *range = range_fn(index, range);

        selection.merge()
    }

    #[must_use]
    fn merge(&self) -> Self {
        let mut ranges: Vec<Range> = Vec::new();
        let mut primary_range_index = self.primary_range_index;

        for range in &self.ranges {
            match ranges.last_mut() {
                Some(last_range) if last_range.is_overlapping(range) => {
                    *last_range = last_range.merge(range);
                    primary_range_index = ranges.len() - 1;
                }
                _ => ranges.push(*range),
            }
        }

        Self {
            ranges,
            primary_range_index,
        }
    }

    pub fn assert_invariants(&self) {
        assert!(!self.ranges.is_empty(), "must have at least one range");

        assert!(
            self.ranges.get(self.primary_range_index).is_some(),
            "primary range index must be valid"
        );

        assert!(
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

        assert!(
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

#[must_use]
pub fn selection_is_valid(selection: &Selection, rope: Option<&Rope>) -> bool {
    for range in &selection.ranges {
        if !range::range_is_valid(range, rope) {
            return false;
        }
    }
    true
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

#[cfg(test)]
mod test {
    use super::*;
    use crate::position::Position;

    #[test]
    fn test_merge() {
        let tuples_to_ranges = |vec: Vec<_>| {
            vec.into_iter()
                .map(|((anchor_line, anchor_column), (head_line, head_column))| {
                    Range::from((
                        Position {
                            line: anchor_line,
                            column: anchor_column,
                        },
                        Position {
                            line: head_line,
                            column: head_column,
                        },
                    ))
                })
                .collect()
        };

        let input_ranges = tuples_to_ranges(vec![
            ((1, 1), (1, 10)),
            ((1, 2), (1, 9)),
            ((2, 1), (2, 10)),
            ((2, 2), (2, 9)),
        ]);

        let input_selection = Selection {
            ranges: input_ranges,
            primary_range_index: 2,
        };

        let expected_ranges = tuples_to_ranges(vec![((1, 1), (1, 10)), ((2, 1), (2, 10))]);

        let expected_selection = Selection {
            ranges: expected_ranges,
            primary_range_index: 1,
        };

        expected_selection.assert_invariants();

        let actual_selection = input_selection.merge();

        assert_eq!(expected_selection, actual_selection);
    }
}
