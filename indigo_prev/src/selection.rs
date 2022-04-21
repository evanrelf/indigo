use crate::{range::Range, regex::select};
use regex::Regex;
use ropey::Rope;

// TODO: Hide fields, add methods that keep `ranges` sorted and non-overlapping
pub struct Selection {
    pub ranges: Vec<Range>,
    pub primary_range_index: usize,
}

impl Selection {
    #[must_use]
    pub fn new(ranges: &[Range]) -> Self {
        debug_assert!(!ranges.is_empty());

        Self {
            ranges: ranges.to_vec(),
            primary_range_index: 0,
        }
    }

    pub fn next_range(&mut self, count: usize) {
        self.primary_range_index =
            wrap_around(self.ranges.len(), self.primary_range_index, count as isize);
    }

    pub fn previous_range(&mut self, count: usize) {
        self.primary_range_index = wrap_around(
            self.ranges.len(),
            self.primary_range_index,
            -(count as isize),
        );
    }

    pub fn filter_ranges<F>(&mut self, filter: F)
    where
        F: Fn(usize, &Range) -> bool,
    {
        self.ranges = self
            .ranges
            .iter()
            .enumerate()
            .filter(|(i, r)| filter(*i, r))
            .map(|(_, r)| r)
            .cloned()
            .collect();
    }

    pub fn primary_range(&mut self) -> &mut Range {
        &mut self.ranges[self.primary_range_index]
    }

    pub fn in_all_ranges<F>(&mut self, f: F)
    where
        F: Fn(&mut Range),
    {
        for range in &mut self.ranges {
            f(range);
        }
    }

    pub fn select(&mut self, rope: &Rope, regex: &Regex) {
        self.ranges = self
            .ranges
            .iter()
            .flat_map(|range| select(rope, range, regex))
            .collect();
        self.primary_range_index = 0; // TODO
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

// Modified version of https://stackoverflow.com/a/39740009
fn wrap_around(length: usize, value: usize, delta: isize) -> usize {
    let length = length as isize;
    if length == 0 {
        0
    } else {
        let value = value as isize;
        let result = if delta >= 0 {
            (value + delta) % length
        } else {
            ((value + delta) - (delta * length)) % length
        };
        result as usize
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_wrap_around() {
        assert_eq!(wrap_around(0, 0, 0), 0);
        assert_eq!(wrap_around(0, 0, 4), 0);
        assert_eq!(wrap_around(2, 0, 0), 0);
        assert_eq!(wrap_around(2, 0, 3), 1);
        assert_eq!(wrap_around(2, 0, 4), 0);
        assert_eq!(wrap_around(2, 0, -3), 1);
        assert_eq!(wrap_around(2, 0, -4), 0);
    }
}