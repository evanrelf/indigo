use crate::Range;
use fancy_regex::Regex;
use ropey::Rope;
use std::borrow::Cow;

#[derive(Debug)]
pub struct Selection<'buffer> {
    rope: &'buffer Rope,
    state: &'buffer SelectionState,
}

impl<'buffer> Selection<'buffer> {
    #[must_use]
    pub(crate) fn new(rope: &'buffer Rope, state: &'buffer SelectionState) -> Self {
        Self { rope, state }
    }
}

#[derive(Debug)]
pub struct SelectionMut<'buffer> {
    rope: &'buffer mut Rope,
    state: &'buffer mut SelectionState,
}

impl<'buffer> SelectionMut<'buffer> {
    #[must_use]
    pub(crate) fn new(rope: &'buffer mut Rope, state: &'buffer mut SelectionState) -> Self {
        Self { rope, state }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) struct SelectionState {
    ranges: Vec<Range>,
    primary: usize,
}

impl Default for SelectionState {
    fn default() -> Self {
        Self {
            ranges: vec![Range::default()],
            primary: 0,
        }
    }
}

impl SelectionState {
    #[must_use]
    pub fn ranges(&self) -> &Vec<Range> {
        &self.ranges
    }

    #[must_use]
    pub fn primary(&self) -> Range {
        self.ranges[self.primary]
    }

    #[must_use]
    pub fn is_overlapping(&self) -> bool {
        // TODO: omg this is so inefficient
        for (i1, r1) in self.ranges.iter().enumerate() {
            for (i2, r2) in self.ranges.iter().enumerate() {
                if i1 == i2 {
                    continue;
                }
                if r1.is_overlapping(r2) {
                    return true;
                }
            }
        }
        false
    }

    pub fn set_primary(&mut self, index: usize) -> anyhow::Result<()> {
        if index >= self.ranges.len() {
            anyhow::bail!("Invalid range index {index}");
        }
        self.primary = index;
        Ok(())
    }

    pub fn insert(&mut self, range: Range) {
        let index = match self.ranges.binary_search(&range) {
            Ok(index) | Err(index) => index, // TODO: Merge overlapping ranges?
        };
        self.ranges.insert(index, range);
    }

    pub fn remove(&mut self, index: usize) -> anyhow::Result<()> {
        if self.ranges.len() <= 1 {
            anyhow::bail!("Cannot remove final range");
        }

        if index >= self.ranges.len() {
            anyhow::bail!("Invalid range index {index}");
        }

        self.ranges.remove(index);

        if self.primary < index {
            self.primary -= 1;
        }

        Ok(())
    }

    pub fn filter<P>(&mut self, predicate: P) -> anyhow::Result<()>
    where
        P: Fn(&Range) -> anyhow::Result<bool>,
    {
        let mut selection_state = self.clone();

        for (i, range) in self.ranges.iter().enumerate() {
            if !predicate(range)? {
                selection_state.remove(i)?;
            }
        }

        *self = selection_state;

        Ok(())
    }

    pub fn flip(&mut self) {
        for range in &mut self.ranges {
            range.flip();
        }
    }

    pub fn flip_forward(&mut self) {
        for range in &mut self.ranges {
            range.flip_forward();
        }
    }

    pub fn flip_backward(&mut self) {
        for range in &mut self.ranges {
            range.flip_backward();
        }
    }

    pub fn select(&mut self, rope: &Rope, regex: &Regex) -> anyhow::Result<()> {
        let mut ranges = Vec::new();

        for range in &self.ranges {
            let mut sub_ranges = range.select(rope, regex)?;
            ranges.append(&mut sub_ranges);
        }

        if ranges.is_empty() {
            anyhow::bail!("No matches");
        }

        self.primary = ranges.len() - 1;
        self.ranges = ranges;

        Ok(())
    }

    pub fn keep(&mut self, rope: &Rope, regex: &Regex) -> anyhow::Result<()> {
        self.filter(|range| {
            let rope_slice = range.to_rope_slice(rope)?;
            let string_slice = Cow::<str>::from(rope_slice);
            let keep = regex.is_match(&string_slice)?;
            Ok(keep)
        })
    }

    pub fn merge(&mut self) {
        let mut ranges: Vec<Range> = Vec::new();
        let mut primary = self.primary;

        for range in &self.ranges {
            match ranges.last_mut() {
                Some(last_range) if last_range.is_overlapping(range) => {
                    last_range.merge(range);
                    primary = ranges.len() - 1;
                }
                _ => ranges.push(*range),
            }
        }

        self.ranges = ranges;
        self.primary = primary;
    }

    pub fn assert_valid(&self) {
        assert!(!self.ranges.is_empty(), "`ranges` isn't empty");

        assert!(
            self.ranges.get(self.primary).is_some(),
            "`primary` index is valid"
        );

        // TODO: Use `is_sorted` once it stabilized
        // https://github.com/rust-lang/rust/issues/53485
        assert!(
            self.ranges.windows(2).all(|range| range[0] <= range[1]),
            "Ranges are sorted"
        );

        // Allowing overlap for now
        // assert!(!self.is_overlapping(), "Ranges must not overlap");

        for range in &self.ranges {
            range.assert_valid();
        }
    }
}

impl From<Range> for SelectionState {
    fn from(range: Range) -> Self {
        Self {
            ranges: vec![range],
            primary: 0,
        }
    }
}
