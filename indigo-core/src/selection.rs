use crate::Range;
use anyhow::Context as _;
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

    #[must_use]
    pub fn ranges(&self) -> &Vec<Range> {
        self.state.ranges()
    }

    #[must_use]
    pub fn primary_range(&self) -> Range {
        self.state.primary_range()
    }

    #[must_use]
    pub fn ranges_overlapping(&self) -> bool {
        self.state.ranges_overlapping()
    }

    pub fn assert_valid(&self) {
        self.state.assert_valid();
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

    #[must_use]
    pub fn ranges(&self) -> &Vec<Range> {
        self.state.ranges()
    }

    #[must_use]
    pub fn primary_range(&self) -> Range {
        self.state.primary_range()
    }

    #[must_use]
    pub fn ranges_overlapping(&self) -> bool {
        self.state.ranges_overlapping()
    }

    pub fn set_primary_range(&mut self, index: usize) -> anyhow::Result<()> {
        self.state.set_primary_range(index)
    }

    pub fn insert_range(&mut self, range: Range) {
        self.state.insert_range(range);
    }

    pub fn remove_range(&mut self, index: usize) -> anyhow::Result<()> {
        self.state.remove_range(index)
    }

    pub fn filter_ranges<P>(&mut self, predicate: P) -> anyhow::Result<()>
    where
        P: Fn(&Range) -> anyhow::Result<bool>,
    {
        self.state.filter_ranges(predicate)
    }

    pub fn flip_ranges(&mut self) {
        self.state.flip_ranges();
    }

    pub fn flip_ranges_forward(&mut self) {
        self.state.flip_ranges_forward();
    }

    pub fn flip_ranges_backward(&mut self) {
        self.state.flip_ranges_backward();
    }

    pub fn select_ranges(&mut self, regex: &Regex) -> anyhow::Result<()> {
        self.state.select_ranges(regex, self.rope)
    }

    pub fn keep_ranges(&mut self, regex: &Regex) -> anyhow::Result<()> {
        self.state.keep_ranges(regex, self.rope)
    }

    pub fn merge_ranges(&mut self) {
        self.state.merge_ranges();
    }

    pub fn insert_char(&mut self, c: char) {
        self.state
            .insert_char(c, self.rope)
            .expect("insert never fails because selection positions are always valid");
    }

    pub fn insert(&mut self, s: &str) {
        self.state
            .insert(s, self.rope)
            .expect("insert never fails because selection positions are always valid");
    }

    pub fn assert_valid(&self) {
        self.state.assert_valid();
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
    fn ranges(&self) -> &Vec<Range> {
        &self.ranges
    }

    #[must_use]
    fn primary_range(&self) -> Range {
        self.ranges[self.primary]
    }

    #[must_use]
    fn ranges_overlapping(&self) -> bool {
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

    fn set_primary_range(&mut self, index: usize) -> anyhow::Result<()> {
        if index >= self.ranges.len() {
            anyhow::bail!("Invalid range index {index}");
        }
        self.primary = index;
        Ok(())
    }

    fn insert_range(&mut self, range: Range) {
        let index = match self.ranges.binary_search(&range) {
            Ok(index) | Err(index) => index, // TODO: Merge overlapping ranges?
        };
        self.ranges.insert(index, range);
    }

    fn remove_range(&mut self, index: usize) -> anyhow::Result<()> {
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

    fn filter_ranges<P>(&mut self, predicate: P) -> anyhow::Result<()>
    where
        P: Fn(&Range) -> anyhow::Result<bool>,
    {
        let mut selection_state = self.clone();

        for (i, range) in self.ranges.iter().enumerate() {
            if !predicate(range)? {
                selection_state.remove_range(i)?;
            }
        }

        *self = selection_state;

        Ok(())
    }

    fn flip_ranges(&mut self) {
        for range in &mut self.ranges {
            range.flip();
        }
    }

    fn flip_ranges_forward(&mut self) {
        for range in &mut self.ranges {
            range.flip_forward();
        }
    }

    fn flip_ranges_backward(&mut self) {
        for range in &mut self.ranges {
            range.flip_backward();
        }
    }

    fn select_ranges(&mut self, regex: &Regex, rope: &Rope) -> anyhow::Result<()> {
        let mut ranges = Vec::new();

        for range in &self.ranges {
            let mut sub_ranges = range
                .select(rope, regex)
                .context("Failed to select in range")?;
            ranges.append(&mut sub_ranges);
        }

        if ranges.is_empty() {
            anyhow::bail!("No matches");
        }

        self.primary = ranges.len() - 1;
        self.ranges = ranges;

        Ok(())
    }

    fn keep_ranges(&mut self, regex: &Regex, rope: &Rope) -> anyhow::Result<()> {
        self.filter_ranges(|range| {
            let rope_slice = range
                .to_rope_slice(rope)
                .context("Failed to get rope slice")?;
            let string_slice = Cow::<str>::from(rope_slice);
            let keep = regex.is_match(&string_slice)?;
            Ok(keep)
        })
    }

    fn merge_ranges(&mut self) {
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

    fn insert_char(&mut self, c: char, rope: &mut Rope) -> anyhow::Result<()> {
        for range in &mut self.ranges {
            *range = range.insert_char(c, rope)?;
        }
        Ok(())
    }

    fn insert(&mut self, s: &str, rope: &mut Rope) -> anyhow::Result<()> {
        for range in &mut self.ranges {
            *range = range.insert(s, rope)?;
        }
        Ok(())
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
