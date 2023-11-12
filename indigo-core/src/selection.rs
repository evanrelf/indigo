use crate::{direction::Direction, range::Range};
use fancy_regex::Regex;
use ropey::Rope;
use std::borrow::Cow;

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Selection {
    ranges: Vec<Range>,
    primary: usize,
}

impl Default for Selection {
    fn default() -> Self {
        Self {
            ranges: vec![Range::default()],
            primary: 0,
        }
    }
}

impl Selection {
    #[must_use]
    pub fn ranges(&self) -> &Vec<Range> {
        &self.ranges
    }

    #[must_use]
    pub fn primary(&self) -> Range {
        *self.ranges.get(self.primary).unwrap()
    }

    #[must_use]
    pub fn direction(&self) -> Direction {
        // All ranges face the same direction
        self.ranges.get(0).unwrap().direction()
    }

    #[must_use]
    pub fn is_forward(&self) -> bool {
        self.direction() == Direction::Forward
    }

    #[must_use]
    pub fn is_backward(&self) -> bool {
        self.direction() == Direction::Backward
    }

    pub fn set_primary(&mut self, index: usize) -> anyhow::Result<()> {
        if index >= self.ranges.len() {
            anyhow::bail!("Invalid range index {index}");
        }
        self.primary = index;
        Ok(())
    }

    pub fn with_primary(&self, index: usize) -> anyhow::Result<Self> {
        let mut x = self.clone();
        x.set_primary(index)?;
        Ok(x)
    }

    pub fn insert(&mut self, range: Range) {
        let index = match self.ranges.binary_search(&range) {
            Ok(index) | Err(index) => index, // TODO: Merge overlapping ranges?
        };
        self.ranges.insert(index, range);
    }

    #[must_use]
    pub fn inserted(&self, range: Range) -> Self {
        let mut x = self.clone();
        x.insert(range);
        x
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

    pub fn removed(&self, index: usize) -> anyhow::Result<Self> {
        let mut x = self.clone();
        x.remove(index)?;
        Ok(x)
    }

    pub fn filter<P>(&mut self, predicate: P) -> anyhow::Result<()>
    where
        P: Fn(&Range) -> anyhow::Result<bool>,
    {
        let x = self.filtered(predicate)?;
        *self = x;
        Ok(())
    }

    pub fn filtered<P>(&self, predicate: P) -> anyhow::Result<Self>
    where
        P: Fn(&Range) -> anyhow::Result<bool>,
    {
        let mut selection = self.clone();

        for (i, range) in self.ranges.iter().enumerate() {
            if !predicate(range)? {
                selection.remove(i)?;
            }
        }

        Ok(selection)
    }

    pub fn flip(&mut self) {
        for range in &mut self.ranges {
            range.flip();
        }
    }

    #[must_use]
    pub fn flipped(&self) -> Self {
        let mut x = self.clone();
        x.flip();
        x
    }

    pub fn flip_forward(&mut self) {
        if self.is_backward() {
            self.flip();
        }
    }

    #[must_use]
    pub fn flipped_forward(&self) -> Self {
        let mut x = self.clone();
        x.flip_forward();
        x
    }

    pub fn flip_backward(&mut self) {
        if self.is_forward() {
            self.flip();
        }
    }

    #[must_use]
    pub fn flipped_backward(&self) -> Self {
        let mut x = self.clone();
        x.flip_backward();
        x
    }

    pub fn select(&mut self, rope: &Rope, regex: &Regex) -> anyhow::Result<()> {
        let x = self.keeping(rope, regex)?;
        *self = x;
        Ok(())
    }

    pub fn selected(&self, rope: &Rope, regex: &Regex) -> anyhow::Result<Self> {
        let mut ranges = Vec::new();

        for range in &self.ranges {
            let mut sub_ranges = range.selected(rope, regex)?;
            ranges.append(&mut sub_ranges);
        }

        if ranges.is_empty() {
            anyhow::bail!("No matches");
        }

        Ok(Self {
            primary: ranges.len() - 1,
            ranges,
        })
    }

    pub fn keep(&mut self, rope: &Rope, regex: &Regex) -> anyhow::Result<()> {
        let x = self.keeping(rope, regex)?;
        *self = x;
        Ok(())
    }

    pub fn keeping(&self, rope: &Rope, regex: &Regex) -> anyhow::Result<Self> {
        self.filtered(|range| {
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

    #[must_use]
    pub fn merged(&self) -> Self {
        let mut x = self.clone();
        x.merge();
        x
    }

    pub fn assert_valid(&self) {
        assert!(!self.ranges.is_empty(), "`ranges` isn't empty");

        assert!(
            self.ranges.get(self.primary).is_some(),
            "`primary` index is valid"
        );

        let direction = self.primary().direction();
        assert!(
            self.ranges
                .iter()
                .all(|range| range.direction() == direction),
            "All ranges face the same direction"
        );

        // TODO: Use `is_sorted` once it stabilized
        // https://github.com/rust-lang/rust/issues/53485
        assert!(
            self.ranges.windows(2).all(|range| range[0] <= range[1]),
            "Ranges are sorted"
        );

        for range in &self.ranges {
            range.assert_valid();
        }
    }
}

impl From<Range> for Selection {
    fn from(range: Range) -> Self {
        Self {
            ranges: vec![range],
            primary: 0,
        }
    }
}
