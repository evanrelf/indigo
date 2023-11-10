use crate::{conversion::Conversion, direction::Direction, range::Range};
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

    #[must_use]
    pub fn with_primary(&self, index: usize) -> Option<Self> {
        if index < self.ranges.len() {
            Some(Self {
                ranges: self.ranges.clone(),
                primary: index,
            })
        } else {
            None
        }
    }

    #[must_use]
    pub fn insert(&self, range: Range) -> Self {
        let index = match self.ranges.binary_search(&range) {
            Ok(index) | Err(index) => index, // TODO: Merge overlapping ranges?
        };

        let mut ranges = self.ranges.clone();
        ranges.insert(index, range);

        Self {
            ranges,
            primary: self.primary,
        }
    }

    #[must_use]
    pub fn remove(&self, index: usize) -> Option<Self> {
        if self.ranges.len() <= 1 {
            return None;
        }

        if index >= self.ranges.len() {
            return None;
        }

        let mut ranges = self.ranges.clone();
        ranges.remove(index);

        let primary = if self.primary < index {
            self.primary - 1
        } else {
            self.primary
        };

        Some(Self { ranges, primary })
    }

    #[must_use]
    pub fn flip(&self) -> Self {
        let mut selection = self.clone();
        for range in &mut selection.ranges {
            *range = range.flip();
        }
        selection
    }

    #[must_use]
    pub fn flip_forward(&self) -> Cow<Self> {
        if self.is_backward() {
            Cow::Owned(self.flip())
        } else {
            Cow::Borrowed(self)
        }
    }

    #[must_use]
    pub fn flip_backward(&self) -> Cow<Self> {
        if self.is_forward() {
            Cow::Owned(self.flip())
        } else {
            Cow::Borrowed(self)
        }
    }

    pub fn select(&self, rope: &Rope, regex: &Regex) -> anyhow::Result<Conversion<Self>> {
        let mut corrected = false;
        let mut ranges = Vec::new();

        for range in &self.ranges {
            let sub_ranges = range.select(rope, regex)?;
            corrected |= sub_ranges.is_corrected();
            let mut sub_ranges = sub_ranges.into_inner();
            ranges.append(&mut sub_ranges);
        }

        if ranges.is_empty() {
            anyhow::bail!("No matches");
        }

        let selection = Self {
            primary: ranges.len() - 1,
            ranges,
        };

        if corrected {
            Ok(Conversion::Corrected(selection))
        } else {
            Ok(Conversion::Valid(selection))
        }
    }

    pub fn assert_valid(&self) {
        assert!(!self.ranges.is_empty(), "`ranges` isn't empty");

        assert!(
            self.ranges.get(self.primary).is_some(),
            "`primary` index is valid"
        );

        // TODO: All ranges face the same direction

        // TODO: Ranges are sorted

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
