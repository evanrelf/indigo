use crate::{conversion::Conversion, direction::Direction, range::Range};
use fancy_regex::Regex;
use ropey::Rope;

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
