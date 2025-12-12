use range_set_blaze::RangeSetBlaze;
use std::{collections::BTreeMap, mem, ops::RangeInclusive};

#[derive(Default)]
pub struct Attributes(BTreeMap<&'static str, RangeSetBlaze<u32>>);

impl Attributes {
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    pub fn insert(&mut self, range: RangeInclusive<u32>, attribute: &'static str) {
        self.0.entry(attribute).or_default().ranges_insert(range);
    }

    pub fn remove(&mut self, range: RangeInclusive<u32>, attribute: &'static str) {
        if let Some(r) = self.0.get_mut(attribute) {
            *r = mem::take(r) - RangeSetBlaze::from(range);
            if r.is_empty() {
                self.0.remove(attribute);
            }
        }
    }

    #[must_use]
    pub fn contains(&self, range: RangeInclusive<u32>, attribute: &'static str) -> bool {
        match self.0.get(attribute) {
            Some(r) => r.is_superset(&RangeSetBlaze::from(range)),
            None => false,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn demo() {
        let mut attrs = Attributes::new();
        attrs.insert(0..=4, "foo");
        attrs.insert(2..=8, "foo");
        assert!(attrs.contains(0..=8, "foo"));
        attrs.remove(4..=6, "foo");
        assert!(attrs.contains(0..=3, "foo"));
        assert!(!attrs.contains(4..=6, "foo"));
        assert!(attrs.contains(7..=8, "foo"));
    }
}
