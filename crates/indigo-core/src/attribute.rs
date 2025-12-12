use roaring::RoaringBitmap;
use std::{collections::BTreeMap, ops::RangeBounds};

#[derive(Default)]
pub struct Attributes(BTreeMap<&'static str, RoaringBitmap>);

impl Attributes {
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    pub fn insert(&mut self, range: impl RangeBounds<u32>, attribute: &'static str) {
        self.0.entry(attribute).or_default().insert_range(range);
    }

    pub fn remove(&mut self, range: impl RangeBounds<u32>, attribute: &'static str) {
        if let Some(r) = self.0.get_mut(attribute) {
            r.remove_range(range);
            if r.is_empty() {
                self.0.remove(attribute);
            }
        }
    }

    #[must_use]
    pub fn contains(&self, range: impl RangeBounds<u32>, attribute: &'static str) -> bool {
        match self.0.get(attribute) {
            Some(r) => r.contains_range(range),
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
        attrs.insert(2..8, "foo");
        assert!(attrs.contains(0..8, "foo"));
        attrs.remove(4..6, "foo");
        assert!(attrs.contains(0..4, "foo"));
        assert!(!attrs.contains(4..6, "foo"));
        assert!(attrs.contains(6..8, "foo"));
    }
}
