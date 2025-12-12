use roaring::RoaringBitmap;
use std::{
    borrow::Borrow,
    collections::BTreeMap,
    iter,
    ops::{RangeBounds, RangeInclusive},
};

pub struct Attributes<A>(BTreeMap<A, RoaringBitmap>);

impl<A> Attributes<A> {
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    pub fn insert(&mut self, range: impl RangeBounds<u32>, attribute: A)
    where
        A: Ord,
    {
        self.0.entry(attribute).or_default().insert_range(range);
    }

    pub fn remove<Q>(&mut self, range: impl RangeBounds<u32>, attribute: &Q)
    where
        A: Borrow<Q> + Ord,
        Q: Ord + ?Sized,
    {
        let attribute = attribute.borrow();
        if let Some(ranges) = self.0.get_mut(attribute) {
            ranges.remove_range(range);
            if ranges.is_empty() {
                self.0.remove(attribute);
            }
        }
    }

    #[must_use]
    pub fn contains<Q>(&self, range: impl RangeBounds<u32>, attribute: &Q) -> bool
    where
        A: Borrow<Q> + Ord,
        Q: Ord + ?Sized,
    {
        let attribute = attribute.borrow();
        match self.0.get(attribute) {
            Some(ranges) => ranges.contains_range(range),
            None => false,
        }
    }

    #[must_use]
    pub fn ranges<Q>(&self, attribute: &Q) -> Option<impl Iterator<Item = RangeInclusive<u32>>>
    where
        A: Borrow<Q> + Ord,
        Q: Ord + ?Sized,
    {
        self.0.get(attribute.borrow()).map(|ranges| AttrRanges {
            iter: ranges.iter(),
        })
    }

    pub fn attrs(&self, range: impl RangeBounds<u32> + Clone) -> impl Iterator<Item = &A> {
        iter::zip(self.0.iter(), iter::repeat(range))
            .filter_map(|((attr, ranges), range)| ranges.contains_range(range).then_some(attr))
    }
}

impl<A> Default for Attributes<A> {
    fn default() -> Self {
        Self(BTreeMap::default())
    }
}

pub struct AttrRanges<'a> {
    iter: roaring::bitmap::Iter<'a>,
}

impl Iterator for AttrRanges<'_> {
    type Item = RangeInclusive<u32>;
    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next_range()
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
        let expected_ranges = vec![0..=3, 7..=8];
        let actual_ranges = attrs.ranges("foo").unwrap().collect::<Vec<_>>();
        assert_eq!(expected_ranges, actual_ranges);
        let expected_attrs: Vec<&str> = vec!["foo"];
        let actual_attrs: Vec<&str> = attrs.attrs(0..=3).copied().collect::<Vec<_>>();
        assert_eq!(expected_attrs, actual_attrs);
    }
}
