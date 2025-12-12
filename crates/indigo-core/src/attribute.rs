use roaring::RoaringBitmap;
use std::{
    borrow::Borrow,
    collections::BTreeMap,
    iter,
    ops::{RangeBounds, RangeInclusive},
};

pub struct Attributes<A>(BTreeMap<A, RoaringBitmap>);

// TODO: Add range queries with `BTreeMap::range`. For example, if `A = &str`, and I have three
// attributes `["color=red", "color=green", "color=blue"]`, then a range query could let me query
// all the colors without caring about their values.

impl<A> Attributes<A> {
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    pub fn insert<R>(&mut self, range: R, attribute: A)
    where
        R: RangeBounds<u32>,
        A: Ord,
    {
        self.0.entry(attribute).or_default().insert_range(range);
    }

    pub fn remove<R, Q>(&mut self, range: R, attribute: &Q)
    where
        R: RangeBounds<u32>,
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
    pub fn contains<R, Q>(&self, range: R, attribute: &Q) -> bool
    where
        R: RangeBounds<u32>,
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

    pub fn attrs<R>(&self, range: R) -> impl Iterator<Item = &A>
    where
        R: RangeBounds<u32> + Clone,
    {
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
        attrs.insert(0..=4, String::from("foo"));
        attrs.insert(2..=8, String::from("foo"));
        assert!(attrs.contains(0..=8, "foo"));
        attrs.remove(4..=6, "foo");
        assert!(attrs.contains(0..=3, "foo"));
        assert!(!attrs.contains(4..=6, "foo"));
        assert!(attrs.contains(7..=8, "foo"));
        let expected_ranges = vec![0..=3, 7..=8];
        let actual_ranges: Vec<_> = attrs.ranges("foo").unwrap().collect();
        assert_eq!(expected_ranges, actual_ranges);
        let expected_attrs = vec!["foo"];
        let actual_attrs: Vec<_> = attrs.attrs(0..=3).cloned().collect();
        assert_eq!(expected_attrs, actual_attrs);
    }
}
