use crate::ot::OperationSeq;
use roaring::RoaringBitmap;
use std::{
    borrow::Borrow,
    collections::BTreeMap,
    iter,
    ops::{RangeBounds, RangeInclusive},
};

pub struct Attributes<A> {
    pub inner: BTreeMap<A, RoaringBitmap>,
}

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
        self.inner.entry(attribute).or_default().insert_range(range);
    }

    pub fn remove<R, Q>(&mut self, range: R, attribute: &Q)
    where
        R: RangeBounds<u32>,
        A: Borrow<Q> + Ord,
        Q: Ord + ?Sized,
    {
        let attribute = attribute.borrow();
        if let Some(ranges) = self.inner.get_mut(attribute) {
            ranges.remove_range(range);
            if ranges.is_empty() {
                self.inner.remove(attribute);
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
        match self.inner.get(attribute.borrow()) {
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
        self.inner.get(attribute.borrow()).map(|ranges| Ranges {
            iter: ranges.iter(),
        })
    }

    pub fn attrs<R>(&self, range: R) -> impl Iterator<Item = &A>
    where
        R: RangeBounds<u32> + Clone,
    {
        iter::zip(self.inner.iter(), iter::repeat(range))
            .filter_map(|((attr, ranges), range)| ranges.contains_range(range).then_some(attr))
    }

    pub fn transform(&mut self, ops: &OperationSeq) {
        for old_ranges in self.inner.values_mut() {
            let mut new_ranges = RoaringBitmap::new();
            let mut iter = old_ranges.iter();
            while let Some(range) = iter.next_range() {
                let start = u32::try_from(ops.transform_byte_offset(
                    usize::try_from(*range.start()).expect("Machine has 64-bit pointers"),
                ))
                .expect("Byte offset does not exceed u32::MAX");
                let end = u32::try_from(ops.transform_byte_offset(
                    usize::try_from(*range.end()).expect("Machine has 64-bit pointers"),
                ))
                .expect("Byte offset does not exceed u32::MAX");
                new_ranges.insert_range(start..=end);
            }
            *old_ranges = new_ranges;
        }
    }
}

impl<A> Default for Attributes<A> {
    fn default() -> Self {
        Self {
            inner: BTreeMap::default(),
        }
    }
}

/// An iterator over ranges with a certain attribute.
///
/// This struct is created with the [`ranges`](Attributes::ranges) method on [`Attributes`].
pub struct Ranges<'a> {
    iter: roaring::bitmap::Iter<'a>,
}

impl Iterator for Ranges<'_> {
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

    #[test]
    fn inner() {
        let mut attrs = Attributes::new();
        attrs.insert(0..=10, String::from("color=red"));
        attrs.insert(5..=15, String::from("color=green"));
        attrs.insert(10..=20, String::from("color=blue"));
        attrs.insert(0..=5, String::from("font=bold"));
        attrs.insert(15..=25, String::from("font=italic"));

        // Get all colors
        let color_attrs: Vec<_> = attrs
            .inner
            .range(String::from("color=")..String::from("color=\u{10ffff}"))
            .map(|(attr, _)| attr.as_str())
            .collect();
        assert_eq!(color_attrs, vec!["color=blue", "color=green", "color=red"]);

        // Get specific colors
        let specific_colors: Vec<_> = attrs
            .inner
            .range(String::from("color=blue")..=String::from("color=green"))
            .map(|(attr, _)| attr.as_str())
            .collect();
        assert_eq!(specific_colors, vec!["color=blue", "color=green"]);

        // Get all fonts
        let font_attrs: Vec<_> = attrs
            .inner
            .range(String::from("font=")..String::from("font=\u{10ffff}"))
            .map(|(attr, _)| attr.as_str())
            .collect();
        assert_eq!(font_attrs, vec!["font=bold", "font=italic"]);

        // Remove colors
        attrs.inner.retain(|attr, _| !attr.starts_with("color="));

        // Only fonts remain
        let remaining_attrs: Vec<_> = attrs.inner.keys().collect();
        assert_eq!(remaining_attrs, vec!["font=bold", "font=italic"]);
    }
}
