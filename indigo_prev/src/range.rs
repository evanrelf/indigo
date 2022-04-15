use crate::position::Position;
use ropey::{Rope, RopeSlice};
use std::fmt::Display;

#[derive(Clone, Default)]
pub struct Range {
    pub anchor: Position,
    pub head: Position,
    pub target_column: Option<usize>,
}

impl Range {
    #[must_use]
    pub fn new<P>(anchor: P, head: P) -> Self
    where
        P: Into<Position>,
    {
        Self {
            anchor: anchor.into(),
            head: head.into(),
            target_column: None,
        }
    }

    #[must_use]
    pub fn is_forwards(&self) -> bool {
        self.anchor <= self.head
    }

    #[must_use]
    pub fn is_backwards(&self) -> bool {
        self.anchor > self.head
    }

    #[must_use]
    pub fn is_reduced(&self) -> bool {
        self.anchor == self.head
    }

    #[must_use]
    pub fn is_overlapping(&self, other: &Self) -> bool {
        let (self_start, self_end) = if self.is_forwards() {
            (&self.anchor, &self.head)
        } else {
            (&self.head, &self.anchor)
        };

        let (other_start, other_end) = if other.is_forwards() {
            (&other.anchor, &other.head)
        } else {
            (&other.head, &other.anchor)
        };

        (self_start <= other_start && other_start <= self_end)
            || (other_start <= self_start && self_start <= other_end)
    }

    pub fn flip_mut(&mut self) {
        std::mem::swap(&mut self.anchor, &mut self.head);
        self.target_column = None;
    }

    #[must_use]
    pub fn flip(&self) -> Self {
        let mut new = self.clone();
        new.flip_mut();
        new
    }

    pub fn flip_forwards_mut(&mut self) {
        if self.is_backwards() {
            self.flip_mut();
        }
    }

    #[must_use]
    pub fn flip_forwards(&self) -> Self {
        let mut new = self.clone();
        new.flip_forwards_mut();
        new
    }

    pub fn flip_backwards_mut(&mut self) {
        if self.is_forwards() {
            self.flip_mut();
        }
    }

    #[must_use]
    pub fn flip_backwards(&self) -> Self {
        let mut new = self.clone();
        new.flip_backwards_mut();
        new
    }

    pub fn reduce_mut(&mut self) {
        self.anchor = self.head.clone();
    }

    #[must_use]
    pub fn reduce(&self) -> Self {
        let mut new = self.clone();
        new.reduce_mut();
        new
    }

    pub fn merge_mut(&mut self, other: Self) -> bool {
        if !self.is_overlapping(&other) {
            return false;
        }

        match (self.is_forwards(), other.is_forwards()) {
            (true, true) => {
                // Forwards
                if other.anchor < self.anchor {
                    self.anchor = other.anchor;
                }
                if other.head > self.head {
                    self.head = other.head;
                    self.target_column = other.target_column;
                }
            }
            (false, false) => {
                // Backwards
                if other.anchor > self.anchor {
                    self.anchor = other.anchor;
                }
                if other.head < self.head {
                    self.head = other.head;
                    self.target_column = other.target_column;
                }
            }
            _ => {
                // Mixed
                self.merge_mut(other.flip());
            }
        }

        true
    }

    #[must_use]
    pub fn merge(&self, other: &Self) -> Option<Self> {
        let mut new = self.clone();
        if new.merge_mut(other.clone()) {
            Some(new)
        } else {
            None
        }
    }

    #[must_use]
    pub fn to_rope_slice<'rope>(&self, rope: &'rope Rope) -> Option<RopeSlice<'rope>> {
        let anchor_index = self.anchor.to_rope_index(rope)?;
        let head_index = self.head.to_rope_index(rope)?;
        if self.is_forwards() {
            rope.get_slice(anchor_index..=head_index)
        } else {
            rope.get_slice(head_index..=anchor_index)
        }
    }

    #[must_use]
    pub fn to_rope_slice_corrected<'rope>(&self, rope: &'rope Rope) -> RopeSlice<'rope> {
        let anchor_index = self.anchor.to_rope_index_corrected(rope);
        let head_index = self.head.to_rope_index_corrected(rope);
        let rope_slice = if self.is_forwards() {
            rope.slice(anchor_index..=head_index)
        } else {
            rope.slice(head_index..=anchor_index)
        };
        rope_slice
    }

    #[must_use]
    pub fn corrected(&self, rope: &Rope) -> Self {
        let anchor = self.anchor.corrected(rope);
        let head = self.head.corrected(rope);
        Self {
            anchor,
            head,
            target_column: None,
        }
    }
}

impl Display for Range {
    fn fmt(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(formatter, "{}-{}", self.anchor, self.head)
    }
}

impl From<(usize, usize)> for Range {
    fn from(tuple: (usize, usize)) -> Self {
        Self {
            anchor: Position::from(tuple),
            head: Position::from(tuple),
            target_column: None,
        }
    }
}

impl From<Position> for Range {
    fn from(head: Position) -> Self {
        Self {
            anchor: head.clone(),
            head,
            target_column: None,
        }
    }
}

impl<T> From<(T, T)> for Range
where
    T: Into<Position>,
{
    fn from(tuple: (T, T)) -> Self {
        let (anchor, head) = tuple;

        Self {
            anchor: anchor.into(),
            head: head.into(),
            target_column: None,
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_is_overlapping() {
        {
            let x: Range = ((0, 0), (0, 0)).into();
            let y: Range = ((0, 0), (0, 0)).into();
            assert!(x.is_overlapping(&y))
        }
        {
            let x: Range = ((0, 0), (10, 10)).into();
            let y: Range = ((5, 5), (15, 15)).into();
            assert!(x.is_overlapping(&y))
        }
        {
            let x: Range = ((10, 10), (0, 0)).into();
            let y: Range = ((5, 5), (15, 15)).into();
            assert!(x.is_overlapping(&y))
        }
        {
            let x: Range = ((10, 10), (0, 0)).into();
            let y: Range = ((10, 10), (20, 20)).into();
            assert!(x.is_overlapping(&y))
        }
        {
            let x: Range = ((10, 10), (0, 0)).into();
            let y: Range = ((10, 11), (20, 20)).into();
            assert!(!x.is_overlapping(&y))
        }
    }

    #[test]
    fn test_to_rope_slice() {
        fn case(s: &str, range: ((usize, usize), (usize, usize)), expected: &str) {
            let rope = Rope::from_str(s);
            let range = Range::new(range.0, range.1);
            let expected = Some(expected);
            let actual = range.to_rope_slice(&rope).and_then(|slice| slice.as_str());
            assert!(
                expected == actual,
                "\nexpected = {:?}\nactual = {:?}\n",
                expected,
                actual
            );
        }

        case("Hello, world!", ((0, 0), (0, 4)), "Hello");
        case("Hello, world!", ((0, 7), (0, 11)), "world");
        case("Fizz\nBuzz", ((1, 0), (1, 3)), "Buzz");
        case("Fizz\nBuzz", ((0, 0), (1, 3)), "Fizz\nBuzz");
    }
}
