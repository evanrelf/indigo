use crate::cursor::{self, Cursor};
use ropey::{Rope, RopeSlice};
use std::fmt::Display;

#[derive(Clone, Default)]
pub struct Range {
    pub anchor: Cursor,
    pub head: Cursor,
}

impl Range {
    pub fn new<P>(anchor: P, head: P) -> Self
    where
        P: Into<Cursor>,
    {
        Self {
            anchor: anchor.into(),
            head: head.into(),
        }
    }

    pub fn is_forwards(&self) -> bool {
        self.anchor <= self.head
    }

    pub fn is_backwards(&self) -> bool {
        self.anchor > self.head
    }

    pub fn is_reduced(&self) -> bool {
        self.anchor == self.head
    }

    pub fn flip(&mut self) {
        std::mem::swap(&mut self.anchor, &mut self.head);
    }

    pub fn flip_forwards(&mut self) {
        if self.is_backwards() {
            self.flip();
        }
    }

    pub fn flip_backwards(&mut self) {
        if self.is_forwards() {
            self.flip();
        }
    }

    pub fn reduce(&mut self) {
        self.anchor = self.head.clone();
    }

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

    // https://help.figma.com/hc/en-us/articles/360039957534-Boolean-Operations

    #[allow(unused_variables)]
    pub fn union(self, other: Self) -> Self {
        todo!()
    }

    #[allow(unused_variables)]
    pub fn subtract(self, other: Self) -> Self {
        todo!()
    }

    #[allow(unused_variables)]
    pub fn intersect(self, other: Self) -> Self {
        todo!()
    }

    pub fn to_rope_slice<'rope>(&self, rope: &'rope Rope) -> Option<RopeSlice<'rope>> {
        let anchor_index = self.anchor.to_rope_index(rope)?;
        let head_index = self.head.to_rope_index(rope)?;
        if self.is_forwards() {
            rope.get_slice(anchor_index..=head_index)
        } else {
            rope.get_slice(head_index..=anchor_index)
        }
    }

    pub fn to_rope_slice_lossy<'rope>(&self, rope: &'rope Rope) -> RopeSlice<'rope> {
        let anchor_index = self.anchor.to_rope_index_lossy(rope);
        let head_index = self.head.to_rope_index_lossy(rope);
        if self.is_forwards() {
            rope.slice(anchor_index..=head_index)
        } else {
            rope.slice(head_index..=anchor_index)
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
            anchor: Cursor::from(tuple),
            head: Cursor::from(tuple),
        }
    }
}

impl<T> From<(T, T)> for Range
where
    T: Into<Cursor>,
{
    fn from(tuple: (T, T)) -> Self {
        let (anchor, head) = tuple;

        Self {
            anchor: anchor.into(),
            head: head.into(),
        }
    }
}

impl From<Cursor> for Range {
    fn from(head: Cursor) -> Self {
        Self {
            anchor: head.clone(),
            head,
        }
    }
}

// TODO: Delete
pub fn corrected_range(rope: &Rope, range: &Range) -> Option<Range> {
    let anchor = cursor::corrected_cursor(rope, &range.anchor)?;
    let head = cursor::corrected_cursor(rope, &range.head)?;
    Some(Range { anchor, head })
}

#[cfg(test)]
mod test {
    use super::*;

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
