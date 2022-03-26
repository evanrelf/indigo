use crate::{
    operand::Operand,
    range::{self, Range},
};

pub struct Selection {
    pub ranges: Vec<Range>,
    pub primary_range_index: usize,
}

impl Selection {
    pub fn new(ranges: Vec<Range>) -> Self {
        Self {
            ranges,
            primary_range_index: 0,
        }
    }

    pub fn is_overlapping(&self, other: &Self) -> bool {
        for self_range in &self.ranges {
            for other_range in &other.ranges {
                if self_range.is_overlapping(other_range) {
                    return true;
                }
            }
        }

        false
    }

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

    #[allow(unused_variables)]
    pub fn exclude(self, other: Self) -> Self {
        todo!()
    }
}

pub enum Operation {
    NextRange(usize),
    PreviousRange(usize),
    FilterRanges(fn(usize, &Range) -> bool),
    InPrimaryRange(range::Operation),
    InAllRanges(range::Operation),
}

impl Operand for Selection {
    type Operation = Operation;

    fn apply(&mut self, operation: Self::Operation) {
        use Operation::*;

        match operation {
            NextRange(count) => {
                self.primary_range_index =
                    wrap_around(self.ranges.len(), self.primary_range_index, count as isize);
            }
            PreviousRange(count) => {
                self.primary_range_index = wrap_around(
                    self.ranges.len(),
                    self.primary_range_index,
                    -(count as isize),
                );
            }
            FilterRanges(filter) => {
                self.ranges = self
                    .ranges
                    .iter()
                    .enumerate()
                    .filter(|(i, r)| filter(*i, r))
                    .map(|(_, r)| r)
                    .cloned()
                    .collect();
            }
            InPrimaryRange(operation) => {
                self.ranges[self.primary_range_index].apply(operation);
            }
            InAllRanges(operation) => {
                for range in &mut self.ranges {
                    range.apply(operation.clone());
                }
            }
        }
    }
}

// Modified version of https://stackoverflow.com/a/39740009
fn wrap_around(length: usize, value: usize, delta: isize) -> usize {
    let length = length as isize;
    if length == 0 {
        0
    } else {
        let value = value as isize;
        let result = if delta >= 0 {
            (value + delta) % length
        } else {
            ((value + delta) - (delta * length)) % length
        };
        result as usize
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::cursor::Cursor;

    #[test]
    fn test_cursor_partialord() {
        assert!(Cursor::from((0, 0)) < Cursor::from((1, 0)));
        assert!(Cursor::from((0, 99)) < Cursor::from((1, 0)));
        assert!(Cursor::from((1, 0)) < Cursor::from((1, 1)));
    }

    #[test]
    fn test_range_is_overlapping() {
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
    fn test_wrap_around() {
        assert_eq!(wrap_around(0, 0, 0), 0);
        assert_eq!(wrap_around(0, 0, 4), 0);
        assert_eq!(wrap_around(2, 0, 0), 0);
        assert_eq!(wrap_around(2, 0, 3), 1);
        assert_eq!(wrap_around(2, 0, 4), 0);
        assert_eq!(wrap_around(2, 0, -3), 1);
        assert_eq!(wrap_around(2, 0, -4), 0);
    }
}
