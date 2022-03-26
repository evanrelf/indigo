use crate::operand::Operand;
use std::fmt::Display;

#[derive(Clone, Debug, Default, PartialEq, PartialOrd)]
pub struct Cursor {
    pub line: usize,
    pub column: usize,
    pub target_column: Option<usize>,
}

impl Cursor {
    pub fn new(line: usize, column: usize) -> Self {
        Self {
            line,
            column,
            target_column: None,
        }
    }
}

impl Display for Cursor {
    fn fmt(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(formatter, "{}:{}", self.line + 1, self.column + 1)
    }
}

impl From<(usize, usize)> for Cursor {
    fn from((line, column): (usize, usize)) -> Self {
        Self {
            line,
            column,
            target_column: None,
        }
    }
}

#[derive(Clone, Default)]
pub struct Range {
    pub anchor: Cursor,
    pub head: Cursor,
}

#[derive(Clone)]
pub enum RangeOperation {
    Flip,
    FlipForwards,
    FlipBackwards,
    Reduce,
}

impl Operand for Range {
    type Operation = RangeOperation;

    fn apply(&mut self, operation: Self::Operation) {
        use RangeOperation::*;

        match operation {
            Flip => {
                self.flip();
            }
            FlipForwards => {
                self.flip_forwards();
            }
            FlipBackwards => {
                self.flip_backwards();
            }
            Reduce => {
                self.reduce();
            }
        }
    }
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

impl From<Cursor> for Range {
    fn from(head: Cursor) -> Self {
        Self {
            anchor: head.clone(),
            head,
        }
    }
}

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
}

pub enum Operation {
    NextRange(usize),
    PreviousRange(usize),
    FilterRanges(fn(usize, &Range) -> bool),
    InPrimaryRange(RangeOperation),
    InAllRanges(RangeOperation),
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

    #[test]
    fn test_cursor_partialord() {
        assert!(Cursor::from((0, 0)) < Cursor::from((1, 0)));
        assert!(Cursor::from((0, 99)) < Cursor::from((1, 0)));
        assert!(Cursor::from((1, 0)) < Cursor::from((1, 1)));
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
