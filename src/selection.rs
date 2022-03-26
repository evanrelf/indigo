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
        write!(formatter, "{}:{}", self.line, self.column)
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

#[derive(Default)]
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

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_cursor_partialord() {
        assert!(Cursor::from((0, 0)) < Cursor::from((1, 0)));
        assert!(Cursor::from((0, 99)) < Cursor::from((1, 0)));
        assert!(Cursor::from((1, 0)) < Cursor::from((1, 1)));
    }
}
