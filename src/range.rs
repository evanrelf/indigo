use crate::{cursor::Cursor, operand::Operand};
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

#[derive(Clone)]
pub enum Operation {
    Flip,
    FlipForwards,
    FlipBackwards,
    Reduce,
}

impl Operand for Range {
    type Operation = Operation;

    fn apply(&mut self, operation: Self::Operation) {
        use Operation::*;

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
