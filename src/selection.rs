use crate::cursor::Cursor;
use crate::operand::Operand;
use std::fmt::Display;

#[derive(Default)]
pub(crate) struct Selection {
    pub(crate) anchor: Cursor,
    pub(crate) head: Cursor,
}

#[derive(Clone)]
pub(crate) enum Operation {
    Flip,
    FlipForwards,
    FlipBackwards,
    Reduce,
}

impl Operand for Selection {
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

impl Selection {
    pub(crate) fn new<P>(anchor: P, head: P) -> Self
    where
        P: Into<Cursor>,
    {
        Selection {
            anchor: anchor.into(),
            head: head.into(),
        }
    }

    pub(crate) fn is_forwards(&self) -> bool {
        self.anchor <= self.head
    }

    pub(crate) fn is_backwards(&self) -> bool {
        self.anchor > self.head
    }

    pub(crate) fn is_reduced(&self) -> bool {
        self.anchor == self.head
    }

    pub(crate) fn flip(&mut self) {
        std::mem::swap(&mut self.anchor, &mut self.head);
    }

    pub(crate) fn flip_forwards(&mut self) {
        if self.is_backwards() {
            self.flip();
        }
    }

    pub(crate) fn flip_backwards(&mut self) {
        if self.is_forwards() {
            self.flip();
        }
    }

    pub(crate) fn reduce(&mut self) {
        self.anchor = self.head.clone();
    }
}

impl Display for Selection {
    fn fmt(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(formatter, "{}-{}", self.anchor, self.head)
    }
}

impl From<(usize, usize)> for Selection {
    fn from(tuple: (usize, usize)) -> Self {
        Selection {
            anchor: Cursor::from(tuple),
            head: Cursor::from(tuple),
        }
    }
}

impl From<Cursor> for Selection {
    fn from(head: Cursor) -> Self {
        Selection {
            anchor: head.clone(),
            head,
        }
    }
}
