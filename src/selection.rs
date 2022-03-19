use crate::cursor::Cursor;
use std::fmt::Display;

#[derive(Default)]
pub(crate) struct Selection {
    pub(crate) anchor: Cursor,
    pub(crate) cursor: Cursor,
}

#[derive(Clone)]
pub(crate) enum Operation {
    Flip,
    FlipForwards,
    FlipBackwards,
    Reduce,
}

impl Selection {
    pub(crate) fn new<P>(anchor: P, cursor: P) -> Self
    where
        P: Into<Cursor>,
    {
        Selection {
            anchor: anchor.into(),
            cursor: cursor.into(),
        }
    }

    pub(crate) fn apply_operation(&mut self, operation: Operation) -> &mut Self {
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

        self
    }

    pub(crate) fn is_forwards(&self) -> bool {
        self.anchor <= self.cursor
    }

    pub(crate) fn is_backwards(&self) -> bool {
        self.anchor > self.cursor
    }

    pub(crate) fn is_reduced(&self) -> bool {
        self.anchor == self.cursor
    }

    pub(crate) fn flip(&mut self) -> &mut Self {
        std::mem::swap(&mut self.anchor, &mut self.cursor);
        self
    }

    pub(crate) fn flip_forwards(&mut self) -> &mut Self {
        if self.is_backwards() {
            self.flip();
        }
        self
    }

    pub(crate) fn flip_backwards(&mut self) -> &mut Self {
        if self.is_forwards() {
            self.flip();
        }
        self
    }

    pub(crate) fn reduce(&mut self) -> &mut Self {
        self.anchor = self.cursor.clone();
        self
    }
}

impl Display for Selection {
    fn fmt(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(formatter, "{}-{}", self.anchor, self.cursor)
    }
}

impl From<(usize, usize)> for Selection {
    fn from(tuple: (usize, usize)) -> Self {
        Selection {
            anchor: Cursor::from(tuple),
            cursor: Cursor::from(tuple),
        }
    }
}

impl From<Cursor> for Selection {
    fn from(cursor: Cursor) -> Self {
        Selection {
            anchor: cursor.clone(),
            cursor,
        }
    }
}
