use crate::cursor::Cursor;
use std::fmt::Display;

#[derive(Default)]
pub(crate) struct Selection {
    pub(crate) anchor: Cursor,
    pub(crate) cursor: Cursor,
}

impl Selection {
    pub(crate) fn new<P>(anchor: P, cursor: P) -> Selection
    where
        P: Into<Cursor>,
    {
        Selection {
            anchor: anchor.into(),
            cursor: cursor.into(),
        }
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

    pub(crate) fn flip(&mut self) -> &mut Selection {
        std::mem::swap(&mut self.anchor, &mut self.cursor);
        self
    }

    pub(crate) fn flip_forwards(&mut self) -> &mut Selection {
        if self.is_backwards() {
            self.flip();
        }
        self
    }

    pub(crate) fn flip_backwards(&mut self) -> &mut Selection {
        if self.is_forwards() {
            self.flip();
        }
        self
    }

    pub(crate) fn reduce(&mut self) -> &mut Selection {
        self.anchor = self.cursor;
        self
    }
}

impl Display for Selection {
    fn fmt(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(formatter, "{}-{}", self.anchor, self.cursor)
    }
}

impl From<(usize, usize)> for Selection {
    fn from(tuple: (usize, usize)) -> Selection {
        Selection {
            anchor: Cursor::from(tuple),
            cursor: Cursor::from(tuple),
        }
    }
}

impl From<Cursor> for Selection {
    fn from(cursor: Cursor) -> Selection {
        Selection {
            anchor: cursor,
            cursor,
        }
    }
}
