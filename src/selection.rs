use crate::position::Position;
use std::cmp::{max, min};
use std::fmt::Display;

#[derive(Clone, Copy, PartialEq)]
pub struct Selection {
    pub anchor: Position,
    pub cursor: Position,
}

impl Selection {
    pub fn new<P>(anchor: P, cursor: P) -> Selection
    where
        Position: From<P>,
    {
        Selection {
            anchor: Position::from(anchor),
            cursor: Position::from(cursor),
        }
    }

    pub fn is_forwards(&self) -> bool {
        self.anchor <= self.cursor
    }

    pub fn is_backwards(&self) -> bool {
        self.anchor > self.cursor
    }

    pub fn is_reduced(&self) -> bool {
        self.anchor == self.cursor
    }

    pub fn flip(&mut self) -> &Selection {
        let anchor = self.anchor;
        self.anchor = self.cursor;
        self.cursor = anchor;
        self
    }

    pub fn flip_forwards(&mut self) -> &Selection {
        if self.is_backwards() {
            self.flip();
        }
        self
    }

    pub fn flip_backwards(&mut self) -> &Selection {
        if self.is_forwards() {
            self.flip();
        }
        self
    }
}

impl Display for Selection {
    fn fmt(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(formatter, "{}-{}", self.anchor, self.cursor)
    }
}

impl Default for Selection {
    fn default() -> Selection {
        Selection {
            anchor: Position::default(),
            cursor: Position::default(),
        }
    }
}

impl From<(usize, usize)> for Selection {
    fn from(tuple: (usize, usize)) -> Selection {
        Selection {
            anchor: Position::from(tuple),
            cursor: Position::from(tuple),
        }
    }
}

impl From<Position> for Selection {
    fn from(position: Position) -> Selection {
        Selection {
            anchor: position.clone(),
            cursor: position.clone(),
        }
    }
}

}