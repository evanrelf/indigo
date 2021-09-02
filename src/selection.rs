use crate::position::Position;
use std::fmt::Display;

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

    pub fn reduce(&mut self) -> &Selection {
        self.anchor = self.cursor;
        self
    }

    pub fn move_up(&mut self, distance: usize) -> &Selection {
        self.cursor.move_up(distance);
        self
    }

    pub fn move_down(&mut self, distance: usize) -> &Selection {
        self.cursor.move_down(distance);
        self
    }

    pub fn move_left(&mut self, distance: usize) -> &Selection {
        self.cursor.move_left(distance);
        self
    }

    pub fn move_right(&mut self, distance: usize) -> &Selection {
        self.cursor.move_right(distance);
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
            anchor: position,
            cursor: position,
        }
    }
}
