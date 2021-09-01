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

    pub fn reduce(&mut self) -> &Selection {
        self.anchor = self.cursor;
        self
    }

    /// Combine two selections into one
    ///
    /// self:   ##############
    /// other:        ##############
    /// result: ####################
    pub fn union(&mut self, other: &Selection) -> &Selection {
        let was_backwards = self.is_backwards();
        self.flip_forwards();
        let mut other = other.clone();
        let other = other.flip_forwards();

        self.anchor = min(self.anchor, other.anchor);
        self.cursor = max(self.cursor, other.cursor);

        if was_backwards {
            self.flip_backwards();
        }

        self
    }

    /// Subtract the second selection from the first
    ///
    /// self:   ##############
    /// other:        ##############
    /// result: ######
    pub fn subtract(&mut self, other: &Selection) -> &Selection {
        let was_backwards = self.is_backwards();
        self.flip_forwards();
        let mut other = other.clone();
        let other = other.flip_forwards();

        self.anchor = todo!();
        self.cursor = todo!();

        if was_backwards {
            self.flip_backwards();
        }

        self
    }

    /// Find the intersection between two selections
    ///
    /// self:   ##############
    /// other:        ##############
    /// result:       ########
    pub fn intersect(&mut self, other: &Selection) -> &Selection {
        let was_backwards = self.is_backwards();
        self.flip_forwards();
        let mut other = other.clone();
        let other = other.flip_forwards();

        self.anchor = max(self.anchor, other.anchor);
        self.cursor = min(self.cursor, other.cursor);

        if was_backwards {
            self.flip_backwards();
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

#[test]
fn test_union() {
    let mut x = Selection::new((0, 0), (0, 8));
    let y = Selection::new((0, 4), (0, 12));

    let expected = Selection::new((0, 0), (0, 12));
    let actual = *x.union(&y);

    assert!(
        expected == actual,
        "\nexpected = {}\nactual = {}\n",
        expected,
        actual
    );
}

#[test]
fn test_subtract() {
    let mut x = Selection::new((0, 0), (0, 8));
    let y = Selection::new((0, 0), (0, 4));

    let expected = Selection::new((0, 5), (0, 8));
    let actual = *x.subtract(&y);

    assert!(
        expected == actual,
        "\nexpected = {}\nactual = {}\n",
        expected,
        actual
    );
}

#[test]
fn test_intersect() {
    let mut x = Selection::new((0, 0), (0, 8));
    let y = Selection::new((0, 4), (0, 12));

    let expected = Selection::new((0, 4), (0, 8));
    let actual = *x.intersect(&y);

    assert!(
        expected == actual,
        "\nexpected = {}\nactual = {}\n",
        expected,
        actual
    );
}
