use crate::position::Position;
use ropey::{Rope, RopeSlice};
use std::fmt::Display;

#[derive(Default)]
pub struct Selection {
    pub anchor: Position,
    pub cursor: Position,
}

impl Selection {
    pub fn new<P>(anchor: P, cursor: P) -> Selection
    where
        P: Into<Position>,
    {
        Selection {
            anchor: anchor.into(),
            cursor: cursor.into(),
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

    pub fn flip(&mut self) -> &mut Selection {
        std::mem::swap(&mut self.anchor, &mut self.cursor);
        self
    }

    pub fn flip_forwards(&mut self) -> &mut Selection {
        if self.is_backwards() {
            self.flip();
        }
        self
    }

    pub fn flip_backwards(&mut self) -> &mut Selection {
        if self.is_forwards() {
            self.flip();
        }
        self
    }

    pub fn reduce(&mut self) -> &mut Selection {
        self.anchor = self.cursor;
        self
    }
}

// With rope
impl Selection {
    pub fn to_slice<'rope>(&self, rope: &'rope Rope) -> Option<RopeSlice<'rope>> {
        let anchor_index = self.anchor.to_index(rope)?;
        let cursor_index = self.cursor.to_index(rope)?;
        if self.is_forwards() {
            rope.get_slice(anchor_index..=cursor_index)
        } else {
            rope.get_slice(cursor_index..=anchor_index)
        }
    }

    pub fn is_valid(&self, rope: &Rope) -> bool {
        self.anchor.is_valid(rope) && self.cursor.is_valid(rope)
    }

    pub fn corrected(&self, rope: &Rope) -> Option<Selection> {
        let anchor = self.anchor.corrected(rope)?;
        let cursor = self.cursor.corrected(rope)?;
        Some(Selection { anchor, cursor })
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

#[test]
fn test_to_slice() {
    fn case(s: &str, selection: ((usize, usize), (usize, usize)), expected: &str) {
        let rope = Rope::from_str(s);
        let selection = Selection::new(selection.0, selection.1);
        let expected = Some(expected);
        let actual = selection.to_slice(&rope).and_then(|slice| slice.as_str());
        assert!(
            expected == actual,
            "\nexpected = {:?}\nactual = {:?}\n",
            expected,
            actual
        );
    }

    case("Hello, world!", ((0, 0), (0, 4)), "Hello");
    case("Hello, world!", ((0, 7), (0, 11)), "world");
    case("Fizz\nBuzz", ((1, 0), (1, 3)), "Buzz");
    case("Fizz\nBuzz", ((0, 0), (1, 3)), "Fizz\nBuzz");
}
