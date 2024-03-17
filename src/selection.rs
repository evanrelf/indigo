use crate::Position;
use std::cmp::{max, min};

#[derive(Clone, Copy, Debug, Default, PartialEq)]
pub struct Selection {
    pub anchor: Position,
    pub cursor: Position,
    pub target_column: Option<usize>,
}

impl Selection {
    #[must_use]
    pub fn start(&self) -> Position {
        min(self.anchor, self.cursor)
    }

    #[must_use]
    pub fn end(&self) -> Position {
        max(self.anchor, self.cursor)
    }

    #[must_use]
    pub fn is_forward(&self) -> bool {
        self.anchor <= self.cursor
    }

    #[must_use]
    pub fn is_backward(&self) -> bool {
        self.anchor > self.cursor
    }

    #[must_use]
    pub fn is_reduced(&self) -> bool {
        self.anchor == self.cursor
    }

    pub fn flip(&mut self) {
        std::mem::swap(&mut self.anchor, &mut self.cursor);
    }

    pub fn flip_forward(&mut self) {
        if self.is_backward() {
            self.flip();
        }
    }

    pub fn flip_backward(&mut self) {
        if self.is_forward() {
            self.flip();
        }
    }

    pub fn reduce(&mut self) {
        self.anchor = self.cursor;
    }
}
