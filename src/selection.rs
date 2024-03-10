use crate::position::Position;
use std::cmp::{max, min};

#[derive(Debug, Default)]
pub struct Selection {
    pub anchor: Position,
    pub cursor: Position,
    pub target_column: Option<usize>,
}

impl Selection {
    pub fn start(&self) -> Position {
        min(self.anchor, self.cursor)
    }

    pub fn end(&self) -> Position {
        max(self.anchor, self.cursor)
    }

    pub fn is_forward(&self) -> bool {
        self.anchor <= self.cursor
    }

    pub fn is_backward(&self) -> bool {
        self.anchor > self.cursor
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
