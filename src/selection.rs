use crate::{direction::Direction, range::Range};

#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct Selection {
    ranges: Vec<Range>,
    primary: usize,
}

impl Selection {
    pub fn ranges(&self) -> &Vec<Range> {
        &self.ranges
    }

    pub fn primary(&self) -> &Range {
        self.ranges.get(self.primary).unwrap()
    }

    pub fn direction(&self) -> Direction {
        // All ranges face the same direction
        self.ranges.get(0).unwrap().direction()
    }

    pub fn is_forward(&self) -> bool {
        self.direction() == Direction::Forward
    }

    pub fn is_backward(&self) -> bool {
        self.direction() == Direction::Backward
    }

    pub fn flip(&self) -> Self {
        todo!()
    }

    pub fn flip_forward(&self) -> Self {
        if self.is_backward() {
            self.flip()
        } else {
            self.clone()
        }
    }

    pub fn flip_backward(&self) -> Self {
        if self.is_forward() {
            self.flip()
        } else {
            self.clone()
        }
    }

    pub fn is_valid(&self) -> bool {
        // TODO: `ranges` isn't empty
        // TODO: `primary` index is valid
        // TODO: All ranges face the same direction
        // TODO: Ranges are sorted
        todo!()
    }
}

impl From<Range> for Selection {
    fn from(range: Range) -> Self {
        Self {
            ranges: vec![range],
            primary: 0,
        }
    }
}
