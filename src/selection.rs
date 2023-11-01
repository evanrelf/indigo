use crate::{direction::Direction, range::Range, selection_range::SelectionRange};

#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct Selection {
    ranges: Vec<(SelectionRange, Option<usize>)>,
    direction: Direction,
}

impl Selection {
    pub fn ranges(&self) -> &Vec<(SelectionRange, Option<usize>)> {
        &self.ranges
    }

    pub fn direction(&self) -> &Direction {
        &self.direction
    }
}

impl From<Range> for Selection {
    fn from(range: Range) -> Self {
        Self {
            ranges: vec![(SelectionRange::from(range.clone()), range.target_column())],
            direction: range.direction(),
        }
    }
}
