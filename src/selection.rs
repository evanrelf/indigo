use crate::{direction::Direction, range::Range, selection_range::SelectionRange};
use std::collections::BTreeMap;

#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct Selection {
    // TODO: ranges: Vec<SelectionRange>, primary: usize
    primary: (SelectionRange, Option<usize>),
    secondaries: BTreeMap<SelectionRange, Vec<Option<usize>>>,
    direction: Direction,
}

impl Selection {
    pub fn ranges(&self) -> ! {
        todo!()
    }

    pub fn primary(&self) -> &(SelectionRange, Option<usize>) {
        &self.primary
    }

    pub fn secondaries(&self) -> ! {
        todo!()
    }

    pub fn direction(&self) -> &Direction {
        &self.direction
    }

    pub fn is_valid(&self) -> bool {
        todo!()
    }
}

impl From<Range> for Selection {
    fn from(range: Range) -> Self {
        Self {
            primary: (SelectionRange::from(range.clone()), range.target_column()),
            secondaries: BTreeMap::new(),
            direction: range.direction(),
        }
    }
}
