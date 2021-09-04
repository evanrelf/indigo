use crate::selection::Selection;
use ropey::Rope;
use std::fmt::Display;
use std::ops::{Deref, DerefMut};
use std::sync::{Arc, Mutex};

pub struct BufferSelection {
    pub rope: Arc<Mutex<Rope>>,
    pub selection: Selection,
}

impl BufferSelection {
    pub fn new(rope: Arc<Mutex<Rope>>) -> BufferSelection {
        BufferSelection {
            rope,
            selection: Selection::default(),
        }
    }
}

impl Display for BufferSelection {
    fn fmt(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(formatter, "{}", self.selection)
    }
}

impl Deref for BufferSelection {
    type Target = Selection;

    fn deref(&self) -> &Selection {
        &self.selection
    }
}

impl DerefMut for BufferSelection {
    fn deref_mut(&mut self) -> &mut Selection {
        &mut self.selection
    }
}
