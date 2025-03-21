pub struct History<T> {
    items: Vec<T>,
    index: usize,
}

impl<T> History<T> {
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    pub fn push(&mut self, edit: T) {
        self.items.truncate(self.index);
        self.items.push(edit);
        self.index += 1;
    }

    pub fn undo(&mut self) -> Option<&T> {
        if self.index == 0 {
            return None;
        }
        let edit = &self.items[self.index];
        self.index -= 1;
        Some(edit)
    }

    pub fn redo(&mut self) -> Option<&T> {
        let edit = self.items.get(self.index)?;
        self.index += 1;
        Some(edit)
    }
}

impl<T> Default for History<T> {
    fn default() -> Self {
        Self {
            items: Vec::new(),
            index: 0,
        }
    }
}
