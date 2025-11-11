#[derive(Debug)]
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
        self.index -= 1;
        self.items.get(self.index)
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test() {
        let mut history = History::new();
        history.push(1);
        assert_eq!(history.undo(), Some(&1));
        assert_eq!(history.undo(), None);
        assert_eq!(history.redo(), Some(&1));
        history.push(2);
        assert_eq!(history.undo(), Some(&2));
        history.push(3);
        assert_eq!(history.undo(), Some(&3));
        assert_eq!(history.undo(), Some(&1));
    }
}
