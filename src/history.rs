#[derive(Debug)]
pub struct History<T> {
    before: Vec<T>,
    after: Vec<T>,
}

impl<T> History<T> {
    pub fn push(&mut self, state: T)
    where
        T: PartialEq,
    {
        if let Some(before) = self.before.last() {
            if *before == state {
                return;
            }
        }

        self.before.push(state);
        self.after.clear();
    }

    #[must_use]
    pub fn undo(&mut self) -> Option<&T> {
        let state = self.before.pop()?;
        self.after.push(state);
        Some(self.after.last().unwrap())
    }

    #[must_use]
    pub fn redo(&mut self) -> Option<&T> {
        let state = self.after.pop()?;
        self.before.push(state);
        Some(self.before.last().unwrap())
    }
}

impl<T> Default for History<T> {
    fn default() -> Self {
        Self {
            before: Vec::new(),
            after: Vec::new(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test() {
        let mut history = History::default();
        let mut value: u8 = 0;

        history.push(value);
        value = 1;

        history.push(value);
        value = 2;

        history.push(value);
        value = 3;

        assert_eq!(history.undo(), Some(2u8).as_ref());
        assert_eq!(history.undo(), Some(1u8).as_ref());
        assert_eq!(history.undo(), Some(0u8).as_ref());
        assert_eq!(history.undo(), None);

        assert_eq!(history.redo(), Some(0u8).as_ref());

        history.push(42u8);

        assert_eq!(history.redo(), None);

        let _ = value;
    }
}
