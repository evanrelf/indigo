#[derive(Debug)]
pub struct History<T> {
    before: Vec<Vec<T>>,
    after: Vec<Vec<T>>,
}

impl<T> History<T> {
    pub fn push(&mut self, value: T)
    where
        T: PartialEq,
    {
        if let Some(group) = self.before.last_mut() {
            group.push(value);
        } else {
            self.before.push(vec![value]);
        }
        self.after.clear();
    }

    pub fn new_group(&mut self) {
        if let Some(group) = self.before.last_mut() {
            if group.is_empty() {
                return;
            }
        }
        self.before.push(Vec::new());
        self.after.clear();
    }

    #[must_use]
    pub fn undo(&mut self) -> Option<&[T]> {
        let value = self.before.pop()?;
        self.after.push(value);
        Some(self.after.last().unwrap().as_slice())
    }

    #[must_use]
    pub fn redo(&mut self) -> Option<&[T]> {
        let value = self.after.pop()?;
        self.before.push(value);
        Some(self.before.last().unwrap().as_slice())
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
