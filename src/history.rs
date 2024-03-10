use std::sync::Arc;

// <https:///github.com/zaboople/klonk/blob/master/TheGURQ.md>
// <https://github.com/evanrelf/indigo/blob/haskell1/src/Indigo/Core/History.hs>

pub struct History<T> {
    before: Vec<Arc<T>>,
    after: Vec<Arc<T>>,
}

impl<T> History<T> {
    pub fn new() -> Self {
        Self {
            before: Vec::new(),
            after: Vec::new(),
        }
    }

    /// Actions occurring before the frame of reference, from newest to oldest.
    pub fn before(&self) -> &[Arc<T>] {
        &self.before
    }

    /// Action that just occurred from the frame of reference.
    pub fn now(&self) -> Option<Arc<T>> {
        self.before.first().cloned()
    }

    /// Actions occurring after the frame of reference, from oldest to newest.
    pub fn after(&self) -> &[Arc<T>] {
        &self.after
    }

    /// Whether the frame of reference is at the epoch (beginning of history).
    pub fn is_epoch(&self) -> bool {
        self.before.is_empty()
    }

    /// Whether the frame of reference is in the past.
    pub fn is_past(&self) -> bool {
        !self.is_present()
    }

    /// Whether the frame of reference is in the present (end of history).
    pub fn is_present(&self) -> bool {
        self.after.is_empty()
    }

    /// Number of actions in the history, irrespective of the frame of reference.
    pub fn len(&self) -> usize {
        self.before.len() + self.after.len()
    }

    pub fn act(&mut self, action: T)
    where
        T: Action,
    {
        if self.after.is_empty() {
            self.before.push(Arc::new(action));
        } else {
            todo!();
        }
    }

    /// Shift the frame of reference one action earlier.
    pub fn shift_earlier(&mut self) {
        if let Some(action) = self.before.pop() {
            self.after.push(action);
        }
    }

    /// Shift the frame of reference one action later.
    pub fn shift_later(&mut self) {
        if let Some(action) = self.after.pop() {
            self.before.push(action);
        }
    }

    /// Shift the frame of reference to the epoch (beginning of history).
    pub fn shift_epoch(&mut self) {
        while !self.is_epoch() {
            self.shift_earlier();
        }
    }

    /// Shift the frame of reference to the present (end of history).
    pub fn shift_present(&mut self) {
        while !self.is_present() {
            self.shift_later();
        }
    }
}

impl<T> Default for History<T> {
    fn default() -> Self {
        Self::new()
    }
}

pub trait Action {
    fn invert(&self) -> Self;
}
