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
            let after = std::mem::take(&mut self.after);

            let reversed = after.iter().cloned().rev();

            let inverted = after.iter().map(|action| Arc::new(action.invert()));

            self.before.extend(reversed);

            self.before.extend(inverted);

            self.before.push(Arc::new(action));
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

#[cfg(test)]
mod tests {
    use super::*;
    use quickcheck_macros::quickcheck;

    impl Action for (i32, bool) {
        fn invert(&self) -> Self {
            (self.0, !self.1)
        }
    }

    fn make_history<T>(actions: Vec<T>) -> History<T>
    where
        T: Action,
    {
        let mut history = History::new();
        for action in actions {
            history.act(action);
        }
        history
    }

    // TODO: Write more tests. These hardly cover anything.

    #[quickcheck]
    fn window_shopping_doesnt_fork(actions: Vec<(i32, bool)>, earlier: u8, later: u8) {
        let mut history = make_history(actions);

        let before_len = history.len();

        for _ in 0..=earlier {
            history.shift_earlier();
        }

        for _ in 0..=later {
            history.shift_later();
        }

        let after_len = history.len();

        assert_eq!(before_len, after_len);
    }

    #[quickcheck]
    fn acting_in_the_past_sends_you_to_the_present(actions: Vec<(i32, bool)>) {
        if actions.is_empty() {
            return;
        }

        let mut history = make_history(actions);

        history.shift_earlier();

        assert!(!history.after().is_empty());

        history.act((42, true));

        assert!(history.after().is_empty());
    }
}
