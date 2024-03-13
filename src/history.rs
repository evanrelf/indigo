use imbl::Vector;

// <https:///github.com/zaboople/klonk/blob/master/TheGURQ.md>
// <https://github.com/evanrelf/indigo/blob/haskell1/src/Indigo/Core/History.hs>

pub struct History<T> {
    before: Vector<T>,
    after: Vector<T>,
}

impl<T> History<T> {
    pub fn new() -> Self {
        Self {
            before: Vector::new(),
            after: Vector::new(),
        }
    }

    /// Actions occurring before the frame of reference, from newest to oldest.
    pub fn before(&self) -> &Vector<T> {
        &self.before
    }

    /// Action that just occurred from the frame of reference.
    pub fn now(&self) -> Option<&T> {
        self.before.front()
    }

    /// Actions occurring after the frame of reference, from oldest to newest.
    pub fn after(&self) -> &Vector<T> {
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
        T: Action + Clone,
    {
        if self.after.is_empty() {
            self.before.push_back(action);
        } else {
            let after = std::mem::take(&mut self.after);

            let reversed = after.iter().cloned().rev();

            let inverted = after.iter().map(Action::invert);

            self.before.extend(reversed);

            self.before.extend(inverted);

            self.before.push_back(action);
        }
    }

    /// Shift the frame of reference one action earlier.
    pub fn shift_earlier(&mut self)
    where
        T: Clone,
    {
        if let Some(action) = self.before.pop_back() {
            self.after.push_back(action);
        }
    }

    /// Shift the frame of reference one action later.
    pub fn shift_later(&mut self)
    where
        T: Clone,
    {
        if let Some(action) = self.after.pop_back() {
            self.before.push_back(action);
        }
    }

    /// Shift the frame of reference to the epoch (beginning of history).
    pub fn shift_epoch(&mut self)
    where
        T: Clone,
    {
        while !self.is_epoch() {
            self.shift_earlier();
        }
    }

    /// Shift the frame of reference to the present (end of history).
    pub fn shift_present(&mut self)
    where
        T: Clone,
    {
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
        T: Action + Clone,
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
