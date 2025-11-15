use std::{iter, marker::PhantomData};

#[derive(Debug)]
pub struct History<I, T = Vec<I>>
where
    T: Default + Extend<I>,
{
    transactions: Vec<T>,
    index: usize,
    last_is_committed: bool,
    phantom: PhantomData<fn(I)>,
}

impl<I, T> History<I, T>
where
    T: Default + Extend<I>,
{
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    // NOTE: Replace `extend` + `iter::once` with `extend_one` once it stabilizes.
    // https://github.com/rust-lang/rust/issues/72631
    pub fn push(&mut self, item: I) {
        // If forking the timeline in the past, forget the future.
        if self.index < self.transactions.len() {
            self.transactions.truncate(self.index);
            self.last_is_committed = true;
        }
        // If transaction is committed, start a new one.
        if self.last_is_committed {
            self.transactions.push(T::default());
            self.index += 1;
            self.last_is_committed = false;
        }
        // Extend transaction with item.
        self.transactions
            .last_mut()
            .unwrap()
            .extend(iter::once(item));
    }

    pub fn commit(&mut self) {
        self.last_is_committed = true;
    }

    /// Returns undone change.
    pub fn undo(&mut self) -> Option<&T> {
        if self.index == 0 {
            return None;
        }
        self.index -= 1;
        self.last_is_committed = true;
        let transaction = &self.transactions[self.index];
        Some(transaction)
    }

    /// Returns redone change.
    pub fn redo(&mut self) -> Option<&T> {
        if self.index == self.transactions.len() {
            return None;
        }
        let transaction = &self.transactions[self.index];
        self.index += 1;
        self.last_is_committed = true;
        Some(transaction)
    }
}

impl<I, T> Default for History<I, T>
where
    T: Default + Extend<I>,
{
    fn default() -> Self {
        Self {
            transactions: Vec::new(),
            index: 0,
            last_is_committed: true,
            phantom: PhantomData,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn undo_returns_undone() {
        let mut history: History<u8> = History::new();
        history.push(1);
        history.push(2);
        assert_eq!(history.undo(), Some(&vec![1, 2]));
        assert_eq!(history.undo(), None);
    }

    #[test]
    fn redo_returns_redone() {
        let mut history: History<u8> = History::new();
        history.push(1);
        history.push(2);
        history.undo();
        assert_eq!(history.redo(), Some(&vec![1, 2]));
        assert_eq!(history.redo(), None);
    }

    #[test]
    fn undo_redo_symmetry() {
        let mut history: History<u8> = History::new();
        history.push(1);
        history.push(2);
        history.push(3);
        history.commit();
        history.push(4);
        history.push(5);
        history.push(6);
        history.commit();
        assert_eq!(history.undo(), Some(&vec![4, 5, 6]));
        assert_eq!(history.undo(), Some(&vec![1, 2, 3]));
        assert_eq!(history.redo(), Some(&vec![1, 2, 3]));
        assert_eq!(history.redo(), Some(&vec![4, 5, 6]));
    }

    #[test]
    fn no_movement_no_commit() {
        let mut history: History<u8> = History::new();
        history.push(1);
        // Doesn't move because there's nothing to redo.
        history.redo();
        history.push(2);
        // No movement -> no commit.
        assert_eq!(history.undo(), Some(&vec![1, 2]));
    }

    #[test]
    fn movement_commits() {
        let mut history: History<u8> = History::new();
        history.push(1);
        // Moves to the past and then back to the present.
        history.undo();
        history.redo();
        history.push(2);
        // Movement -> commit.
        assert_eq!(history.undo(), Some(&vec![2]));
    }
}
