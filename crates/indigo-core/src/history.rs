use imbl::Vector;
use std::{iter, marker::PhantomData};

#[derive(Debug)]
pub struct History<I, T = Vec<I>>
where
    T: Clone + Default + Extend<I>,
{
    transactions: Vector<T>,
    index: usize,
    last_is_committed: bool,
    phantom: PhantomData<fn(I)>,
}

impl<I, T> History<I, T>
where
    T: Clone + Default + Extend<I>,
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
            self.transactions.push_back(T::default());
            self.index += 1;
            self.last_is_committed = false;
        }
        // Extend transaction with item.
        self.transactions
            .back_mut()
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
    T: Clone + Default + Extend<I>,
{
    fn default() -> Self {
        Self {
            transactions: Vector::new(),
            index: 0,
            last_is_committed: true,
            phantom: PhantomData,
        }
    }
}

#[derive(Clone, Debug, Default, PartialEq)]
pub struct First<T>(pub Option<T>);

impl<T> Extend<T> for First<T> {
    fn extend<I>(&mut self, iter: I)
    where
        I: IntoIterator<Item = T>,
    {
        if self.0.is_none() {
            self.0 = iter.into_iter().next();
        }
    }
}

#[derive(Clone, Debug, Default, PartialEq)]
pub struct Last<T>(pub Option<T>);

impl<T> Extend<T> for Last<T> {
    fn extend<I>(&mut self, iter: I)
    where
        I: IntoIterator<Item = T>,
    {
        self.0 = iter.into_iter().last();
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::ops::AddAssign;

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

    #[test]
    fn fork_timeline_discards_future() {
        let mut history: History<u8> = History::new();
        history.push(1);
        history.commit();
        history.push(2);
        history.commit();
        history.undo();
        history.push(3);
        assert_eq!(history.undo(), Some(&vec![3]));
    }

    #[derive(Clone, Debug, Default, PartialEq)]
    struct Sum<T>(T);

    impl<T> Extend<T> for Sum<T>
    where
        T: AddAssign,
    {
        fn extend<I>(&mut self, ns: I)
        where
            I: IntoIterator<Item = T>,
        {
            for n in ns {
                self.0 += n;
            }
        }
    }

    #[test]
    fn custom_transaction_type_sum() {
        let mut history: History<u8, Sum<u8>> = History::new();
        history.push(1);
        history.push(1);
        history.push(1);
        history.commit();
        history.push(2);
        history.push(2);
        history.push(2);
        history.commit();
        assert_eq!(history.undo(), Some(&Sum(6)));
        assert_eq!(history.undo(), Some(&Sum(3)));
    }

    #[test]
    fn custom_transaction_type_first() {
        let mut history: History<u8, First<u8>> = History::new();
        history.push(1);
        history.push(2);
        history.push(3);
        history.commit();
        history.push(4);
        history.push(5);
        history.push(6);
        history.commit();
        assert_eq!(history.undo(), Some(&First(Some(4))));
        assert_eq!(history.undo(), Some(&First(Some(1))));
    }

    #[test]
    fn custom_transaction_type_last() {
        let mut history: History<u8, Last<u8>> = History::new();
        history.push(1);
        history.push(2);
        history.push(3);
        history.commit();
        history.push(4);
        history.push(5);
        history.push(6);
        history.commit();
        assert_eq!(history.undo(), Some(&Last(Some(6))));
        assert_eq!(history.undo(), Some(&Last(Some(3))));
    }
}
