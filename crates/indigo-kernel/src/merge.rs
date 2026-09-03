use std::{
    collections::{BTreeSet, HashSet},
    convert::Infallible,
    hash::Hash,
    ops::Deref,
    sync::atomic::{AtomicUsize, Ordering},
};

pub trait Merge {
    type Error;
    fn merge(&mut self, child: Self) -> Result<(), Self::Error>;
}

static CLOCK: AtomicUsize = AtomicUsize::new(0);

#[derive(Clone)]
pub struct LastWriteWins<T: ?Sized> {
    tick: usize,
    value: T,
}

impl<T> LastWriteWins<T> {
    #[must_use]
    pub fn new(value: T) -> Self {
        Self {
            tick: CLOCK.fetch_add(1, Ordering::Relaxed) + 1,
            value,
        }
    }

    #[must_use]
    pub fn read(&self) -> &T {
        &self.value
    }

    pub fn write(&mut self, value: T) {
        self.tick = CLOCK.fetch_add(1, Ordering::Relaxed) + 1;
        self.value = value;
    }
}

impl<T> Deref for LastWriteWins<T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        self.read()
    }
}

impl<T> Merge for LastWriteWins<T> {
    type Error = Infallible;
    fn merge(&mut self, child: Self) -> Result<(), Self::Error> {
        if self.tick < child.tick {
            self.tick = child.tick;
            self.value = child.value;
        }
        Ok(())
    }
}

pub struct GrowOnlyHashSet<T> {
    set: HashSet<T>,
}

impl<T> GrowOnlyHashSet<T> {
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    pub fn insert(&mut self, value: T) -> bool
    where
        T: Eq + Hash,
    {
        self.set.insert(value)
    }
}

impl<T> Default for GrowOnlyHashSet<T> {
    fn default() -> Self {
        Self {
            set: HashSet::default(),
        }
    }
}

impl<T> Deref for GrowOnlyHashSet<T> {
    type Target = HashSet<T>;
    fn deref(&self) -> &Self::Target {
        &self.set
    }
}

impl<T> Merge for GrowOnlyHashSet<T>
where
    T: Eq + Hash,
{
    type Error = Infallible;
    fn merge(&mut self, mut child: Self) -> Result<(), Self::Error> {
        self.set.extend(child.set.drain());
        Ok(())
    }
}

pub struct GrowOnlyBTreeSet<T> {
    set: BTreeSet<T>,
}

impl<T> GrowOnlyBTreeSet<T> {
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    pub fn insert(&mut self, value: T) -> bool
    where
        T: Ord,
    {
        self.set.insert(value)
    }

    pub fn append(&mut self, other: &mut Self)
    where
        T: Ord,
    {
        self.set.append(&mut other.set);
    }
}

impl<T> Default for GrowOnlyBTreeSet<T> {
    fn default() -> Self {
        Self {
            set: BTreeSet::default(),
        }
    }
}

impl<T> Deref for GrowOnlyBTreeSet<T> {
    type Target = BTreeSet<T>;
    fn deref(&self) -> &Self::Target {
        &self.set
    }
}

impl<T> Merge for GrowOnlyBTreeSet<T>
where
    T: Ord,
{
    type Error = Infallible;
    fn merge(&mut self, mut child: Self) -> Result<(), Self::Error> {
        self.set.append(&mut child.set);
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use hegel::{TestCase, generators as gs};
    use std::{cmp::max, iter::zip};

    #[hegel::test(test_cases = 100)]
    fn test_last_write_wins(tc: TestCase) {
        #[derive(Default)]
        struct StateMachine {
            registers: Vec<LastWriteWins<i64>>,
            model: Vec<(usize, i64)>,
            next_sequence_number: usize,
        }

        #[expect(clippy::needless_pass_by_value)]
        #[hegel::state_machine]
        impl StateMachine {
            fn draw_index(&self, tc: &TestCase) -> usize {
                tc.assume(!self.registers.is_empty());
                tc.draw(gs::integers::<usize>().max_value(self.registers.len() - 1))
            }
            fn next_write(&mut self, value: i64) -> (usize, i64) {
                let sequence_number = self.next_sequence_number;
                self.next_sequence_number += 1;
                (sequence_number, value)
            }
            #[rule]
            fn new_register(&mut self, tc: TestCase) {
                let value = tc.draw(gs::integers::<i64>());
                self.registers.push(LastWriteWins::new(value));
                let write = self.next_write(value);
                self.model.push(write);
            }
            #[rule]
            fn write(&mut self, tc: TestCase) {
                let index = self.draw_index(&tc);
                let value = tc.draw(gs::integers::<i64>());
                self.registers[index].write(value);
                self.model[index] = self.next_write(value);
            }
            #[rule]
            fn merge(&mut self, tc: TestCase) {
                let parent = self.draw_index(&tc);
                let child = self.draw_index(&tc);
                let child_register = self.registers[child].clone();
                self.registers[parent].merge(child_register).unwrap();
                self.model[parent] = max(self.model[parent], self.model[child]);
            }
            #[invariant]
            fn reads_agree_with_model(&self, _: TestCase) {
                for (register, (_, value)) in zip(&self.registers, &self.model) {
                    assert_eq!(register.read(), value);
                }
            }
        }

        hegel::stateful::run(StateMachine::default(), tc);
    }
}
