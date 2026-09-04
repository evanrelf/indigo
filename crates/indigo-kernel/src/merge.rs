use std::{
    collections::{BTreeSet, HashSet},
    hash::Hash,
    ops::Deref,
    sync::atomic::{AtomicUsize, Ordering},
};

/// State-based merge
///
/// # Laws
///
/// - Associativity: _x_ ∨ (_y_ ∨ _z_) = (_x_ ∨ _y_) ∨ _z_
/// - Commutativity: _x_ ∨ _y_ = _y_ ∨ _x_
/// - Idempotence: _x_ ∨ _x_ = _x_
/// - Identity: If `Self: Default`, then _x_ ∨ `Self::default()` = _x_
///
/// where _x_ ∨ _y_ means `x.join(y)`
///
/// # References
///
/// - Join-semilattices on Wikipedia: <https://en.wikipedia.org/wiki/Semilattice>
/// - Rob Rix's `semilattices` Haskell package: <https://hackage.haskell.org/package/semilattices>
/// - State-based CRDTs: <https://en.wikipedia.org/wiki/Conflict-free_replicated_data_type#State-based_CRDTs>
pub trait Join {
    fn join(&mut self, other: Self);
}

/// Operation-based merge
///
/// # Laws
///
/// - Atomicity: If `x.apply(d)` fails, `x` is unchanged
/// - Coherence with `Join`: If `Self: Join` and `Delta = Self`, then `x.apply(y)` succeeds and is
///   the same as `x.join(y)`
///
/// # References
///
/// - Operation-based CRDTs: <https://en.wikipedia.org/wiki/Conflict-free_replicated_data_type#Operation-based_CRDTs>
pub trait Apply<Delta> {
    type Error;

    fn apply(&mut self, delta: Delta) -> Result<(), Self::Error>;
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

impl<T> Join for LastWriteWins<T> {
    fn join(&mut self, other: Self) {
        if self.tick < other.tick {
            self.tick = other.tick;
            self.value = other.value;
        }
    }
}

/* TODO: Rethink implementation so it satisfies `Join`'s invariant.
pub struct GrowOnlyCounter<T>(Saturating<T>);

impl<T> Deref for GrowOnlyCounter<T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        &self.0.0
    }
}

impl<T, Rhs> AddAssign<Rhs> for GrowOnlyCounter<T>
where
    Saturating<T>: AddAssign<Rhs>,
{
    fn add_assign(&mut self, rhs: Rhs) {
        self.0 += rhs;
    }
}
*/

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

impl<T> Join for GrowOnlyHashSet<T>
where
    T: Eq + Hash,
{
    fn join(&mut self, mut other: Self) {
        self.set.extend(other.set.drain());
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

impl<T> Join for GrowOnlyBTreeSet<T>
where
    T: Ord,
{
    fn join(&mut self, mut other: Self) {
        self.set.append(&mut other.set);
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
            fn join(&mut self, tc: TestCase) {
                let left = self.draw_index(&tc);
                let right = self.draw_index(&tc);
                let right_register = self.registers[right].clone();
                self.registers[left].join(right_register);
                self.model[left] = max(self.model[left], self.model[right]);
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
