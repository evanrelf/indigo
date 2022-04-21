use crate::{listener::*, reducer::*};
use std::{
    any::{Any, TypeId},
    collections::HashMap,
    ops::Deref,
};

/// A `Store` holds arbitrary state, reducer functions which change that state in response to
/// actions, and listeners which perform effects in response to state changes.
#[derive(Default)]
pub struct Store<S> {
    state: S,
    // Map from action type ID to vector of reducer functions
    reducers: HashMap<TypeId, Vec<Box<dyn StoreReducer<S>>>>,
    // Map from state type ID to vector of listener functions
    listeners: HashMap<TypeId, Vec<Box<dyn StoreListener<S>>>>,
}

impl<S> Store<S> {
    /// Creates a new store holding the given state.
    pub fn new(state: S) -> Self {
        Self {
            state,
            reducers: Default::default(),
            listeners: Default::default(),
        }
    }

    /// Adds a reducer to the store.
    pub fn add_reducer<F, A, R>(&mut self, reducer: R)
    where
        A: Any,
        R: IntoStoreReducer<S, F, A>,
    {
        self.reducers
            .entry(TypeId::of::<A>())
            .or_default()
            .push(reducer.into_store_reducer());
    }

    /// Adds a listener to the store.
    pub fn add_listener<F, L>(&mut self, listener: L)
    where
        F: Any,
        L: IntoStoreListener<S, F>,
    {
        self.listeners
            .entry(TypeId::of::<F>())
            .or_default()
            .push(listener.into_store_listener());
    }

    /// Dispatches an action to reducers in the store.
    ///
    /// # Panics
    ///
    /// Panics if a reducer or listener requires a field not present in the
    /// store's state.
    pub fn dispatch<A>(&mut self, action: A)
    where
        A: Any,
    {
        self.reducers
            .get(&TypeId::of::<A>())
            .into_iter()
            .flatten()
            .for_each(|reducer| {
                let changed = reducer.reduce(&mut self.state, &action);
                if changed {
                    self.listeners
                        .get_mut(&reducer.state_type_id())
                        .into_iter()
                        .flatten()
                        .for_each(|listener| {
                            listener.as_mut().listen(&self.state);
                        });
                }
            });
    }
}

impl<S> Deref for Store<S> {
    type Target = S;

    fn deref(&self) -> &Self::Target {
        &self.state
    }
}

#[cfg(test)]
mod test {
    #![allow(dead_code)]

    use super::*;
    use crate::type_map::TypeMap;

    #[test]
    #[should_panic]
    fn test_missing_state() {
        struct Count(i32);
        enum CountAction {
            Incremented,
            Decremented,
        }

        let mut store: Store<TypeMap> = Store::default();
        store.add_reducer(|_state: &mut Count, _action: &CountAction| {});
        store.dispatch(CountAction::Incremented);
    }
}
