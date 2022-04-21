use crate::{listener::*, reducer::*, type_map::*, field::*};
use std::{
    any::{Any, TypeId},
    collections::HashMap,
};

/// A `Store` holds arbitrary state, reducer functions which change that state in response to
/// actions, and listeners which perform effects in response to state changes.
///
/// # Examples
///
/// ```
#[doc = include_str!("../examples/basic.rs")]
/// ```
#[derive(Default)]
pub struct Store<S = TypeMap> {
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
        S: Field<F>,
        F: Any,
        A: Any,
        R: IntoReducer<F, A>,
        R::Reducer: 'static,
    {
        self.reducers
            .entry(TypeId::of::<A>())
            .or_default()
            .push(Box::new(reducer.into_reducer()));
    }

    /// Adds a listener to the store.
    pub fn add_listener<F, L>(&mut self, listener: L)
    where
        S: Field<F>,
        F: Any,
        L: IntoListener<F>,
        L::Listener: 'static,
    {
        self.listeners
            .entry(TypeId::of::<F>())
            .or_default()
            .push(Box::new(listener.into_listener()));
    }

    /// Dispatches an action to reducers in the store.
    ///
    /// # Panics
    ///
    /// Panics if a reducer or listener requires state not present in the store.
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

impl Store<TypeMap> {
    /// Adds a piece of state to the store.
    pub fn add_state<S>(&mut self, state: S)
    where
        S: Any,
    {
        self.state.insert(state);
    }

    /// Returns a reference to a piece of state from the store.
    pub fn get_state<S>(&self) -> Option<&S>
    where
        S: Any,
    {
        self.state.get()
    }
}

#[cfg(test)]
mod test {
    #![allow(dead_code)]

    use super::*;

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
