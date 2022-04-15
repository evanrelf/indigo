use crate::{listener::*, reducer::*};
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
#[doc = include_str!("../examples/example.rs")]
/// ```
#[derive(Default)]
pub struct Store {
    state: HashMap<TypeId, Box<dyn Any>>,
    // Map from action type ID to vector of reducer functions
    reducers: HashMap<TypeId, Vec<Box<dyn StoreReducer>>>,
    // Map from state type ID to vector of listener functions
    listeners: HashMap<TypeId, Vec<Box<dyn StoreListener>>>,
}

impl Store {
    /// Creates an empty store.
    pub fn new() -> Self {
        Self::default()
    }

    /// Adds a piece of state to the store.
    pub fn add_state<S>(&mut self, state: S)
    where
        S: Any,
    {
        self.state.insert(state.type_id(), Box::new(state));
    }

    /// Adds a reducer to the store.
    pub fn add_reducer<S, A, R>(&mut self, reducer: R)
    where
        A: Any,
        R: IntoReducer<S, A>,
        R::Reducer: 'static,
    {
        self.reducers
            .entry(TypeId::of::<A>())
            .or_default()
            .push(Box::new(reducer.into_reducer()));
    }

    /// Adds a listener to the store.
    pub fn add_listener<S, L>(&mut self, listener: L)
    where
        S: Any,
        L: IntoListener<S>,
        L::Listener: 'static,
    {
        self.listeners
            .entry(TypeId::of::<S>())
            .or_default()
            .push(Box::new(listener.into_listener()));
    }

    /// Returns a reference to a piece of state from the store.
    pub fn get_state<S>(&self) -> Option<&S>
    where
        S: Any,
    {
        self.state
            .get(&TypeId::of::<S>())
            // `unwrap` is safe because `add_state` uses the value's type ID as the key
            .map(|b| b.downcast_ref().unwrap())
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

trait StoreReducer {
    fn reduce(&self, state: &mut HashMap<TypeId, Box<dyn Any>>, action: &dyn Any) -> bool;

    fn state_type_id(&self) -> TypeId;
}

impl<R> StoreReducer for R
where
    R: 'static + Reducer,
{
    fn reduce(&self, state: &mut HashMap<TypeId, Box<dyn Any>>, action: &dyn Any) -> bool {
        let state = match state
            .get_mut(&TypeId::of::<R::State>())
            // `unwrap` is safe because `add_state` uses the value's type ID as the key
            .map(|b| b.downcast_mut().unwrap())
        {
            None => panic!("Reducer requires state not present in store"),
            Some(s) => s,
        };

        let action = match action.downcast_ref() {
            None => return false,
            Some(a) => a,
        };

        self.reduce(state, action);

        true
    }

    fn state_type_id(&self) -> TypeId {
        TypeId::of::<R::State>()
    }
}

trait StoreListener {
    fn listen(&mut self, state: &HashMap<TypeId, Box<dyn Any>>);
}

impl<L> StoreListener for L
where
    L: 'static + Listener,
{
    fn listen(&mut self, state: &HashMap<TypeId, Box<dyn Any>>) {
        let state = match state
            .get(&TypeId::of::<L::State>())
            // `unwrap` is safe because `add_state` uses the value's type ID as the key
            .map(|b| b.downcast_ref().unwrap())
        {
            None => panic!("Listener requires state not present in store"),
            Some(s) => s,
        };

        self.listen(state);
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

        let mut store = Store::new();
        store.add_reducer(|_state: &mut Count, _action: &CountAction| {});
        store.dispatch(CountAction::Incremented);
    }
}
