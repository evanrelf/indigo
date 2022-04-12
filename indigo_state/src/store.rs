use crate::{listener::*, reducer::*};
use std::{
    any::{Any, TypeId},
    collections::HashMap,
};

#[derive(Default)]
pub struct Store {
    state: HashMap<TypeId, Box<dyn Any>>,
    reducers: HashMap<TypeId, Vec<Box<dyn StoreReducer>>>,
    listeners: HashMap<TypeId, Vec<Box<dyn StoreListener>>>,
}

impl Store {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn add_state<S>(&mut self, state: S)
    where
        S: 'static,
    {
        self.state.insert(state.type_id(), Box::new(state));
    }

    pub fn get_state<S>(&self) -> Option<&S>
    where
        S: 'static,
    {
        self.state
            .get(&TypeId::of::<S>())
            .map(|b| b.downcast_ref().unwrap())
    }

    pub fn add_reducer<S, A, R>(&mut self, reducer: R)
    where
        A: 'static,
        R: IntoReducer<S, A>,
    {
        self.reducers
            .entry(TypeId::of::<A>())
            .or_default()
            .push(Box::new(reducer.into_reducer()));
    }

    pub fn add_listener<S, L>(&mut self, listener: L)
    where
        S: 'static,
        L: IntoListener<S>,
    {
        self.listeners
            .entry(TypeId::of::<S>())
            .or_default()
            .push(Box::new(listener.into_listener()));
    }

    pub fn dispatch<A>(&mut self, action: A)
    where
        A: 'static,
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
    fn action_type_id(&self) -> TypeId;
}

impl<R> StoreReducer for R
where
    R: Reducer,
{
    fn reduce(&self, state: &mut HashMap<TypeId, Box<dyn Any>>, action: &dyn Any) -> bool {
        let state = match state
            .get_mut(&TypeId::of::<R::State>())
            .map(|b| b.downcast_mut().unwrap())
        {
            None => panic!("Reducer references state not present in store"),
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
    fn action_type_id(&self) -> TypeId {
        TypeId::of::<R::Action>()
    }
}

trait StoreListener {
    fn listen(&mut self, state: &HashMap<TypeId, Box<dyn Any>>);
    fn state_type_id(&self) -> TypeId;
}

impl<L> StoreListener for L
where
    L: Listener,
{
    fn listen(&mut self, state: &HashMap<TypeId, Box<dyn Any>>) {
        let state = match state
            .get(&TypeId::of::<L::State>())
            .map(|b| b.downcast_ref().unwrap())
        {
            None => panic!("Listener references state not present in store"),
            Some(s) => s,
        };
        self.listen(state);
    }
    fn state_type_id(&self) -> TypeId {
        TypeId::of::<L::State>()
    }
}

#[cfg(test)]
mod test {
    #![allow(dead_code)]

    use super::*;
    use std::sync::{Arc, Mutex};

    #[derive(Debug, PartialEq)]
    struct Name(String);

    enum NameAction {
        Renamed(String),
        Cleared,
    }

    fn name_reducer(state: &mut Name, action: &NameAction) {
        use NameAction::*;

        match action {
            Renamed(name) => state.0 = name.clone(),
            Cleared => state.0.clear(),
        }
    }

    #[derive(Debug, PartialEq)]
    struct Count(isize);

    enum CountAction {
        Incremented,
        Decremented,
    }

    fn count_reducer(state: &mut Count, action: &CountAction) {
        use CountAction::*;

        match action {
            Incremented => state.0 += 1,
            Decremented => state.0 -= 1,
        }
    }

    #[test]
    fn test_general_use() {
        let mut store = Store::new();

        store.add_state(Count(0));
        store.add_reducer(count_reducer);

        let count_changes = Arc::new(Mutex::new(0u8));
        let listener_count_changes = Arc::clone(&count_changes);
        store.add_listener(move |_: &Count| {
            *listener_count_changes.lock().unwrap() += 1;
        });

        store.add_state(Name("Alice".to_string()));
        store.add_reducer(name_reducer);

        store.dispatch(CountAction::Incremented);
        store.dispatch(CountAction::Incremented);
        store.dispatch(CountAction::Decremented);

        store.dispatch(NameAction::Renamed("Bob".to_string()));

        assert_eq!(*store.get_state::<Count>().unwrap(), Count(1));
        assert_eq!(*count_changes.lock().unwrap(), 3);
        assert_eq!(*store.get_state::<Name>().unwrap(), Name("Bob".to_string()));
    }

    #[test]
    fn test_reducer_closure() {
        let mut store = Store::new();

        store.add_state(Count(0));
        store.add_reducer(|state: &mut Count, action: &CountAction| {
            use CountAction::*;

            match action {
                Incremented => state.0 += 1,
                Decremented => state.0 -= 1,
            }
        });

        store.dispatch(CountAction::Incremented);
        store.dispatch(CountAction::Incremented);
        store.dispatch(CountAction::Decremented);

        assert_eq!(*store.get_state::<Count>().unwrap(), Count(1));
    }

    #[test]
    #[should_panic]
    fn test_missing_state() {
        let mut store = Store::new();

        store.add_reducer(count_reducer);

        store.dispatch(CountAction::Incremented);
    }
}
