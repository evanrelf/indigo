use crate::{reducer::*, type_map::TypeMap};
use std::any::Any;

// Ideas:
// - Allow reducers to request multiple pieces of state?
//   - Separate `StoreQuery` trait for handling `Query<(Foo, Bar, Baz)>`?
//   - Allow requesting context? (e.g. read-only access to another field)
// - Allow reducers to handle multiple different events?
// - Panic when action isn't handled by any reducers?
// - Priorities for reducers?
//   - Levels
//     - Override (don't let anything else handle this action)
//     - Before{High,Medium,Low} (handle action before the default)
//     - Default
//     - After{High,Medium,Low} (handle action after the default)
//     - Fallback (only run if nothing else handled this action)
//   - Panic when priority is ambiguous? (e.g. multiple "override" or
//     "fallback"-level reducers)
// - Add more helpful panic message(s)? (e.g. using `std::any::type_name`)

#[derive(Default)]
pub struct Store {
    state: TypeMap,
    reducers: Vec<Box<dyn StoreReducer>>,
}

impl Store {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn add_state<S>(&mut self, state: S)
    where
        S: 'static,
    {
        self.state.insert(state);
    }

    pub fn get_state<S>(&self) -> Option<&S>
    where
        S: 'static,
    {
        self.state.get()
    }

    pub fn add_reducer<S, A, R>(&mut self, reducer: R)
    where
        R: IntoReducer<S, A>,
    {
        self.reducers.push(Box::new(reducer.into_reducer()));
    }

    pub fn dispatch<A>(&mut self, action: A)
    where
        A: 'static,
    {
        for reducer in &self.reducers {
            reducer.reduce(&mut self.state, &action);
        }
    }
}

pub trait StoreReducer {
    fn reduce(&self, type_map: &mut TypeMap, action: &dyn Any);
}

impl<R> StoreReducer for R
where
    R: Reducer,
{
    fn reduce(&self, type_map: &mut TypeMap, action: &dyn Any) {
        let state = match type_map.get_mut() {
            None => panic!("Reducer references state not present in store"),
            Some(s) => s,
        };
        let action = match action.downcast_ref() {
            None => return,
            Some(a) => a,
        };
        self.reduce(state, action);
    }
}

#[cfg(test)]
mod test {
    #![allow(dead_code)]

    use super::*;

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
    fn test1() {
        let mut store = Store::new();

        store.add_state(Count(0));
        store.add_reducer(count_reducer);

        store.add_state(Name("Alice".to_string()));
        store.add_reducer(name_reducer);

        store.dispatch(CountAction::Incremented);
        store.dispatch(CountAction::Incremented);
        store.dispatch(CountAction::Decremented);

        store.dispatch(NameAction::Renamed("Bob".to_string()));

        assert_eq!(*store.get_state::<Count>().unwrap(), Count(1));
        assert_eq!(*store.get_state::<Name>().unwrap(), Name("Bob".to_string()));
    }

    #[test]
    #[should_panic]
    fn test2() {
        let mut store = Store::new();

        store.add_reducer(count_reducer);

        store.dispatch(CountAction::Incremented);
    }
}
