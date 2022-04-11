#![allow(dead_code)]

use crate::type_map::TypeMap;
use std::any::Any;

#[derive(Default)]
pub struct Store {
    state: TypeMap,
    reducers: Vec<Box<dyn Any>>,
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

    pub fn add_reducer<F, S, A>(&mut self, reducer: F)
    where
        F: 'static + Fn(&mut S, &A),
        S: 'static,
        A: 'static,
    {
        self.reducers.push(Box::new(reducer));
    }

    pub fn dispatch<S, A>(&mut self, action: A)
    where
        S: 'static,
        A: 'static,
    {
        /*
        for reducer in &self.reducers {
            let reducer: Box<dyn Fn(&mut S, &A)> = if let Ok(r) = reducer.downcast() {
                r
            } else {
                return;
            };

            let state: = if let Some(s) = self.state.get_mut::<S>() {
                s
            } else {
                return;
            };

            reducer(state, action);
        }
        */
    }
}

pub trait Provide {
    fn provide(self, state: &mut TypeMap, action: &Box<dyn Any>);
}

impl<S, A> Provide for Box<dyn Fn(&mut S, &A)>
where
    S: 'static,
    A: 'static,
{
    fn provide(self, state: &mut TypeMap, action: &Box<dyn Any>) {
        let state = if let Some(s) = state.get_mut() {
            s
        } else {
            return;
        };

        let action = if let Some(a) = action.downcast_ref() {
            a
        } else {
            return;
        };

        self(state, action);
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[derive(Debug, Default, PartialEq)]
    struct Counter(isize);

    enum CounterAction {
        Incremented,
        Decremented,
    }

    fn increment(counter: &mut Counter, action: &CounterAction) {
        use CounterAction::*;

        match action {
            Incremented => counter.0 += 1,
            Decremented => counter.0 -= 1,
        }
    }

    #[test]
    fn test() {
        let mut store = Store::new();
        let mut type_map = TypeMap::new();

        type_map.insert(Counter::default());
        store.add_state(Counter::default());

        store.add_reducer(increment);

        (Box::new(increment) as Box<dyn Fn(&mut Counter, &CounterAction)>).provide(
            &mut type_map,
            &(Box::new(CounterAction::Incremented) as Box<dyn Any>),
        );

        assert_eq!(type_map.remove::<Counter>(), Some(Box::new(Counter(1))));
    }
}
