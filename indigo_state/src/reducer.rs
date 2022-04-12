use crate::store::Store;
use std::{any::TypeId, marker::PhantomData};

// Problems to solve:
// - Store reducers for calling in the future
// - Get values from `TypeMap` to give to reducer functions
// - Dispatch actions to reducers that can handle them

/******************************************************************************/

pub trait Reducer {
    type State: 'static;
    type Action: 'static;
    fn reduce(&self, state: &mut Self::State, action: &Self::Action);
}

pub trait IntoReducer<State, Action> {
    type Reducer: Reducer<State = State, Action = Action>;
    fn into_reducer(self) -> Self::Reducer;
}

pub struct FunctionReducer<State, Action, F> {
    function: F,
    #[allow(clippy::type_complexity)]
    marker: PhantomData<fn() -> (State, Action, F)>,
}

impl<State, Action, F> Reducer for FunctionReducer<State, Action, F>
where
    State: 'static,
    Action: 'static,
    F: Fn(&mut State, &Action),
{
    type State = State;
    type Action = Action;
    fn reduce(&self, state: &mut Self::State, action: &Self::Action) {
        (self.function)(state, action);
    }
}

impl<State, Action, F> IntoReducer<State, Action> for F
where
    State: 'static,
    Action: 'static,
    F: Fn(&mut State, &Action),
{
    type Reducer = FunctionReducer<State, Action, F>;
    fn into_reducer(self) -> Self::Reducer {
        FunctionReducer {
            function: self,
            marker: PhantomData,
        }
    }
}

#[cfg(test)]
mod test {
    #![allow(dead_code)]

    use super::*;

    struct Counter(isize);

    enum CounterAction {
        Incremented,
        Decremented,
    }

    fn counter_reducer(state: &mut Counter, action: &CounterAction) {
        use CounterAction::*;

        match action {
            Incremented => state.0 += 1,
            Decremented => state.0 -= 1,
        }
    }

    #[test]
    fn test() {
        let mut state = Counter(0);
        let action = CounterAction::Incremented;
        let reducer = counter_reducer.into_reducer();
        reducer.reduce(&mut state, &action);
        assert_eq!(state.0, 1);
    }
}

/******************************************************************************/
