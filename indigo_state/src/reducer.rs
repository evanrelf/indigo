use std::marker::PhantomData;

pub trait Reducer {
    type State;
    type Action;
    fn run(&self, state: &mut Self::State, action: &Self::Action);
}

pub trait IntoReducer<S, A> {
    type Reducer: Reducer<State = S, Action = A>;
    fn into_reducer(self) -> Self::Reducer;
}

pub struct FunctionReducer<S, A, F> {
    function: F,
    #[allow(clippy::type_complexity)]
    marker: PhantomData<fn() -> (S, A, F)>,
}

impl<S, A, F> Reducer for FunctionReducer<S, A, F>
where
    F: Fn(&mut S, &A),
{
    type State = S;
    type Action = A;
    fn run(&self, state: &mut Self::State, action: &Self::Action) {
        (self.function)(state, action);
    }
}

/******************************************************************************/

impl<S, A, F> IntoReducer<S, A> for F
where
    F: Fn(&mut S, &A),
{
    type Reducer = FunctionReducer<S, A, F>;
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
        reducer.run(&mut state, &action);
        assert_eq!(state.0, 1);
    }
}
