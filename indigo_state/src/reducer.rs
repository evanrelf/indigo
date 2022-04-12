use std::marker::PhantomData;

pub trait Reducer {
    type State: 'static;

    type Action: 'static;

    fn reduce(&self, state: &mut Self::State, action: &Self::Action);
}

pub trait IntoReducer<S, A> {
    type Reducer: 'static + Reducer<State = S, Action = A>;

    fn into_reducer(self) -> Self::Reducer;
}

pub struct FunctionReducer<S, A, F> {
    function: F,
    #[allow(clippy::type_complexity)]
    marker: PhantomData<fn() -> (S, A, F)>,
}

impl<S, A, F> Reducer for FunctionReducer<S, A, F>
where
    S: 'static,
    A: 'static,
    F: Fn(&mut S, &A),
{
    type State = S;

    type Action = A;

    fn reduce(&self, state: &mut Self::State, action: &Self::Action) {
        (self.function)(state, action);
    }
}

impl<S, A, F> IntoReducer<S, A> for F
where
    S: 'static,
    A: 'static,
    F: 'static + Fn(&mut S, &A),
{
    type Reducer = FunctionReducer<S, A, F>;

    fn into_reducer(self) -> Self::Reducer {
        FunctionReducer {
            function: self,
            marker: PhantomData,
        }
    }
}
