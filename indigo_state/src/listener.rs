use crate::field::Field;
use std::marker::PhantomData;

pub trait Listener {
    type State;

    fn listen(&mut self, state: &Self::State);
}

pub trait IntoListener<S> {
    type Listener: Listener<State = S>;

    fn into_listener(self) -> Self::Listener;
}

pub struct FunctionListener<S, F> {
    function: F,
    marker: PhantomData<fn() -> (S, F)>,
}

impl<S, F> Listener for FunctionListener<S, F>
where
    F: FnMut(&S),
{
    type State = S;

    fn listen(&mut self, state: &Self::State) {
        (self.function)(state);
    }
}

impl<S, F> IntoListener<S> for F
where
    F: FnMut(&S),
{
    type Listener = FunctionListener<S, F>;

    fn into_listener(self) -> Self::Listener {
        FunctionListener {
            function: self,
            marker: PhantomData,
        }
    }
}

pub trait StoreListener<S> {
    fn listen(&mut self, state: &S);
}

impl<S, L> StoreListener<S> for L
where
    S: Field<L::State>,
    L: Listener,
{
    fn listen(&mut self, state: &S) {
        let state = match state.get() {
            None => panic!("Listener requires state not present in store"),
            Some(s) => s,
        };

        self.listen(state);
    }
}
