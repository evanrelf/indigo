use crate::type_map::TypeMap;
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

pub trait StoreListener<S = TypeMap> {
    fn listen(&mut self, state: &S);
}

impl<L> StoreListener for L
where
    L: 'static + Listener,
{
    fn listen(&mut self, state: &TypeMap) {
        let state = match state.get::<L::State>() {
            None => panic!("Listener requires state not present in store"),
            Some(s) => s,
        };

        self.listen(state);
    }
}
