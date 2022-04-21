use crate::field::Field;
use std::{any::Any, marker::PhantomData};

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
        let state = match state.field() {
            None => panic!("Listener requires state not present in store"),
            Some(s) => s,
        };

        self.listen(state);
    }
}

pub trait IntoStoreListener<S, F> {
    fn into_store_listener(self) -> Box<dyn StoreListener<S>>;
}

impl<S, F, L> IntoStoreListener<S, F> for L
where
    S: Field<F>,
    F: Any,
    L: IntoListener<F>,
    L::Listener: 'static,
{
    fn into_store_listener(self) -> Box<dyn StoreListener<S>> {
        Box::new(self.into_listener())
    }
}
