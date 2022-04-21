use crate::field::Field;
use std::{
    any::{Any, TypeId},
    marker::PhantomData,
};

pub trait Reducer {
    type State;

    type Action;

    fn reduce(&self, state: &mut Self::State, action: &Self::Action);
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

    fn reduce(&self, state: &mut Self::State, action: &Self::Action) {
        (self.function)(state, action);
    }
}

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

pub trait StoreReducer<S> {
    fn reduce(&self, state: &mut S, action: &dyn Any) -> bool;

    fn state_type_id(&self) -> TypeId;
}

impl<S, R> StoreReducer<S> for R
where
    S: Field<R::State>,
    R: Reducer,
    R::State: Any,
    R::Action: Any,
{
    fn reduce(&self, state: &mut S, action: &dyn Any) -> bool {
        let state = match state.field_mut() {
            Err(_) => panic!("Reducer requires state not present in store"),
            Ok(s) => s,
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

pub trait IntoStoreReducer<S, F, A> {
    fn into_store_reducer(self) -> Box<dyn StoreReducer<S>>;
}

impl<S, F, A, R> IntoStoreReducer<S, F, A> for R
where
    S: Field<F>,
    F: Any,
    A: Any,
    R: IntoReducer<F, A>,
    R::Reducer: 'static,
{
    fn into_store_reducer(self) -> Box<dyn StoreReducer<S>> {
        Box::new(self.into_reducer())
    }
}
