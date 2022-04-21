use crate::type_map::TypeMap;
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

pub trait StoreReducer<S = TypeMap> {
    fn reduce(&self, state: &mut S, action: &dyn Any) -> bool;

    fn state_type_id(&self) -> TypeId;
}

impl<R> StoreReducer for R
where
    R: 'static + Reducer,
{
    fn reduce(&self, state: &mut TypeMap, action: &dyn Any) -> bool {
        let state = match state
            .get_mut(&TypeId::of::<R::State>())
            // `unwrap` is safe because `add_state` uses the value's type ID as the key
            .map(|b| b.downcast_mut().unwrap())
        {
            None => panic!("Reducer requires state not present in store"),
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
}
