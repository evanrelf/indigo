use std::marker::PhantomData;

pub trait Listener {
    type State: 'static;

    fn listen(&mut self, state: &Self::State);
}

pub trait IntoListener<S> {
    type Listener: 'static + Listener<State = S>;

    fn into_listener(self) -> Self::Listener;
}

pub struct FunctionListener<S, F> {
    function: F,
    marker: PhantomData<fn() -> (S, F)>,
}

impl<S, F> Listener for FunctionListener<S, F>
where
    S: 'static,
    F: FnMut(&S),
{
    type State = S;

    fn listen(&mut self, state: &Self::State) {
        (self.function)(state);
    }
}

impl<S, F> IntoListener<S> for F
where
    S: 'static,
    F: 'static + FnMut(&S),
{
    type Listener = FunctionListener<S, F>;

    fn into_listener(self) -> Self::Listener {
        FunctionListener {
            function: self,
            marker: PhantomData,
        }
    }
}
