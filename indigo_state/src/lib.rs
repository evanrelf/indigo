pub struct Store<'store, State, Action> {
    state: State,
    reducer: Box<dyn 'store + Fn(&State, Action) -> State>,
}

impl<'store, State, Action> Store<'store, State, Action> {
    pub fn new<F>(reducer: F) -> Self
    where
        State: Default,
        F: 'store + Fn(&State, Action) -> State,
    {
        Self {
            state: State::default(),
            reducer: Box::new(reducer),
        }
    }

    pub fn get_state(&self) -> &State {
        &self.state
    }

    pub fn dispatch(&mut self, action: Action) {
        self.state = (self.reducer)(&self.state, action);
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[derive(Debug, Default)]
    struct State {
        count: isize,
    }

    enum Action {
        Incremented,
        Decremented,
    }

    fn reducer(state: &State, action: Action) -> State {
        match action {
            Action::Incremented => State {
                count: state.count + 1,
            },
            Action::Decremented => State {
                count: state.count - 1,
            },
        }
    }

    #[test]
    fn main() {
        let mut store = Store::new(reducer);

        store.dispatch(Action::Incremented);
        store.dispatch(Action::Incremented);
        store.dispatch(Action::Decremented);

        assert_eq!(store.get_state().count, 1);
    }
}
