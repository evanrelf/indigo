pub struct Store<'store, State, Event> {
    state: State,
    reducer: Box<dyn 'store + Fn(&State, Event) -> State>,
}

impl<'store, State, Event> Store<'store, State, Event> {
    pub fn new<F>(reducer: F) -> Self
    where
        State: Default,
        F: 'store + Fn(&State, Event) -> State,
    {
        Self {
            state: State::default(),
            reducer: Box::new(reducer),
        }
    }

    pub fn get_state(&self) -> &State {
        &self.state
    }

    pub fn dispatch(&mut self, event: Event) {
        self.state = (self.reducer)(&self.state, event);
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[derive(Debug, Default)]
    struct State {
        count: isize,
    }

    enum Event {
        Incremented,
        Decremented,
    }

    fn reducer(state: &State, event: Event) -> State {
        match event {
            Event::Incremented => State {
                count: state.count + 1,
            },
            Event::Decremented => State {
                count: state.count - 1,
            },
        }
    }

    #[test]
    fn main() {
        let mut store = Store::new(reducer);

        store.dispatch(Event::Incremented);
        store.dispatch(Event::Incremented);
        store.dispatch(Event::Decremented);

        assert_eq!(store.get_state().count, 1);
    }
}
