#[derive(Default)]
pub struct Store<State> {
    state: State,
}

impl<State> Store<State> {
    pub fn new() -> Self
    where
        State: Default,
    {
        Self::default()
    }

    pub fn state(&self) -> &State {
        &self.state
    }

    pub fn dispatch<Event>(&mut self, event: Event)
    where
        State: Reducer<Event>,
    {
        self.state = self.state.reduce(event);
    }
}

pub trait Reducer<Event> {
    #[must_use]
    fn reduce(&self, event: Event) -> Self;
}

#[cfg(test)]
mod test {
    use super::*;

    #[derive(Default)]
    struct State {
        count: isize,
    }

    enum CountEvent {
        Increment,
        Decrement,
    }

    impl Reducer<CountEvent> for State {
        fn reduce(&self, event: CountEvent) -> Self {
            use CountEvent::*;

            match event {
                Increment => Self {
                    count: self.count + 1,
                },
                Decrement => Self {
                    count: self.count - 1,
                },
            }
        }
    }

    #[test]
    fn main() {
        let mut store: Store<State> = Store::new();

        store.dispatch(CountEvent::Increment);
        store.dispatch(CountEvent::Increment);
        store.dispatch(CountEvent::Decrement);

        assert_eq!(store.state().count, 1);
    }
}
