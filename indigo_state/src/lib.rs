#[derive(Default)]
pub struct Store<S> {
    state: S,
}

impl<S> Store<S> {
    pub fn new() -> Self
    where
        S: Default,
    {
        Self::default()
    }

    pub fn state(&self) -> &S {
        &self.state
    }

    pub fn dispatch<E>(&mut self, event: E)
    where
        S: Reducer<E>,
    {
        self.state = self.state.reduce(event);
    }
}

pub trait Reducer<E> {
    #[must_use]
    fn reduce(&self, event: E) -> Self;
}

#[cfg(test)]
mod test {
    use super::*;

    #[derive(Default)]
    struct State {
        count: Count,
    }

    #[derive(Debug, Default, PartialEq)]
    struct Count(isize);

    enum CountEvent {
        Increment,
        Decrement,
    }

    impl Reducer<CountEvent> for State {
        fn reduce(&self, event: CountEvent) -> Self {
            Self {
                count: self.count.reduce(event),
            }
        }
    }

    impl Reducer<CountEvent> for Count {
        fn reduce(&self, event: CountEvent) -> Self {
            use CountEvent::*;

            match event {
                Increment => Self(self.0 + 1),
                Decrement => Self(self.0 - 1),
            }
        }
    }

    #[test]
    fn main() {
        let mut store: Store<State> = Store::new();

        store.dispatch(CountEvent::Increment);
        store.dispatch(CountEvent::Increment);
        store.dispatch(CountEvent::Decrement);

        assert_eq!(store.state().count, Count(1));
    }
}
