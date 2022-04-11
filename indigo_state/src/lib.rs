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
        self.state.reduce(event);
    }
}

pub trait Reducer<E> {
    fn reduce(&mut self, event: E);
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
        fn reduce(&mut self, event: CountEvent) {
            use CountEvent::*;

            match event {
                Increment => self.count += 1,
                Decrement => self.count -= 1,
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
