#[derive(Default)]
pub struct Store<'store, S> {
    state: S,
    listeners: Vec<Box<dyn 'store + Fn(&S)>>,
}

impl<'store, S> Store<'store, S> {
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

        for listener in &self.listeners {
            listener(&self.state);
        }
    }

    pub fn subscribe<F>(&mut self, listener: F)
    where
        F: 'store + Fn(&S),
    {
        self.listeners.push(Box::new(listener));
    }
}

pub trait Reducer<E> {
    fn reduce(&mut self, event: E);
}

#[cfg(test)]
mod test {
    use super::*;

    #[derive(Debug, Default)]
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

        store.subscribe(|state| {
            println!("{:?}", state);
        });

        store.dispatch(CountEvent::Increment);
        store.dispatch(CountEvent::Increment);
        store.dispatch(CountEvent::Decrement);

        assert_eq!(store.state().count, 1);
    }
}
