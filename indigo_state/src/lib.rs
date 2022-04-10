#![allow(clippy::bool_assert_comparison)]
#![allow(clippy::new_without_default)]

pub struct Store<State> {
    state: State,
}

impl<State> Store<State> {
    pub fn new() -> Self
    where
        State: Default,
    {
        Self {
            state: State::default(),
        }
    }

    pub fn get_state(&self) -> &State {
        &self.state
    }

    pub fn dispatch(&mut self, event: impl Event<State>) {
        self.state = Event::<State>::reduce(&self.state, event);
    }
}

pub trait Event<State> {
    fn reduce(state: &State, event: Self) -> State;
}

#[cfg(test)]
mod test {
    use super::*;

    #[derive(Clone, Debug, Default)]
    struct State {
        light_switch: bool,
        count: isize,
    }

    enum LightSwitchEvent {
        TurnOn,
        TurnOff,
    }

    impl Event<State> for LightSwitchEvent {
        fn reduce(state: &State, event: Self) -> State {
            let mut state = state.clone();

            match event {
                Self::TurnOn => state.light_switch = true,
                Self::TurnOff => state.light_switch = false,
            }

            state
        }
    }

    enum CounterEvent {
        Incremented,
        Decremented,
    }

    impl Event<State> for CounterEvent {
        fn reduce(state: &State, event: Self) -> State {
            match event {
                Self::Incremented => State {
                    count: state.count + 1,
                    ..state.clone()
                },
                Self::Decremented => State {
                    count: state.count - 1,
                    ..state.clone()
                },
            }
        }
    }

    #[test]
    fn main() {
        let mut store = Store::new();

        store.dispatch(LightSwitchEvent::TurnOn);
        store.dispatch(LightSwitchEvent::TurnOff);
        store.dispatch(LightSwitchEvent::TurnOn);

        assert_eq!(store.get_state().light_switch, true);

        store.dispatch(CounterEvent::Incremented);
        store.dispatch(CounterEvent::Incremented);
        store.dispatch(CounterEvent::Decremented);

        assert_eq!(store.get_state().count, 1);
    }
}
