use crate::type_map::TypeMap;

#[derive(Default)]
pub struct Store {
    state: TypeMap,
}

impl Store {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn add_state<S>(&mut self, state: S)
    where
        S: 'static,
    {
        self.state.insert(state);
    }
}
