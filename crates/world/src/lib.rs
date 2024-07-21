use std::{
    any::{Any, TypeId},
    collections::HashMap,
};

#[derive(Default)]
pub struct World {
    state: HashMap<TypeId, Box<dyn Any>>,
}

impl World {
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    pub fn insert_state<S: State>(&mut self, state: S) {
        self.state.insert(state.type_id(), Box::new(state));
    }

    #[must_use]
    pub fn get_state<S: State>(&self) -> Option<&S> {
        self.state
            .get(&TypeId::of::<S>())
            .map(|s| s.downcast_ref().unwrap())
    }

    #[must_use]
    pub fn get_state_mut<S: State>(&mut self) -> Option<&mut S> {
        self.state
            .get_mut(&TypeId::of::<S>())
            .map(|s| s.downcast_mut().unwrap())
    }

    #[must_use]
    pub fn remove_state<S: State>(&mut self) -> Option<S> {
        self.state
            .remove(&TypeId::of::<S>())
            .map(|s| *s.downcast().unwrap())
    }
}

pub trait State: Any {}

impl<T: Any> State for T {}
