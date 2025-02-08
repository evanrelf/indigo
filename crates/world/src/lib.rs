// TODO: Remove
#![expect(clippy::needless_pass_by_value)]
#![expect(unused_variables)]

use downcast_rs::{impl_downcast, Downcast};
use std::{
    any::{Any, TypeId},
    collections::HashMap,
};

#[derive(Default)]
pub struct World {
    state: HashMap<TypeId, Box<dyn State>>,
    hooks: HashMap<TypeId, HashMap<usize, Box<dyn Hook>>>,
    hook_id: usize,
}

impl World {
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    pub fn insert_state(&mut self, state: impl State) {
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
            .map(|s| *s.downcast().unwrap_or_else(|_| unreachable!()))
    }

    pub fn insert_hook(&mut self, hook: impl Hook) -> HookId {
        todo!();
    }

    pub fn trigger_hooks(&self, event: impl Event) {
        todo!()
    }

    pub fn remove_hook(&mut self, hook_id: HookId) {
        todo!()
    }
}

pub trait State: Downcast {}

impl_downcast!(State);

impl<T: Any> State for T {}

pub trait Event: Downcast {}

impl_downcast!(Event);

impl<T: Any> Event for T {}

pub trait Hook {}

#[derive(Clone, Copy)]
pub struct HookId {
    event: TypeId,
    hook: usize,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_state() {
        let mut world = World::new();
        world.insert_state(0u8);
        assert_eq!(world.get_state::<u8>(), Some(&0));
        world.insert_state(1u8);
        assert_eq!(world.get_state::<u8>(), Some(&1));
        assert_eq!(world.get_state_mut::<u8>(), Some(&mut 1));
        assert_eq!(world.remove_state::<u8>(), Some(1));
        assert_eq!(world.remove_state::<u8>(), None);
        assert_eq!(world.get_state::<u8>(), None);
    }
}
