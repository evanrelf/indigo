pub mod view;

use crate::view::Context;

pub struct App<State, Logic> {
    state: State,
    logic: Logic,
}

impl<State, Logic, View> App<State, Logic>
where
    Logic: FnMut(&mut State) -> View,
    View: xilem_core::View<State, (), Context>,
{
    pub fn new(state: State, logic: Logic) -> Self {
        Self { state, logic }
    }

    pub fn run(self) -> anyhow::Result<()> {
        todo!()
    }
}
