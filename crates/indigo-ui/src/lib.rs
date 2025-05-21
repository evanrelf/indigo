mod two;

use std::{any::Any, fmt::Debug, sync::Arc};

pub trait AnyMessage: Any + Debug + Send {}

impl<T> AnyMessage for T where T: Any + Debug + Send {}

pub struct DynMessage(pub Box<dyn AnyMessage>);

pub enum MessageResult<Action, Message = DynMessage> {
    Action(Action),
    Nop,
    Stale(Message),
}

pub trait View<AppState, Action, Message = DynMessage> {
    type Element;

    type ViewState;

    fn build(&self) -> (Self::Element, Self::ViewState);

    fn rebuild(&self, view_state: &mut Self::ViewState, element: &mut Self::Element);

    fn message(
        &self,
        app_state: &mut AppState,
        view_state: &mut Self::ViewState,
        message: Message,
    ) -> MessageResult<Action>;
}

pub struct App<State, Logic> {
    state: State,
    logic: Logic,
}

impl<S, L, V> App<S, L>
where
    L: FnMut(&mut S) -> V,
    V: View<S, ()>,
{
    pub fn new(state: S, logic: L) -> Self {
        Self { state, logic }
    }

    pub fn run(self) {
        todo!()
    }
}

pub struct Text {
    content: Arc<str>,
}

pub fn text(content: impl Into<Arc<str>>) -> Text {
    Text {
        content: content.into(),
    }
}

impl<AppState, Action> View<AppState, Action> for Text {
    type Element = ();

    type ViewState = ();

    fn build(&self) -> (Self::Element, Self::ViewState) {
        todo!()
    }

    fn rebuild(&self, view_state: &mut Self::ViewState, element: &mut Self::Element) {
        todo!()
    }

    fn message(
        &self,
        app_state: &mut AppState,
        view_state: &mut Self::ViewState,
        message: DynMessage,
    ) -> MessageResult<Action> {
        todo!()
    }
}

fn example() {
    let state = String::from("world");
    let logic = |name: &mut String| text(format!("Hello, {name}!"));
    let app = App::new(state, logic);
    app.run();
}
