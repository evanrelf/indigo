use ratatui::{buffer::Buffer, layout::Rect};
use std::{any::Any, sync::Arc};

pub trait AnyMessage: Any + Send {}

impl<T> AnyMessage for T where T: Any + Send {}

impl dyn AnyMessage {
    pub fn downcast<T: AnyMessage>(self: Box<Self>) -> Result<Box<T>, Box<Self>> {
        if self.is::<T>() {
            Ok((self as Box<dyn Any>).downcast::<T>().unwrap())
        } else {
            Err(self)
        }
    }

    pub fn is<T: AnyMessage>(&self) -> bool {
        let this: &dyn Any = self;
        this.is::<T>()
    }
}

pub struct DynMessage(pub Box<dyn AnyMessage>);

impl DynMessage {
    pub fn downcast<T: AnyMessage>(self) -> Result<Box<T>, Self> {
        self.0.downcast().map_err(Self)
    }
}

pub trait Component<State = (), Action = (), Message = DynMessage> {
    fn handle(&self, state: &mut State, message: Message) -> Option<Action>;

    fn render(&self, state: &State, area: Rect, buffer: &mut Buffer);
}

pub struct Text {
    contents: Arc<str>,
}

pub fn text(contents: impl Into<Arc<str>>) -> Text {
    Text {
        contents: contents.into(),
    }
}

impl<State, Action> Component<State, Action> for Text {
    fn handle(&self, state: &mut State, message: DynMessage) -> Option<Action> {
        None
    }

    fn render(&self, state: &State, area: Rect, buffer: &mut Buffer) {
        use ratatui::{text::Text, widgets::Widget as _};
        Text::raw(&*self.contents).render(area, buffer);
    }
}

pub struct Button<F> {
    label: Arc<str>,
    on_click: F,
}

pub fn button<State, Action>(
    label: impl Into<Arc<str>>,
    on_click: impl Fn(&mut State) -> Action,
) -> Button<impl for<'a> Fn(&'a mut State) -> Action> {
    Button {
        label: label.into(),
        on_click,
    }
}

impl<State, Action, F> Component<State, Action> for Button<F>
where
    F: Fn(&mut State) -> Action,
{
    fn handle(&self, state: &mut State, message: DynMessage) -> Option<Action> {
        struct Click; // TODO
        if message.downcast::<Click>().is_ok() {
            let action = (self.on_click)(state);
            Some(action)
        } else {
            None
        }
    }

    fn render(&self, state: &State, area: Rect, buffer: &mut Buffer) {
        let text = text(self.label.clone());
        Component::<_, Action>::render(&text, state, area, buffer);
    }
}

fn example(count: &mut i32) -> impl Component<i32> {
    button("Increment", |count| *count += 1)
}
