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

pub trait View<State = (), Action = (), Message = DynMessage> {
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

impl<State, Action> View<State, Action> for Text {
    fn handle(&self, _state: &mut State, _message: DynMessage) -> Option<Action> {
        None
    }

    fn render(&self, _state: &State, area: Rect, buffer: &mut Buffer) {
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

impl<State, Action, F> View<State, Action> for Button<F>
where
    F: Fn(&mut State) -> Action,
{
    fn handle(&self, state: &mut State, message: DynMessage) -> Option<Action> {
        use crossterm::event::{Event, KeyCode, MouseEventKind};
        if let Ok(event) = message.downcast::<Event>() {
            match *event {
                Event::Mouse(mouse_event)
                    if matches!(mouse_event.kind, MouseEventKind::Down(_)) =>
                {
                    let action = (self.on_click)(state);
                    Some(action)
                }
                Event::Key(key_event) if matches!(key_event.code, KeyCode::Enter) => {
                    let action = (self.on_click)(state);
                    Some(action)
                }
                _ => None,
            }
        } else {
            None
        }
    }

    fn render(&self, state: &State, area: Rect, buffer: &mut Buffer) {
        let text = text(self.label.clone());
        View::<_, Action>::render(&text, state, area, buffer);
    }
}

pub struct App<State, Logic> {
    state: State,
    logic: Logic,
    restore_on_drop: bool,
}

impl<S, L, C> App<S, L>
where
    L: FnMut(&mut S) -> C,
    C: View<S>,
{
    pub fn new(state: S, logic: L) -> Self {
        Self {
            state,
            logic,
            restore_on_drop: false,
        }
    }

    pub fn run(mut self) {
        use crossterm;
        let mut terminal = ratatui::init();
        self.restore_on_drop = true;
        let mut view = (self.logic)(&mut self.state);
        terminal
            .draw(|frame| {
                view.render(&self.state, frame.area(), frame.buffer_mut());
            })
            .unwrap();
        loop {
            let event = crossterm::event::read().unwrap();
            let message = DynMessage(Box::new(event));
            view.handle(&mut self.state, message);
            view = (self.logic)(&mut self.state);
            terminal
                .draw(|frame| {
                    view.render(&self.state, frame.area(), frame.buffer_mut());
                })
                .unwrap();
        }
    }
}

impl<State, Logic> Drop for App<State, Logic> {
    fn drop(&mut self) {
        if self.restore_on_drop {
            ratatui::restore();
        }
    }
}
