use crate::{editor::Editor, key::Key, mode::Mode};

pub enum Event {
    Key(Key),
}

impl Event {
    #[must_use]
    pub fn kind(&self) -> EventKind {
        match self {
            Self::Key(_) => EventKind::Key,
        }
    }
}

#[derive(Eq, Hash, PartialEq)]
pub enum EventKind {
    Key,
}

pub fn handle_event(editor: &mut Editor, event: &Event) {
    match editor.mode {
        Mode::Normal(_) => handle_event_normal(editor, event),
        Mode::Insert => handle_event_insert(editor, event),
    }
}

pub fn handle_event_normal(editor: &mut Editor, event: &Event) {
    let Mode::Normal(ref mut normal_mode) = editor.mode else {
        unreachable!()
    };

    match event {
        Event::Key(key) => match key {
            _ => {}
        },
    }
}

pub fn handle_event_insert(editor: &mut Editor, event: &Event) {
    let Mode::Insert = editor.mode else {
        unreachable!()
    };

    match event {
        Event::Key(key) => match key {
            _ => {}
        },
    }
}
