use crate::{
    action::Action,
    editor::Editor,
    key::{Key, KeyCode},
    mode::Mode,
};
use std::num::NonZeroUsize;

#[cfg(any(feature = "arbitrary", test))]
use arbitrary::Arbitrary;

#[cfg_attr(any(feature = "arbitrary", test), derive(Arbitrary))]
#[derive(Debug)]
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

#[expect(clippy::trivially_copy_pass_by_ref)]
#[must_use]
fn is(x: &Key, y: &str) -> bool {
    let mut x = *x;
    let mut y = y.parse::<Key>().unwrap();
    x.normalize();
    y.normalize();
    x == y
}

#[tracing::instrument(skip_all)]
pub fn handle_event(editor: &mut Editor, event: &Event) -> Option<Action> {
    match editor.mode {
        Mode::Normal(_) => handle_event_normal(editor, event),
        Mode::Insert(_) => handle_event_insert(editor, event),
        Mode::Command(_) => handle_event_command(editor, event),
    }
}

#[tracing::instrument(skip_all)]
pub fn handle_event_normal(editor: &mut Editor, event: &Event) -> Option<Action> {
    let Mode::Normal(ref mut normal_mode) = editor.mode else {
        unreachable!()
    };

    let updated_count = |c: char| {
        let n = usize::from(u8::try_from(c).unwrap() - b'0');
        normal_mode
            .count
            .saturating_mul(NonZeroUsize::new(10).unwrap())
            .saturating_add(n)
    };

    match event {
        Event::Key(key) => match (key.modifiers, key.code) {
            (m, KeyCode::Char(c @ '0'..='9')) if m.is_empty() => {
                Some(Action::UpdateCount(updated_count(c)))
            }
            _ if is(key, "<esc>") => Some(Action::EnterNormalMode),
            _ if is(key, ":") => Some(Action::EnterCommandMode),
            _ if is(key, "i") => Some(Action::EnterInsertMode),
            // TODO: Add `a` for entering insert mode with the cursor moved to the right.
            _ if is(key, "h") => Some(Action::MoveLeft),
            _ if is(key, "l") => Some(Action::MoveRight),
            _ if is(key, "H") => Some(Action::ExtendLeft),
            _ if is(key, "L") => Some(Action::ExtendRight),
            _ if is(key, ";") => Some(Action::Reduce),
            _ if is(key, "<a-;>") => Some(Action::Flip),
            _ if is(key, "<a-s-;>") => Some(Action::FlipForward),
            _ if is(key, "d") => Some(Action::Delete),
            _ if is(key, "<c-u>") => Some(Action::ScrollHalfPageUp),
            _ if is(key, "<c-d>") => Some(Action::ScrollHalfPageDown),
            _ if is(key, "<c-b>") => Some(Action::ScrollFullPageUp),
            _ if is(key, "<c-f>") => Some(Action::ScrollFullPageDown),
            _ if is(key, "<c-c>") => Some(Action::Exit(1)),
            _ => None,
        },
    }
}

#[tracing::instrument(skip_all)]
pub fn handle_event_insert(editor: &mut Editor, event: &Event) -> Option<Action> {
    let Mode::Insert(ref _insert_mode) = editor.mode else {
        unreachable!()
    };

    match event {
        Event::Key(key) => match (key.modifiers, key.code) {
            _ if is(key, "<esc>") => Some(Action::EnterNormalMode),
            _ if is(key, "<bs>") => Some(Action::DeleteBefore),
            _ if is(key, "<del>") => Some(Action::DeleteAfter),
            (m, KeyCode::Char(c)) if m.is_empty() => Some(Action::InsertChar(c)),
            _ if is(key, "<ret>") => Some(Action::InsertChar('\n')),
            _ if is(key, "<tab>") => Some(Action::InsertChar('\t')),
            _ if is(key, "<c-u>") => Some(Action::ScrollHalfPageUp),
            _ if is(key, "<c-d>") => Some(Action::ScrollHalfPageDown),
            _ if is(key, "<c-b>") => Some(Action::ScrollFullPageUp),
            _ if is(key, "<c-f>") => Some(Action::ScrollFullPageDown),
            _ if is(key, "<c-c>") => Some(Action::Exit(1)),
            _ => None,
        },
    }
}

#[tracing::instrument(skip_all)]
pub fn handle_event_command(editor: &mut Editor, event: &Event) -> Option<Action> {
    let Mode::Command(ref mut _command_mode) = editor.mode else {
        unreachable!()
    };

    match event {
        Event::Key(key) => match (key.modifiers, key.code) {
            _ if is(key, "<esc>") => Some(Action::EnterNormalMode),
            _ if is(key, "<bs>") => Some(Action::DeleteBefore),
            _ if is(key, "<ret>") => Some(Action::EnterNormalMode),
            (m, KeyCode::Char(c)) if m.is_empty() => Some(Action::InsertChar(c)),
            _ if is(key, "<c-c>") => Some(Action::Exit(1)),
            _ => None,
        },
    }
}
