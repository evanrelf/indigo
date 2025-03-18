use crate::{
    action::Action,
    editor::Editor,
    key::{Key, KeyCode},
    mode::Mode,
};
use std::{borrow::Cow, num::NonZeroUsize, rc::Rc};

#[cfg(any(feature = "arbitrary", test))]
use arbitrary::Arbitrary;

#[cfg_attr(any(feature = "arbitrary", test), derive(Arbitrary))]
#[derive(Debug)]
pub enum Event {
    Call(Rc<[Action]>),
    KeyInput(Key),
}

impl From<&[Action]> for Event {
    fn from(actions: &[Action]) -> Self {
        Self::Call(Rc::from(actions))
    }
}

impl From<Key> for Event {
    fn from(key: Key) -> Self {
        Self::KeyInput(key)
    }
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
#[must_use]
pub fn handle_event(editor: &mut Editor, event: &Event) -> Rc<[Action]> {
    match editor.mode {
        Mode::Normal(_) => handle_event_normal(editor, event),
        Mode::Insert(_) => handle_event_insert(editor, event),
        Mode::Command(_) => handle_event_command(editor, event),
    }
}

#[tracing::instrument(skip_all)]
pub fn handle_event_normal(editor: &mut Editor, event: &Event) -> Rc<[Action]> {
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
        Event::Call(actions) => Rc::clone(actions),
        Event::KeyInput(key) => match (key.modifiers, key.code) {
            (m, KeyCode::Char(c @ '0'..='9')) if m.is_empty() => {
                Rc::from([Action::UpdateCount(updated_count(c))])
            }
            _ if is(key, "<esc>") => Rc::from([Action::EnterNormalMode]),
            _ if is(key, ":") => Rc::from([Action::EnterCommandMode]),
            _ if is(key, "i") => Rc::from([Action::EnterInsertMode]),
            // TODO: Add `a` for entering insert mode with the cursor moved to the right.
            _ if is(key, "h") => Rc::from([Action::MoveLeft]),
            _ if is(key, "l") => Rc::from([Action::MoveRight]),
            _ if is(key, "H") => Rc::from([Action::ExtendLeft]),
            _ if is(key, "L") => Rc::from([Action::ExtendRight]),
            _ if is(key, ";") => Rc::from([Action::Reduce]),
            _ if is(key, "<a-;>") => Rc::from([Action::Flip]),
            _ if is(key, "<a-s-;>") => Rc::from([Action::FlipForward]),
            _ if is(key, "d") => Rc::from([Action::Delete]),
            _ if is(key, "<c-u>") => Rc::from([Action::ScrollHalfPageUp]),
            _ if is(key, "<c-d>") => Rc::from([Action::ScrollHalfPageDown]),
            _ if is(key, "<c-b>") => Rc::from([Action::ScrollFullPageUp]),
            _ if is(key, "<c-f>") => Rc::from([Action::ScrollFullPageDown]),
            _ if is(key, "<c-c>") => Rc::from([Action::Exit(1)]),
            _ => Rc::from([]),
        },
    }
}

#[tracing::instrument(skip_all)]
pub fn handle_event_insert(editor: &mut Editor, event: &Event) -> Rc<[Action]> {
    let Mode::Insert(ref _insert_mode) = editor.mode else {
        unreachable!()
    };

    match event {
        Event::Call(actions) => Rc::clone(actions),
        Event::KeyInput(key) => match (key.modifiers, key.code) {
            _ if is(key, "<esc>") => Rc::from([Action::EnterNormalMode]),
            _ if is(key, "<bs>") => Rc::from([Action::DeleteBefore]),
            _ if is(key, "<del>") => Rc::from([Action::DeleteAfter]),
            (m, KeyCode::Char(c)) if m.is_empty() => Rc::from([Action::InsertChar(c)]),
            _ if is(key, "<ret>") => Rc::from([Action::InsertChar('\n')]),
            _ if is(key, "<tab>") => Rc::from([Action::InsertChar('\t')]),
            _ if is(key, "<c-u>") => Rc::from([Action::ScrollHalfPageUp]),
            _ if is(key, "<c-d>") => Rc::from([Action::ScrollHalfPageDown]),
            _ if is(key, "<c-b>") => Rc::from([Action::ScrollFullPageUp]),
            _ if is(key, "<c-f>") => Rc::from([Action::ScrollFullPageDown]),
            _ if is(key, "<c-c>") => Rc::from([Action::Exit(1)]),
            _ => Rc::from([]),
        },
    }
}

#[tracing::instrument(skip_all)]
pub fn handle_event_command(editor: &mut Editor, event: &Event) -> Rc<[Action]> {
    let Mode::Command(ref mut command_mode) = editor.mode else {
        unreachable!()
    };

    match event {
        Event::Call(actions) => Rc::clone(actions),
        Event::KeyInput(key) => match (key.modifiers, key.code) {
            _ if is(key, "<esc>") => Rc::from([Action::EnterNormalMode]),
            _ if is(key, "<bs>") => Rc::from([Action::DeleteBefore]),
            _ if is(key, "<ret>") => Rc::from([
                Action::EnterNormalMode,
                Action::RunCommand(Rc::from(Cow::<str>::from(command_mode.text()))),
            ]),
            (m, KeyCode::Char(c)) if m.is_empty() => Rc::from([Action::InsertChar(c)]),
            _ if is(key, "<c-c>") => Rc::from([Action::Exit(1)]),
            _ => Rc::from([]),
        },
    }
}
