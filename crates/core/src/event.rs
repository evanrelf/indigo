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
pub fn handle_event(editor: &mut Editor, event: &Event) -> Vec<Action> {
    match editor.mode {
        Mode::Normal(_) => handle_event_normal(editor, event),
        Mode::Insert(_) => handle_event_insert(editor, event),
        Mode::Command(_) => handle_event_command(editor, event),
    }
}

#[tracing::instrument(skip_all)]
pub fn handle_event_normal(editor: &mut Editor, event: &Event) -> Vec<Action> {
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
                vec![Action::UpdateCount(updated_count(c))]
            }
            _ if is(key, "<esc>") => vec![Action::EnterNormalMode],
            _ if is(key, ":") => vec![Action::EnterCommandMode],
            _ if is(key, "i") => vec![Action::EnterInsertMode],
            // TODO: Add `a` for entering insert mode with the cursor moved to the right.
            _ if is(key, "h") => vec![Action::MoveLeft],
            _ if is(key, "l") => vec![Action::MoveRight],
            _ if is(key, "H") => vec![Action::ExtendLeft],
            _ if is(key, "L") => vec![Action::ExtendRight],
            _ if is(key, ";") => vec![Action::Reduce],
            _ if is(key, "<a-;>") => vec![Action::Flip],
            _ if is(key, "<a-s-;>") => vec![Action::FlipForward],
            _ if is(key, "d") => vec![Action::Delete],
            _ if is(key, "<c-u>") => vec![Action::ScrollHalfPageUp],
            _ if is(key, "<c-d>") => vec![Action::ScrollHalfPageDown],
            _ if is(key, "<c-b>") => vec![Action::ScrollFullPageUp],
            _ if is(key, "<c-f>") => vec![Action::ScrollFullPageDown],
            _ if is(key, "<c-c>") => vec![Action::Exit(1)],
            _ => vec![],
        },
    }
}

#[tracing::instrument(skip_all)]
pub fn handle_event_insert(editor: &mut Editor, event: &Event) -> Vec<Action> {
    let Mode::Insert(ref _insert_mode) = editor.mode else {
        unreachable!()
    };

    match event {
        Event::Key(key) => match (key.modifiers, key.code) {
            _ if is(key, "<esc>") => vec![Action::EnterNormalMode],
            _ if is(key, "<bs>") => vec![Action::DeleteBefore],
            _ if is(key, "<del>") => vec![Action::DeleteAfter],
            (m, KeyCode::Char(c)) if m.is_empty() => vec![Action::InsertChar(c)],
            _ if is(key, "<ret>") => vec![Action::InsertChar('\n')],
            _ if is(key, "<tab>") => vec![Action::InsertChar('\t')],
            _ if is(key, "<c-u>") => vec![Action::ScrollHalfPageUp],
            _ if is(key, "<c-d>") => vec![Action::ScrollHalfPageDown],
            _ if is(key, "<c-b>") => vec![Action::ScrollFullPageUp],
            _ if is(key, "<c-f>") => vec![Action::ScrollFullPageDown],
            _ if is(key, "<c-c>") => vec![Action::Exit(1)],
            _ => vec![],
        },
    }
}

#[tracing::instrument(skip_all)]
pub fn handle_event_command(editor: &mut Editor, event: &Event) -> Vec<Action> {
    let Mode::Command(ref mut command_mode) = editor.mode else {
        unreachable!()
    };

    match event {
        Event::Key(key) => match (key.modifiers, key.code) {
            _ if is(key, "<esc>") => vec![Action::EnterNormalMode],
            _ if is(key, "<bs>") => vec![Action::DeleteBefore],
            _ if is(key, "<ret>") => vec![
                Action::EnterNormalMode,
                Action::RunCommand(command_mode.rope().to_string()),
            ],
            (m, KeyCode::Char(c)) if m.is_empty() => vec![Action::InsertChar(c)],
            _ if is(key, "<c-c>") => vec![Action::Exit(1)],
            _ => vec![],
        },
    }
}
