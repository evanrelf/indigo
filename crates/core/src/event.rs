use crate::{
    action,
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
pub fn handle_event(editor: &mut Editor, event: &Event) -> bool {
    match editor.mode {
        Mode::Normal(_) => handle_event_normal(editor, event),
        Mode::Insert(_) => handle_event_insert(editor, event),
        Mode::Command(_) => handle_event_command(editor, event),
    }
}

#[tracing::instrument(skip_all)]
pub fn handle_event_normal(editor: &mut Editor, event: &Event) -> bool {
    let Mode::Normal(ref mut normal_mode) = editor.mode else {
        unreachable!()
    };

    let mut update_count = |c: char| {
        let n = usize::from(u8::try_from(c).unwrap() - b'0');
        normal_mode.count = normal_mode
            .count
            .saturating_mul(NonZeroUsize::new(10).unwrap())
            .saturating_add(n);
    };

    match event {
        Event::Key(key) => match (key.modifiers, key.code) {
            (m, KeyCode::Char(c @ '0'..='9')) if m.is_empty() => update_count(c),
            _ if is(key, "<esc>") => action::enter_normal_mode(editor),
            _ if is(key, ":") => action::enter_command_mode(editor),
            _ if is(key, "i") => action::enter_insert_mode(editor),
            // TODO: Add `a` for entering insert mode with the cursor moved to the right.
            _ if is(key, "h") => action::move_left(editor),
            _ if is(key, "l") => action::move_right(editor),
            _ if is(key, "H") => action::extend_left(editor),
            _ if is(key, "L") => action::extend_right(editor),
            _ if is(key, ";") => action::reduce(editor),
            _ if is(key, "<a-;>") => action::flip(editor),
            _ if is(key, "<a-s-;>") => action::flip_forward(editor),
            _ if is(key, "d") => action::delete(editor),
            _ if is(key, "<c-u>") => action::scroll_half_page_up(editor),
            _ if is(key, "<c-d>") => action::scroll_half_page_down(editor),
            _ if is(key, "<c-b>") => action::scroll_full_page_up(editor),
            _ if is(key, "<c-f>") => action::scroll_full_page_down(editor),
            // _ if is(key, "<c-l>") => terminal.clear()?,
            _ if is(key, "<c-c>") => editor.exit = Some(1),
            _ => return false,
        },
    }

    true
}

#[tracing::instrument(skip_all)]
pub fn handle_event_insert(editor: &mut Editor, event: &Event) -> bool {
    let Mode::Insert(ref _insert_mode) = editor.mode else {
        unreachable!()
    };

    match event {
        Event::Key(key) => match (key.modifiers, key.code) {
            _ if is(key, "<esc>") => action::enter_normal_mode(editor),
            _ if is(key, "<bs>") => action::delete_before(editor),
            _ if is(key, "<del>") => action::delete_after(editor),
            (m, KeyCode::Char(c)) if m.is_empty() => action::insert_char(editor, c),
            _ if is(key, "<ret>") => action::insert_char(editor, '\n'),
            _ if is(key, "<tab>") => action::insert_char(editor, '\t'),
            _ if is(key, "<c-u>") => action::scroll_half_page_up(editor),
            _ if is(key, "<c-d>") => action::scroll_half_page_down(editor),
            _ if is(key, "<c-b>") => action::scroll_full_page_up(editor),
            _ if is(key, "<c-f>") => action::scroll_full_page_down(editor),
            // _ if is(key, "<c-l>") => terminal.clear()?,
            _ if is(key, "<c-c>") => editor.exit = Some(1),
            _ => return false,
        },
    }

    true
}

#[tracing::instrument(skip_all)]
pub fn handle_event_command(editor: &mut Editor, event: &Event) -> bool {
    let Mode::Command(ref mut _command_mode) = editor.mode else {
        unreachable!()
    };

    match event {
        Event::Key(key) => match (key.modifiers, key.code) {
            _ if is(key, "<esc>") => action::enter_normal_mode(editor),
            _ if is(key, "<bs>") => action::delete_before(editor),
            _ if is(key, "<ret>") => action::enter_normal_mode(editor), // TODO: Run command
            (m, KeyCode::Char(c)) if m.is_empty() => action::insert_char(editor, c),
            _ if is(key, "<c-c>") => editor.exit = Some(1),
            _ => return false,
        },
    }

    true
}
