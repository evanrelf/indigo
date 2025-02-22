use crate::{
    actions,
    editor::Editor,
    key::{Key, KeyCode, KeyModifier},
    mode::Mode,
};
use std::num::NonZeroUsize;

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
    use KeyCode as C;
    use KeyModifier as M;

    let Mode::Normal(ref mut normal_mode) = editor.mode else {
        unreachable!()
    };

    match event {
        Event::Key(key) => match (key.modifiers, key.code) {
            (m, C::Char(c @ ('0'..='9'))) if m.is_empty() => {
                let n = usize::from(u8::try_from(c).unwrap() - b'0');
                normal_mode.count = normal_mode
                    .count
                    .saturating_mul(NonZeroUsize::new(10).unwrap())
                    .saturating_add(n);
            }
            (m, C::Escape) if m.is_empty() => actions::enter_normal_mode(editor),
            (m, C::Char('i')) if m.is_empty() => actions::enter_insert_mode(editor),
            // TODO: Add `a` for entering insert mode with the cursor moved to the right.
            (m, C::Char('h')) if m.is_empty() => actions::move_left(editor),
            (m, C::Char('l')) if m.is_empty() => actions::move_right(editor),
            (m, C::Char('h' | 'H')) if m == M::Shift => actions::extend_left(editor),
            (m, C::Char('l' | 'L')) if m == M::Shift => actions::extend_right(editor),
            (m, C::Char(';')) if m.is_empty() => actions::reduce(editor),
            (m, C::Char(';')) if m == M::Alt => actions::flip(editor),
            (m, C::Char(';')) if m == M::Alt | M::Shift => actions::flip_forward(editor),
            (m, C::Char('d')) if m.is_empty() => actions::delete(editor),
            (m, C::Char('u')) if m == M::Control => actions::scroll_half_page_up(editor),
            (m, C::Char('d')) if m == M::Control => actions::scroll_half_page_down(editor),
            (m, C::Char('b')) if m == M::Control => actions::scroll_full_page_up(editor),
            (m, C::Char('f')) if m == M::Control => actions::scroll_full_page_down(editor),
            // (m, C::Char('l')) if m == M::Control => terminal.clear()?,
            (m, C::Char('c')) if m == M::Control => editor.quit = true,
            _ => {}
        },
    }
}

pub fn handle_event_insert(editor: &mut Editor, event: &Event) {
    use KeyCode as C;
    use KeyModifier as M;

    let Mode::Insert = editor.mode else {
        unreachable!()
    };

    match event {
        Event::Key(key) => match (key.modifiers, key.code) {
            (m, C::Escape) if m.is_empty() => actions::enter_normal_mode(editor),
            (m, C::Backspace) if m.is_empty() => actions::delete_before(editor),
            (m, C::Delete) if m.is_empty() => actions::delete_after(editor),
            (m, C::Char(c)) if m.is_empty() => actions::insert_char(editor, c),
            (m, C::Return) if m.is_empty() => actions::insert_char(editor, '\n'),
            (m, C::Tab) if m.is_empty() => actions::insert_char(editor, '\t'),
            (m, C::Char('u')) if m == M::Control => actions::scroll_half_page_up(editor),
            (m, C::Char('d')) if m == M::Control => actions::scroll_half_page_down(editor),
            (m, C::Char('b')) if m == M::Control => actions::scroll_full_page_up(editor),
            (m, C::Char('f')) if m == M::Control => actions::scroll_full_page_down(editor),
            // (m, C::Char('l')) if m == M::Control => terminal.clear()?,
            (m, C::Char('c')) if m == M::Control => editor.quit = true,
            _ => {}
        },
    }
}
