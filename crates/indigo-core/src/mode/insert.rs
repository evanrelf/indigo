use crate::{
    editor::Editor,
    event::{Event, KeyEvent},
    key::{KeyCode, is},
    mode::{Mode, normal::enter_normal_mode},
};

#[derive(Clone, Default)]
pub struct State {}

pub enum Action {
    EnterNormalMode,
    DeleteBefore,
    DeleteAfter,
    InsertChar(char),
}

pub fn handle_event_insert(editor: &mut Editor, event: &Event) -> bool {
    let Mode::Insert(_insert_mode) = &editor.mode else {
        panic!("Not in insert mode")
    };

    let mut handled = true;

    match event {
        Event::Key(KeyEvent { key, .. }) => match (key.modifiers, key.code) {
            _ if is(key, "<esc>") => enter_normal_mode(editor),
            _ if is(key, "<bs>") => delete_before(editor),
            _ if is(key, "<del>") => delete_after(editor),
            (m, KeyCode::Char(c)) if m.is_empty() => insert_char(editor, char::from(c)),
            _ if is(key, "<s-\\ >") => insert_char(editor, ' '),
            _ if is(key, "<ret>") => insert_char(editor, '\n'),
            _ if is(key, "<tab>") => insert_char(editor, '\t'),
            _ => handled = false,
        },
    }

    editor.pending_keys.clear();

    handled
}

pub fn enter_insert_mode(editor: &mut Editor) {
    if editor.focused_buffer().text.readonly {
        editor.message = Some(Err(String::from("Buffer is readonly")));
        return;
    }
    let mut window = editor.focused_window_mut();
    window
        .selection_mut()
        .for_each_mut(|mut range| range.reduce());
    window.scroll_to_selection();
    editor.mode = Mode::Insert(State::default());
}

fn delete_before(editor: &mut Editor) {
    let mut window = editor.focused_window_mut();
    window.selection_mut().delete_before();
    window.scroll_to_selection();
    editor.mode.set_count(None);
}

fn delete_after(editor: &mut Editor) {
    let mut window = editor.focused_window_mut();
    window.selection_mut().delete_after();
    window.scroll_to_selection();
    editor.mode.set_count(None);
}

fn insert_char(editor: &mut Editor, char: char) {
    let mut window = editor.focused_window_mut();
    window.selection_mut().insert_char(char);
    window.scroll_to_selection();
    editor.mode.set_count(None);
}

pub fn paste(editor: &mut Editor, text: &str) {
    let mut window = editor.focused_window_mut();
    window.selection_mut().insert(text);
    window.scroll_to_selection();
    editor.mode.set_count(None);
}
