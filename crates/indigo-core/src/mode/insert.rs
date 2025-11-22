use crate::{
    editor::Editor,
    event::{Event, KeyEvent},
    key::{KeyCode, is},
    mode::{Mode, normal::enter_normal_mode},
    window::{
        scroll_full_page_down, scroll_full_page_up, scroll_half_page_down, scroll_half_page_up,
    },
};

#[derive(Default)]
pub struct InsertMode {}

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
            _ if is(key, "<ret>") => insert_char(editor, '\n'),
            _ if is(key, "<tab>") => insert_char(editor, '\t'),
            _ if is(key, "<c-u>") => scroll_half_page_up(editor),
            _ if is(key, "<c-d>") => scroll_half_page_down(editor),
            _ if is(key, "<c-b>") => scroll_full_page_up(editor),
            _ if is(key, "<c-f>") => scroll_full_page_down(editor),
            _ if is(key, "<c-c>") => editor.exit(1),
            _ => handled = false,
        },
    }

    handled
}

pub fn enter_insert_mode(editor: &mut Editor) {
    editor
        .buffer
        .selection_mut()
        .for_each_mut(|mut range| range.reduce());
    editor.mode = Mode::Insert(InsertMode::default());
}

fn delete_before(editor: &mut Editor) {
    editor.buffer.selection_mut().for_each_mut(|mut range| {
        range.delete_before();
    });
    editor.mode.set_count(None);
}

fn delete_after(editor: &mut Editor) {
    editor.buffer.selection_mut().for_each_mut(|mut range| {
        range.delete_after();
    });
    editor.mode.set_count(None);
}

fn insert_char(editor: &mut Editor, char: char) {
    editor.buffer.selection_mut().for_each_mut(|mut range| {
        range.insert_char(char);
    });
    editor.mode.set_count(None);
}

pub fn paste(editor: &mut Editor, text: &str) {
    editor.buffer.selection_mut().for_each_mut(|mut range| {
        range.insert(text);
    });
    editor.mode.set_count(None);
}
