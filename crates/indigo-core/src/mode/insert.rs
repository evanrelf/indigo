use crate::{
    action::{
        enter_normal_mode, exit, scroll_full_page_down, scroll_full_page_up, scroll_half_page_down,
        scroll_half_page_up,
    },
    editor::Editor,
    event::{Event, KeyEvent},
    key::{KeyCode, is},
    mode::Mode,
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
            (m, KeyCode::Char(c)) if m.is_empty() => insert_char(editor, c),
            _ if is(key, "<ret>") => insert_char(editor, '\n'),
            _ if is(key, "<tab>") => insert_char(editor, '\t'),
            _ if is(key, "<c-u>") => scroll_half_page_up(editor),
            _ if is(key, "<c-d>") => scroll_half_page_down(editor),
            _ if is(key, "<c-b>") => scroll_full_page_up(editor),
            _ if is(key, "<c-f>") => scroll_full_page_down(editor),
            _ if is(key, "<c-c>") => exit(editor, 1),
            _ => handled = false,
        },
    }

    handled
}

pub fn enter_insert_mode(editor: &mut Editor) {
    let mut range = editor.buffer.range_mut();
    range.reduce();
    editor.mode = Mode::Insert(InsertMode::default());
}

fn delete_before(editor: &mut Editor) {
    let mut range = editor.buffer.range_mut();
    range.delete_before();
    editor.mode.set_count(None);
}

fn delete_after(editor: &mut Editor) {
    let mut range = editor.buffer.range_mut();
    range.delete_after();
    editor.mode.set_count(None);
}

fn insert_char(editor: &mut Editor, char: char) {
    let mut range = editor.buffer.range_mut();
    range.insert_char(char);
    editor.mode.set_count(None);
}
