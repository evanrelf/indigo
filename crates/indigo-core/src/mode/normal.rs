use crate::{
    editor::Editor,
    event::{Event, KeyEvent},
    key::{KeyCode, is},
    mode::{
        Mode, command::enter_command_mode, goto::enter_goto_mode, insert::InsertMode,
        insert::enter_insert_mode, seek::enter_seek_mode,
    },
    window::{
        scroll_full_page_down, scroll_full_page_up, scroll_half_page_down, scroll_half_page_up,
    },
};
use std::num::NonZeroUsize;

#[derive(Default)]
pub struct NormalMode {
    pub count: Option<NonZeroUsize>,
}

pub fn handle_event_normal(editor: &mut Editor, event: &Event) -> bool {
    use crate::mode::seek::{
        SeekDirection::{Next, Prev},
        SeekInclude::{Onto, Until},
        SeekSelect::{Extend, Move},
    };

    let mut handled = true;

    let count = |c: u8| {
        let digit = usize::from(c - b'0');
        let current = editor.mode.count().map_or(0, |count| usize::from(count));
        NonZeroUsize::new(current.saturating_mul(10).saturating_add(digit))
    };

    match event {
        Event::Key(KeyEvent { key, .. }) => match (key.modifiers, key.code) {
            (m, KeyCode::Char(c @ b'0'..=b'9')) if m.is_empty() => set_count(editor, count(c)),
            _ if is(key, "<esc>") => enter_normal_mode(editor),
            _ if is(key, ":") => enter_command_mode(editor),
            _ if is(key, "i") => enter_insert_mode(editor),
            _ if is(key, "I") => insert_at_line_non_blank_start(editor),
            _ if is(key, "a") => {
                move_right(editor);
                enter_insert_mode(editor);
            }
            _ if is(key, "h") => move_left(editor),
            _ if is(key, "l") => move_right(editor),
            _ if is(key, "k") => move_up(editor),
            _ if is(key, "j") => move_down(editor),
            _ if is(key, "H") => extend_left(editor),
            _ if is(key, "L") => extend_right(editor),
            _ if is(key, "K") => extend_up(editor),
            _ if is(key, "J") => extend_down(editor),
            _ if is(key, "<a-t>") => enter_seek_mode(editor, Move, Until, Prev),
            _ if is(key, "<a-T>") => enter_seek_mode(editor, Extend, Until, Prev),
            _ if is(key, "t") => enter_seek_mode(editor, Move, Until, Next),
            _ if is(key, "T") => enter_seek_mode(editor, Extend, Until, Next),
            _ if is(key, "<a-f>") => enter_seek_mode(editor, Move, Onto, Prev),
            _ if is(key, "<a-F>") => enter_seek_mode(editor, Extend, Onto, Prev),
            _ if is(key, "f") => enter_seek_mode(editor, Move, Onto, Next),
            _ if is(key, "F") => enter_seek_mode(editor, Extend, Onto, Next),
            _ if is(key, "g") => enter_goto_mode(editor),
            _ if is(key, ";") => reduce(editor),
            _ if is(key, "<a-;>") => flip(editor),
            _ if is(key, "<a-:>") => flip_forward(editor),
            _ if is(key, "%") => select_all(editor),
            _ if is(key, "d") => delete(editor),
            _ if is(key, "c") => {
                delete(editor);
                enter_insert_mode(editor);
            }
            _ if is(key, "u") => undo(editor),
            _ if is(key, "U") => redo(editor),
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

pub fn enter_normal_mode(editor: &mut Editor) {
    editor.window_mut().buffer_mut().commit();
    editor.mode = Mode::Normal(NormalMode::default());
}

fn set_count(editor: &mut Editor, count: Option<NonZeroUsize>) {
    editor.mode.set_count(count);
}

fn extend_left(editor: &mut Editor) {
    let count = editor.mode.count().unwrap_or(NonZeroUsize::MIN).get();
    editor
        .window_mut()
        .buffer_mut()
        .selection_mut()
        .for_each_mut(|mut range| range.extend_left(count));
    editor.mode.set_count(None);
}

fn move_left(editor: &mut Editor) {
    let count = editor.mode.count().unwrap_or(NonZeroUsize::MIN).get();
    editor
        .window_mut()
        .buffer_mut()
        .selection_mut()
        .for_each_mut(|mut range| range.move_left(count));
    editor.mode.set_count(None);
}

fn extend_right(editor: &mut Editor) {
    let count = editor.mode.count().unwrap_or(NonZeroUsize::MIN).get();
    editor
        .window_mut()
        .buffer_mut()
        .selection_mut()
        .for_each_mut(|mut range| range.extend_right(count));
    editor.mode.set_count(None);
}

fn move_right(editor: &mut Editor) {
    let count = editor.mode.count().unwrap_or(NonZeroUsize::MIN).get();
    editor
        .window_mut()
        .buffer_mut()
        .selection_mut()
        .for_each_mut(|mut range| range.move_right(count));
    editor.mode.set_count(None);
}

fn extend_up(editor: &mut Editor) {
    let count = editor.mode.count().unwrap_or(NonZeroUsize::MIN).get();
    editor
        .window_mut()
        .buffer_mut()
        .selection_mut()
        .for_each_mut(|mut range| range.extend_up(count));
    editor.mode.set_count(None);
}

fn move_up(editor: &mut Editor) {
    let count = editor.mode.count().unwrap_or(NonZeroUsize::MIN).get();
    editor
        .window_mut()
        .buffer_mut()
        .selection_mut()
        .for_each_mut(|mut range| range.move_up(count));
    editor.mode.set_count(None);
}

fn extend_down(editor: &mut Editor) {
    let count = editor.mode.count().unwrap_or(NonZeroUsize::MIN).get();
    editor
        .window_mut()
        .buffer_mut()
        .selection_mut()
        .for_each_mut(|mut range| range.extend_down(count));
    editor.mode.set_count(None);
}

fn move_down(editor: &mut Editor) {
    let count = editor.mode.count().unwrap_or(NonZeroUsize::MIN).get();
    editor
        .window_mut()
        .buffer_mut()
        .selection_mut()
        .for_each_mut(|mut range| range.move_down(count));
    editor.mode.set_count(None);
}

fn flip(editor: &mut Editor) {
    editor
        .window_mut()
        .buffer_mut()
        .selection_mut()
        .for_each_mut(|mut range| range.flip());
    editor.mode.set_count(None);
}

fn flip_forward(editor: &mut Editor) {
    editor
        .window_mut()
        .buffer_mut()
        .selection_mut()
        .for_each_mut(|mut range| range.flip_forward());
    editor.mode.set_count(None);
}

fn reduce(editor: &mut Editor) {
    editor
        .window_mut()
        .buffer_mut()
        .selection_mut()
        .for_each_mut(|mut range| range.reduce());
    editor.mode.set_count(None);
}

fn delete(editor: &mut Editor) {
    editor
        .window_mut()
        .buffer_mut()
        .selection_mut()
        .for_each_mut(|mut range| {
            range.delete();
        });
    editor.mode.set_count(None);
}

fn undo(editor: &mut Editor) {
    let count = editor.mode.count().unwrap_or(NonZeroUsize::MIN).get();
    for _ in 1..=count {
        if !editor.window_mut().buffer_mut().undo().unwrap() {
            break;
        }
    }
    editor.mode.set_count(None);
}

fn redo(editor: &mut Editor) {
    let count = editor.mode.count().unwrap_or(NonZeroUsize::MIN).get();
    for _ in 1..=count {
        if !editor.window_mut().buffer_mut().redo().unwrap() {
            break;
        }
    }
    editor.mode.set_count(None);
}

fn select_all(editor: &mut Editor) {
    editor
        .window_mut()
        .buffer_mut()
        .selection_mut()
        .select_all();
    editor.mode.set_count(None);
}

fn insert_at_line_non_blank_start(editor: &mut Editor) {
    editor
        .window_mut()
        .buffer_mut()
        .selection_mut()
        .for_each_mut(|mut range| {
            range.flip_backward();
            range.move_to_line_non_blank_start();
            range.reduce();
        });
    editor.mode = Mode::Insert(InsertMode::default());
}
