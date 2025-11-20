use crate::{
    editor::Editor,
    mode::{InsertMode, Mode, NormalMode},
};
use std::{num::NonZeroUsize, process::ExitCode};

pub fn set_count(editor: &mut Editor, count: Option<NonZeroUsize>) {
    editor.mode.set_count(count);
}

pub fn enter_normal_mode(editor: &mut Editor) {
    editor.buffer.commit();
    editor.mode = Mode::Normal(NormalMode::default());
}

pub fn enter_insert_mode(editor: &mut Editor) {
    let mut range = editor.buffer.range_mut();
    range.reduce();
    editor.mode = Mode::Insert(InsertMode::default());
}

pub fn extend_left(editor: &mut Editor) {
    let count = editor.mode.count().unwrap_or(NonZeroUsize::MIN).get();
    let mut range = editor.buffer.range_mut();
    range.extend_left(count);
    editor.mode.set_count(None);
}

pub fn move_left(editor: &mut Editor) {
    let count = editor.mode.count().unwrap_or(NonZeroUsize::MIN).get();
    let mut range = editor.buffer.range_mut();
    range.move_left(count);
    editor.mode.set_count(None);
}

pub fn extend_right(editor: &mut Editor) {
    let count = editor.mode.count().unwrap_or(NonZeroUsize::MIN).get();
    let mut range = editor.buffer.range_mut();
    range.extend_right(count);
    editor.mode.set_count(None);
}

pub fn move_right(editor: &mut Editor) {
    let count = editor.mode.count().unwrap_or(NonZeroUsize::MIN).get();
    let mut range = editor.buffer.range_mut();
    range.move_right(count);
    editor.mode.set_count(None);
}

pub fn extend_up(editor: &mut Editor) {
    let count = editor.mode.count().unwrap_or(NonZeroUsize::MIN).get();
    let mut range = editor.buffer.range_mut();
    range.extend_up(count);
    editor.mode.set_count(None);
}

pub fn move_up(editor: &mut Editor) {
    let count = editor.mode.count().unwrap_or(NonZeroUsize::MIN).get();
    let mut range = editor.buffer.range_mut();
    range.move_up(count);
    editor.mode.set_count(None);
}

pub fn extend_down(editor: &mut Editor) {
    let count = editor.mode.count().unwrap_or(NonZeroUsize::MIN).get();
    let mut range = editor.buffer.range_mut();
    range.extend_down(count);
    editor.mode.set_count(None);
}

pub fn move_down(editor: &mut Editor) {
    let count = editor.mode.count().unwrap_or(NonZeroUsize::MIN).get();
    let mut range = editor.buffer.range_mut();
    range.move_down(count);
    editor.mode.set_count(None);
}

pub fn flip(editor: &mut Editor) {
    let mut range = editor.buffer.range_mut();
    range.flip();
    editor.mode.set_count(None);
}

pub fn flip_forward(editor: &mut Editor) {
    let mut range = editor.buffer.range_mut();
    range.flip_forward();
    editor.mode.set_count(None);
}

pub fn reduce(editor: &mut Editor) {
    let mut range = editor.buffer.range_mut();
    range.reduce();
    editor.mode.set_count(None);
}

pub fn scroll_up(editor: &mut Editor) {
    let mut window = editor.window_mut();
    let line = window.vertical_scroll().saturating_sub(3);
    window.scroll_to_line(line);
}

pub fn scroll_down(editor: &mut Editor) {
    let mut window = editor.window_mut();
    let line = window.vertical_scroll() + 3;
    window.scroll_to_line(line);
}

pub fn scroll_half_page_up(editor: &mut Editor) {
    let mut window = editor.window_mut();
    let line = window
        .vertical_scroll()
        .saturating_sub(usize::from(window.height()) / 2);
    window.scroll_to_line(line);
}

pub fn scroll_half_page_down(editor: &mut Editor) {
    let mut window = editor.window_mut();
    let line = window.vertical_scroll() + usize::from(window.height()) / 2;
    window.scroll_to_line(line);
}

pub fn scroll_full_page_up(editor: &mut Editor) {
    let mut window = editor.window_mut();
    let line = window
        .vertical_scroll()
        .saturating_sub(usize::from(window.height()));
    window.scroll_to_line(line);
}

pub fn scroll_full_page_down(editor: &mut Editor) {
    let mut window = editor.window_mut();
    let line = window.vertical_scroll() + usize::from(window.height());
    window.scroll_to_line(line);
}

pub fn insert_char(editor: &mut Editor, char: char) {
    let mut range = editor.buffer.range_mut();
    range.insert_char(char);
    editor.mode.set_count(None);
}

pub fn insert(editor: &mut Editor, text: &str) {
    let mut range = editor.buffer.range_mut();
    range.insert(text);
    editor.mode.set_count(None);
}

pub fn delete_before(editor: &mut Editor) {
    let mut range = editor.buffer.range_mut();
    range.delete_before();
    editor.mode.set_count(None);
}

pub fn delete(editor: &mut Editor) {
    let mut range = editor.buffer.range_mut();
    range.delete();
    editor.mode.set_count(None);
}

pub fn delete_after(editor: &mut Editor) {
    let mut range = editor.buffer.range_mut();
    range.delete_after();
    editor.mode.set_count(None);
}

pub fn undo(editor: &mut Editor) {
    let count = editor.mode.count().unwrap_or(NonZeroUsize::MIN).get();
    for _ in 1..=count {
        if !editor.buffer.undo().unwrap() {
            break;
        }
    }
    editor.mode.set_count(None);
}

pub fn redo(editor: &mut Editor) {
    let count = editor.mode.count().unwrap_or(NonZeroUsize::MIN).get();
    for _ in 1..=count {
        if !editor.buffer.redo().unwrap() {
            break;
        }
    }
    editor.mode.set_count(None);
}

pub fn exit(editor: &mut Editor, exit_code: u8) {
    editor.exit = Some(ExitCode::from(exit_code));
}
