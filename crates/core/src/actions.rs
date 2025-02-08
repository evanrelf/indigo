use crate::{range2::RangeExt as _, Editor};
use std::cmp::max;

pub fn move_left(editor: &mut Editor) {
    let distance = max(1, editor.count);
    editor.range2_mut().move_left(distance);
    editor.count = 0;
}

pub fn move_right(editor: &mut Editor) {
    let distance = max(1, editor.count);
    editor.range2_mut().move_right(distance);
    editor.count = 0;
}

pub fn extend_left(editor: &mut Editor) {
    let distance = max(1, editor.count);
    editor.range2_mut().extend_left(distance);
    editor.count = 0;
}

pub fn extend_right(editor: &mut Editor) {
    let distance = max(1, editor.count);
    editor.range2_mut().extend_right(distance);
    editor.count = 0;
}

pub fn scroll_up(editor: &mut Editor) {
    let line = editor.vertical_scroll().saturating_sub(3);
    editor.scroll_to(line);
}

pub fn scroll_down(editor: &mut Editor) {
    let line = editor.vertical_scroll() + 3;
    editor.scroll_to(line);
}

pub fn scroll_half_page_up(editor: &mut Editor) {
    let line = editor.vertical_scroll().saturating_sub(editor.height / 2);
    editor.scroll_to(line);
}

pub fn scroll_half_page_down(editor: &mut Editor) {
    let line = editor.vertical_scroll() + editor.height / 2;
    editor.scroll_to(line);
}

pub fn scroll_full_page_up(editor: &mut Editor) {
    let line = editor.vertical_scroll().saturating_sub(editor.height);
    editor.scroll_to(line);
}

pub fn scroll_full_page_down(editor: &mut Editor) {
    let line = editor.vertical_scroll() + editor.height;
    editor.scroll_to(line);
}

pub fn insert_char(editor: &mut Editor, c: char) {
    editor.range2_mut().insert_char(c);
    editor.count = 0;
}

pub fn backspace(editor: &mut Editor) {
    editor.range2_mut().backspace();
    editor.count = 0;
}

pub fn delete(editor: &mut Editor) {
    editor.range2_mut().delete();
    editor.count = 0;
}
