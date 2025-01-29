use crate::{Editor, RangeExt as _};
use std::cmp::max;

// TODO: Replace `with_range{,_mut}` with `range{,_mut}`?

pub fn move_left(editor: &mut Editor) {
    let distance = max(1, editor.count);
    editor.with_range(|range| range.move_left(distance));
}

pub fn move_right(editor: &mut Editor) {
    let distance = max(1, editor.count);
    editor.with_range(|range| range.move_right(distance));
}

pub fn extend_left(editor: &mut Editor) {
    let distance = max(1, editor.count);
    editor.with_range(|range| range.extend_left(distance));
}

pub fn extend_right(editor: &mut Editor) {
    let distance = max(1, editor.count);
    editor.with_range(|range| range.extend_right(distance));
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
    editor.with_range_mut(|range| range.insert_char(c));
}

pub fn backspace(editor: &mut Editor) {
    editor.with_range_mut(|range| range.backspace(1));
}

pub fn delete(editor: &mut Editor) {
    editor.with_range_mut(|range| range.delete(1));
}
