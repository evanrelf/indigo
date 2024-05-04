use crate::{Editor, RopeExt as _};
use std::cmp::min;

pub fn move_left(editor: &mut Editor) {
    editor.cursor = editor.cursor.saturating_sub(1);
}

pub fn move_right(editor: &mut Editor) {
    editor.cursor += 1;
}

pub fn scroll_up(editor: &mut Editor) {
    let line = editor.vertical_scroll.saturating_sub(3);
    let last_line = editor.text().len_lines_indigo().saturating_sub(1);
    editor.vertical_scroll = min(line, last_line);
}

pub fn scroll_down(editor: &mut Editor) {
    let line = editor.vertical_scroll + 3;
    let last_line = editor.text().len_lines_indigo().saturating_sub(1);
    editor.vertical_scroll = min(line, last_line);
}
