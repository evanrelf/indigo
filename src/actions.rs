use crate::{CursorExt as _, Editor};
use ratatui::prelude::Rect;

pub fn move_left(editor: &mut Editor) {
    editor.with_cursor(|cursor| cursor.move_left(1));
}

pub fn move_right(editor: &mut Editor) {
    editor.with_cursor(|cursor| cursor.move_right(1));
}

pub fn scroll_up(editor: &mut Editor) {
    let line = editor.vertical_scroll().saturating_sub(3);
    editor.scroll_to(line);
}

pub fn scroll_down(editor: &mut Editor) {
    let line = editor.vertical_scroll() + 3;
    editor.scroll_to(line);
}

pub fn scroll_half_page_up(editor: &mut Editor, area: Rect) {
    let line = editor
        .vertical_scroll()
        .saturating_sub(usize::from(area.height / 2));
    editor.scroll_to(line);
}

pub fn scroll_half_page_down(editor: &mut Editor, area: Rect) {
    let line = editor.vertical_scroll() + usize::from(area.height / 2);
    editor.scroll_to(line);
}
