use crate::Editor;

pub fn move_left(editor: &mut Editor) {
    editor.cursor = editor.cursor.saturating_sub(1);
}

pub fn move_right(editor: &mut Editor) {
    editor.cursor += 1;
}

pub fn scroll_up(editor: &mut Editor) {
    editor.scroll_to(editor.vertical_scroll().saturating_sub(3));
}

pub fn scroll_down(editor: &mut Editor) {
    editor.scroll_to(editor.vertical_scroll() + 3);
}
