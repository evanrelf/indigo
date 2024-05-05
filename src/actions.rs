use crate::Editor;

pub fn move_left(editor: &mut Editor) {
    editor.move_left();
}

pub fn move_right(editor: &mut Editor) {
    editor.move_right();
}

pub fn scroll_up(editor: &mut Editor) {
    let line = editor.vertical_scroll().saturating_sub(3);
    let char_offset = 0;
    editor.scroll_to(line, char_offset);
}

pub fn scroll_down(editor: &mut Editor) {
    let line = editor.vertical_scroll() + 3;
    let char_offset = 0;
    editor.scroll_to(line, char_offset);
}
