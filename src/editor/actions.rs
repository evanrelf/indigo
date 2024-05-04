use crate::editor::Editor;

pub fn move_left(editor: &mut Editor) {
    editor.cursor = editor.cursor.saturating_sub(1);
}

pub fn move_right(editor: &mut Editor) {
    editor.cursor += 1;
}
