use crate::editor::Editor;
use std::process::ExitCode;

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

pub fn exit(editor: &mut Editor, exit_code: u8) {
    editor.exit = Some(ExitCode::from(exit_code));
}
