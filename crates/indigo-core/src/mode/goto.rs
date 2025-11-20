use crate::{
    editor::Editor,
    event::{Event, KeyEvent},
    key::is,
    mode::{Mode, normal::enter_normal_mode},
};

#[derive(Default)]
pub struct GotoMode {}

pub fn handle_event_goto(editor: &mut Editor, event: &Event) -> bool {
    let Mode::Goto(_goto_mode) = &editor.mode else {
        panic!("Not in goto mode")
    };

    match event {
        Event::Key(KeyEvent { key, .. }) => match (key.modifiers, key.code) {
            _ if is(key, "k") => move_to_top(editor),
            _ if is(key, "K") => extend_to_top(editor),
            _ if is(key, "j") => move_to_bottom(editor),
            _ if is(key, "J") => extend_to_bottom(editor),
            _ if is(key, "h") => move_to_line_start(editor),
            _ if is(key, "H") => extend_to_line_start(editor),
            _ if is(key, "i") => move_to_line_non_blank_start(editor),
            _ if is(key, "I") => extend_to_line_non_blank_start(editor),
            _ if is(key, "l") => move_to_line_end(editor),
            _ if is(key, "L") => extend_to_line_end(editor),
            _ => {}
        },
    }

    enter_normal_mode(editor);

    true
}

pub fn enter_goto_mode(editor: &mut Editor) {
    editor.mode = Mode::Goto(GotoMode::default());
}

fn extend_to_top(editor: &mut Editor) {
    let mut range = editor.buffer.range_mut();
    range.extend_to_top();
    editor.mode.set_count(None);
}

fn move_to_top(editor: &mut Editor) {
    let mut range = editor.buffer.range_mut();
    range.move_to_top();
    editor.mode.set_count(None);
}

fn extend_to_bottom(editor: &mut Editor) {
    let mut range = editor.buffer.range_mut();
    range.extend_to_bottom();
    editor.mode.set_count(None);
}

fn move_to_bottom(editor: &mut Editor) {
    let mut range = editor.buffer.range_mut();
    range.move_to_bottom();
    editor.mode.set_count(None);
}

fn extend_to_line_start(editor: &mut Editor) {
    let mut range = editor.buffer.range_mut();
    range.extend_to_line_start();
    editor.mode.set_count(None);
}

fn move_to_line_start(editor: &mut Editor) {
    let mut range = editor.buffer.range_mut();
    range.move_to_line_start();
    editor.mode.set_count(None);
}

fn extend_to_line_non_blank_start(editor: &mut Editor) {
    let mut range = editor.buffer.range_mut();
    range.extend_to_line_non_blank_start();
    editor.mode.set_count(None);
}

fn move_to_line_non_blank_start(editor: &mut Editor) {
    let mut range = editor.buffer.range_mut();
    range.move_to_line_non_blank_start();
    editor.mode.set_count(None);
}

fn extend_to_line_end(editor: &mut Editor) {
    let mut range = editor.buffer.range_mut();
    range.extend_to_line_end();
    editor.mode.set_count(None);
}

fn move_to_line_end(editor: &mut Editor) {
    let mut range = editor.buffer.range_mut();
    range.move_to_line_end();
    editor.mode.set_count(None);
}
