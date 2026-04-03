use crate::{
    editor::Editor,
    event::{Event, KeyEvent},
    keymap::{Keymap, KeymapResult, keymap},
    mode::{Mode, normal::enter_normal_mode},
};
use std::sync::LazyLock;

#[derive(Clone, Default)]
pub struct State {}

pub static KEYMAP: LazyLock<Keymap<fn(&mut Editor)>> = LazyLock::new(|| {
    keymap! { fn(&mut Editor);
        "k" => |editor| move_to_start(editor),
        "K" => |editor| extend_to_start(editor),
        "j" => |editor| move_to_bottom(editor),
        "J" => |editor| extend_to_bottom(editor),
        "e" => |editor| move_to_end(editor),
        "E" => |editor| extend_to_end(editor),
        "h" => |editor| move_to_line_start(editor),
        "H" => |editor| extend_to_line_start(editor),
        "i" => |editor| move_to_line_non_blank_start(editor),
        "I" => |editor| extend_to_line_non_blank_start(editor),
        "l" => |editor| move_until_line_end(editor),
        "L" => |editor| extend_until_line_end(editor),
    }
});

pub fn handle_event_goto(editor: &mut Editor, event: &Event) -> bool {
    let Mode::Goto(_goto_mode) = &editor.mode else {
        panic!("Not in goto mode")
    };

    match event {
        Event::Key(KeyEvent { key, .. }) => match KEYMAP.get_keys(&[*key]) {
            KeymapResult::Mapped(f) => f(editor),
            KeymapResult::Fallback(f) => f(editor),
            KeymapResult::Unmapped | KeymapResult::Pending => {}
        },
    }

    enter_normal_mode(editor);

    true
}

pub fn enter_goto_mode(editor: &mut Editor) {
    editor.mode = Mode::Goto(State::default());
}

fn extend_to_start(editor: &mut Editor) {
    let mut window = editor.focused_window_mut();
    window
        .selection_mut()
        .for_each_mut(|mut range| range.extend_to_start());
    window.scroll_to_selection();
    editor.mode.set_count(None);
}

fn move_to_start(editor: &mut Editor) {
    let mut window = editor.focused_window_mut();
    window
        .selection_mut()
        .for_each_mut(|mut range| range.move_to_start());
    window.scroll_to_selection();
    editor.mode.set_count(None);
}

fn extend_to_bottom(editor: &mut Editor) {
    let mut window = editor.focused_window_mut();
    window
        .selection_mut()
        .for_each_mut(|mut range| range.extend_to_bottom());
    window.scroll_to_selection();
    editor.mode.set_count(None);
}

fn move_to_bottom(editor: &mut Editor) {
    let mut window = editor.focused_window_mut();
    window
        .selection_mut()
        .for_each_mut(|mut range| range.move_to_bottom());
    window.scroll_to_selection();
    editor.mode.set_count(None);
}

fn extend_to_end(editor: &mut Editor) {
    let mut window = editor.focused_window_mut();
    window
        .selection_mut()
        .for_each_mut(|mut range| range.extend_to_end());
    window.scroll_to_selection();
    editor.mode.set_count(None);
}

fn move_to_end(editor: &mut Editor) {
    let mut window = editor.focused_window_mut();
    window
        .selection_mut()
        .for_each_mut(|mut range| range.move_to_end());
    window.scroll_to_selection();
    editor.mode.set_count(None);
}

fn extend_to_line_start(editor: &mut Editor) {
    editor
        .focused_window_mut()
        .selection_mut()
        .for_each_mut(|mut range| range.extend_to_line_start());
    editor.mode.set_count(None);
}

fn move_to_line_start(editor: &mut Editor) {
    editor
        .focused_window_mut()
        .selection_mut()
        .for_each_mut(|mut range| range.move_to_line_start());
    editor.mode.set_count(None);
}

fn extend_to_line_non_blank_start(editor: &mut Editor) {
    editor
        .focused_window_mut()
        .selection_mut()
        .for_each_mut(|mut range| range.extend_to_line_non_blank_start());
    editor.mode.set_count(None);
}

fn move_to_line_non_blank_start(editor: &mut Editor) {
    editor
        .focused_window_mut()
        .selection_mut()
        .for_each_mut(|mut range| range.move_to_line_non_blank_start());
    editor.mode.set_count(None);
}

fn extend_until_line_end(editor: &mut Editor) {
    editor
        .focused_window_mut()
        .selection_mut()
        .for_each_mut(|mut range| range.extend_until_line_end());
    editor.mode.set_count(None);
}

fn move_until_line_end(editor: &mut Editor) {
    editor
        .focused_window_mut()
        .selection_mut()
        .for_each_mut(|mut range| range.move_until_line_end());
    editor.mode.set_count(None);
}
