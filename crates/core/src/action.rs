use crate::{
    editor::Editor,
    mode::{CommandMode, InsertMode, Mode, NormalMode},
};
use std::num::NonZeroUsize;

#[cfg(any(feature = "arbitrary", test))]
use arbitrary::Arbitrary;

// TODO: Don't assume editing operations target the file's rope. Could be editing the command's
// rope!

#[cfg_attr(any(feature = "arbitrary", test), derive(Arbitrary))]
#[derive(Debug)]
pub enum Action {
    UpdateCount(NonZeroUsize),
    EnterNormalMode,
    EnterInsertMode,
    EnterCommandMode,
    MoveLeft,
    MoveRight,
    MoveTo(usize),
    ExtendLeft,
    ExtendRight,
    ExtendTo(usize),
    Flip,
    FlipForward,
    Reduce,
    ScrollUp,
    ScrollDown,
    ScrollHalfPageUp,
    ScrollHalfPageDown,
    ScrollFullPageUp,
    ScrollFullPageDown,
    InsertChar(char),
    Insert(String),
    DeleteBefore,
    Delete,
    DeleteAfter,
    RunCommand(String),
    Exit(u8),
}

pub fn handle_action(editor: &mut Editor, action: Action) {
    match action {
        Action::UpdateCount(count) => update_count(editor, count),
        Action::EnterNormalMode => enter_normal_mode(editor),
        Action::EnterInsertMode => enter_insert_mode(editor),
        Action::EnterCommandMode => enter_command_mode(editor),
        Action::MoveLeft => move_left(editor),
        Action::MoveRight => move_right(editor),
        Action::MoveTo(index) => move_to(editor, index),
        Action::ExtendLeft => extend_left(editor),
        Action::ExtendRight => extend_right(editor),
        Action::ExtendTo(index) => extend_to(editor, index),
        Action::Flip => flip(editor),
        Action::FlipForward => flip_forward(editor),
        Action::Reduce => reduce(editor),
        Action::ScrollUp => scroll_up(editor),
        Action::ScrollDown => scroll_down(editor),
        Action::ScrollHalfPageUp => scroll_half_page_up(editor),
        Action::ScrollHalfPageDown => scroll_half_page_down(editor),
        Action::ScrollFullPageUp => scroll_full_page_up(editor),
        Action::ScrollFullPageDown => scroll_full_page_down(editor),
        Action::InsertChar(char) => insert_char(editor, char),
        Action::Insert(string) => insert(editor, &string),
        Action::DeleteBefore => delete_before(editor),
        Action::Delete => delete(editor),
        Action::DeleteAfter => delete_after(editor),
        Action::RunCommand(command) => run_command(editor, &command),
        Action::Exit(exit_code) => exit(editor, exit_code),
    }
}

fn update_count(editor: &mut Editor, count: NonZeroUsize) {
    editor.mode.set_count(count);
}

fn enter_normal_mode(editor: &mut Editor) {
    editor.mode = Mode::Normal(NormalMode::default());
}

fn enter_insert_mode(editor: &mut Editor) {
    editor.buffer.with_range_mut(|range| range.reduce());
    editor.mode = Mode::Insert(InsertMode::default());
}

fn enter_command_mode(editor: &mut Editor) {
    editor.mode = Mode::Command(CommandMode::default());
}

fn move_left(editor: &mut Editor) {
    let count = editor.mode.count();
    editor.buffer.with_range_mut(|range| {
        range.extend_left(count);
        range.reduce();
    });
    editor.mode.set_count(NonZeroUsize::MIN);
}

fn move_right(editor: &mut Editor) {
    let count = editor.mode.count();
    editor.buffer.with_range_mut(|range| {
        range.extend_right(count);
        range.reduce();
    });
    editor.mode.set_count(NonZeroUsize::MIN);
}

fn move_to(_editor: &mut Editor, _index: usize) {
    // TODO: Allow changing range indices
}

fn extend_left(editor: &mut Editor) {
    let count = editor.mode.count();
    editor
        .buffer
        .with_range_mut(|range| range.extend_left(count));
    editor.mode.set_count(NonZeroUsize::MIN);
}

fn extend_right(editor: &mut Editor) {
    let count = editor.mode.count();
    editor
        .buffer
        .with_range_mut(|range| range.extend_right(count));
    editor.mode.set_count(NonZeroUsize::MIN);
}

fn extend_to(_editor: &mut Editor, _index: usize) {
    // TODO: Allow changing range indices
}

fn flip(editor: &mut Editor) {
    editor.buffer.with_range_mut(|range| range.flip());
    editor.mode.set_count(NonZeroUsize::MIN);
}

fn flip_forward(editor: &mut Editor) {
    editor.buffer.with_range_mut(|range| range.flip_forward());
    editor.mode.set_count(NonZeroUsize::MIN);
}

fn reduce(editor: &mut Editor) {
    editor.buffer.with_range_mut(|range| range.reduce());
    editor.mode.set_count(NonZeroUsize::MIN);
}

fn scroll_up(editor: &mut Editor) {
    let line = editor.buffer.vertical_scroll().saturating_sub(3);
    editor.buffer.scroll_to(line);
}

fn scroll_down(editor: &mut Editor) {
    let line = editor.buffer.vertical_scroll() + 3;
    editor.buffer.scroll_to(line);
}

fn scroll_half_page_up(editor: &mut Editor) {
    let line = editor
        .buffer
        .vertical_scroll()
        .saturating_sub(editor.height / 2);
    editor.buffer.scroll_to(line);
}

fn scroll_half_page_down(editor: &mut Editor) {
    let line = editor.buffer.vertical_scroll() + editor.height / 2;
    editor.buffer.scroll_to(line);
}

fn scroll_full_page_up(editor: &mut Editor) {
    let line = editor
        .buffer
        .vertical_scroll()
        .saturating_sub(editor.height);
    editor.buffer.scroll_to(line);
}

fn scroll_full_page_down(editor: &mut Editor) {
    let line = editor.buffer.vertical_scroll() + editor.height;
    editor.buffer.scroll_to(line);
}

fn insert_char(editor: &mut Editor, char: char) {
    if let Mode::Command(ref mut command_mode) = editor.mode {
        command_mode.with_cursor_mut(|cursor| cursor.insert_char(char));
    } else {
        editor
            .buffer
            .with_range_mut(|range| range.insert_char(char));
        editor.mode.set_count(NonZeroUsize::MIN);
    }
}

fn insert(editor: &mut Editor, string: &str) {
    if let Mode::Command(ref mut command_mode) = editor.mode {
        command_mode.with_cursor_mut(|cursor| cursor.insert(string));
    } else {
        editor.buffer.with_range_mut(|range| range.insert(string));
        editor.mode.set_count(NonZeroUsize::MIN);
    }
}

fn delete_before(editor: &mut Editor) {
    let count = editor.mode.count();
    if let Mode::Command(ref mut command_mode) = editor.mode {
        command_mode.with_cursor_mut(|cursor| cursor.delete_before(count));
    } else {
        editor
            .buffer
            .with_range_mut(|range| range.delete_before(count));
        editor.mode.set_count(NonZeroUsize::MIN);
    }
}

fn delete(editor: &mut Editor) {
    editor.buffer.with_range_mut(|range| range.delete());
    editor.mode.set_count(NonZeroUsize::MIN);
}

fn delete_after(editor: &mut Editor) {
    let count = editor.mode.count();
    editor
        .buffer
        .with_range_mut(|range| range.delete_after(count));
    editor.mode.set_count(NonZeroUsize::MIN);
}

fn run_command(editor: &mut Editor, command: &str) {
    match command {
        "q" | "quit" => exit(editor, 0),
        _ => {}
    }
}

fn exit(editor: &mut Editor, exit_code: u8) {
    editor.exit = Some(exit_code);
}
