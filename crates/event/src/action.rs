use indigo_core::{
    editor::Editor,
    mode::{CommandMode, InsertMode, Mode, NormalMode},
};
use std::{num::NonZeroUsize, rc::Rc};

#[cfg(any(feature = "arbitrary", test))]
use arbitrary::Arbitrary;

// TODO: Don't assume editing operations target the file's rope. Could be editing the command's
// rope!

#[cfg_attr(any(feature = "arbitrary", test), derive(Arbitrary))]
#[derive(Clone, Debug)]
pub enum Action {
    UpdateCount(NonZeroUsize),
    EnterNormalMode,
    EnterInsertMode,
    EnterCommandMode,
    MoveLeft,
    MoveRight,
    ExtendLeft,
    ExtendRight,
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
    Insert(Rc<str>),
    DeleteBefore,
    Delete,
    DeleteAfter,
    RunCommand(Rc<str>),
    Exit(u8),
}

pub fn handle_action(editor: &mut Editor, action: &Action) {
    match action {
        Action::UpdateCount(count) => update_count(editor, *count),
        Action::EnterNormalMode => enter_normal_mode(editor),
        Action::EnterInsertMode => enter_insert_mode(editor),
        Action::EnterCommandMode => enter_command_mode(editor),
        Action::MoveLeft => move_left(editor),
        Action::MoveRight => move_right(editor),
        Action::ExtendLeft => extend_left(editor),
        Action::ExtendRight => extend_right(editor),
        Action::Flip => flip(editor),
        Action::FlipForward => flip_forward(editor),
        Action::Reduce => reduce(editor),
        Action::ScrollUp => scroll_up(editor),
        Action::ScrollDown => scroll_down(editor),
        Action::ScrollHalfPageUp => scroll_half_page_up(editor),
        Action::ScrollHalfPageDown => scroll_half_page_down(editor),
        Action::ScrollFullPageUp => scroll_full_page_up(editor),
        Action::ScrollFullPageDown => scroll_full_page_down(editor),
        Action::InsertChar(char) => insert_char(editor, *char),
        Action::Insert(text) => insert(editor, text),
        Action::DeleteBefore => delete_before(editor),
        Action::Delete => delete(editor),
        Action::DeleteAfter => delete_after(editor),
        Action::RunCommand(command) => run_command(editor, command),
        Action::Exit(exit_code) => exit(editor, *exit_code),
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
        command_mode.cursor_mut().insert_char(char);
    } else {
        editor
            .buffer
            .with_range_mut(|range| range.insert_char(char));
        editor.mode.set_count(NonZeroUsize::MIN);
    }
}

fn insert(editor: &mut Editor, text: &str) {
    if let Mode::Command(ref mut command_mode) = editor.mode {
        command_mode.cursor_mut().insert(text);
    } else {
        editor.buffer.with_range_mut(|range| range.insert(text));
        editor.mode.set_count(NonZeroUsize::MIN);
    }
}

fn delete_before(editor: &mut Editor) {
    let count = editor.mode.count();
    if let Mode::Command(ref mut command_mode) = editor.mode {
        if command_mode.cursor().char_offset() == 0 {
            enter_normal_mode(editor);
        } else {
            command_mode.cursor_mut().delete_before(count);
        }
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
    match command.trim() {
        "q" | "quit" => exit(editor, 0),
        _ => {}
    }
}

fn exit(editor: &mut Editor, exit_code: u8) {
    editor.exit = Some(exit_code);
}
