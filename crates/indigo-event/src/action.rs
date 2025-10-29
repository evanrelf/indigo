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
    SetCount(Option<NonZeroUsize>),
    EnterNormalMode,
    EnterInsertMode,
    EnterCommandMode,
    MoveLeft,
    MoveRight,
    MoveUntilPrevByte(u8),
    MoveToPrevByte(u8),
    MoveUntilNextByte(u8),
    MoveToNextByte(u8),
    ExtendLeft,
    ExtendRight,
    ExtendUntilPrevByte(u8),
    ExtendToPrevByte(u8),
    ExtendUntilNextByte(u8),
    ExtendToNextByte(u8),
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
    Undo,
    Redo,
    RunCommand(Rc<str>),
    Exit(u8),
}

pub fn handle_action(editor: &mut Editor, action: &Action) {
    match action {
        Action::SetCount(count) => set_count(editor, *count),
        Action::EnterNormalMode => enter_normal_mode(editor),
        Action::EnterInsertMode => enter_insert_mode(editor),
        Action::EnterCommandMode => enter_command_mode(editor),
        Action::MoveLeft => move_left(editor),
        Action::MoveRight => move_right(editor),
        Action::MoveUntilPrevByte(byte) => move_until_prev_byte(editor, *byte),
        Action::MoveToPrevByte(byte) => move_to_prev_byte(editor, *byte),
        Action::MoveUntilNextByte(byte) => move_until_next_byte(editor, *byte),
        Action::MoveToNextByte(byte) => move_to_next_byte(editor, *byte),
        Action::ExtendLeft => extend_left(editor),
        Action::ExtendRight => extend_right(editor),
        Action::ExtendUntilPrevByte(byte) => extend_until_prev_byte(editor, *byte),
        Action::ExtendToPrevByte(byte) => extend_to_prev_byte(editor, *byte),
        Action::ExtendUntilNextByte(byte) => extend_until_next_byte(editor, *byte),
        Action::ExtendToNextByte(byte) => extend_to_next_byte(editor, *byte),
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
        Action::Undo => undo(editor),
        Action::Redo => redo(editor),
        Action::RunCommand(command) => run_command(editor, command),
        Action::Exit(exit_code) => exit(editor, *exit_code),
    }
}

fn set_count(editor: &mut Editor, count: Option<NonZeroUsize>) {
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
    let count = editor.mode.count().unwrap_or(NonZeroUsize::MIN).get();
    editor.buffer.with_range_mut(|range| {
        for _ in 1..=count {
            range.extend_left();
        }
        range.reduce();
    });
    editor.mode.set_count(None);
}

fn move_right(editor: &mut Editor) {
    let count = editor.mode.count().unwrap_or(NonZeroUsize::MIN).get();
    editor.buffer.with_range_mut(|range| {
        for _ in 1..=count {
            range.extend_right();
        }
        range.reduce();
    });
    editor.mode.set_count(None);
}

fn move_until_prev_byte(editor: &mut Editor, byte: u8) {
    editor.buffer.with_range_mut(|range| {
        range.reduce();
        range.extend_until_prev_byte(byte);
    });
    editor.mode.set_count(None);
}

fn move_to_prev_byte(editor: &mut Editor, byte: u8) {
    editor.buffer.with_range_mut(|range| {
        range.reduce();
        range.extend_until_prev_byte(byte);
        range.extend_left();
    });
    editor.mode.set_count(None);
}

fn move_until_next_byte(editor: &mut Editor, byte: u8) {
    editor.buffer.with_range_mut(|range| {
        range.reduce();
        range.extend_until_next_byte(byte);
    });
    editor.mode.set_count(None);
}

fn move_to_next_byte(editor: &mut Editor, byte: u8) {
    editor.buffer.with_range_mut(|range| {
        range.reduce();
        range.extend_until_next_byte(byte);
        range.extend_right();
    });
    editor.mode.set_count(None);
}

fn extend_until_prev_byte(editor: &mut Editor, byte: u8) {
    editor.buffer.with_range_mut(|range| {
        range.extend_until_prev_byte(byte);
    });
    editor.mode.set_count(None);
}

fn extend_to_prev_byte(editor: &mut Editor, byte: u8) {
    editor.buffer.with_range_mut(|range| {
        range.extend_until_prev_byte(byte);
        range.extend_right();
    });
    editor.mode.set_count(None);
}

fn extend_until_next_byte(editor: &mut Editor, byte: u8) {
    editor.buffer.with_range_mut(|range| {
        range.extend_until_next_byte(byte);
    });
    editor.mode.set_count(None);
}

fn extend_to_next_byte(editor: &mut Editor, byte: u8) {
    editor.buffer.with_range_mut(|range| {
        range.extend_until_next_byte(byte);
        range.extend_right();
    });
    editor.mode.set_count(None);
}

fn extend_left(editor: &mut Editor) {
    let count = editor.mode.count().unwrap_or(NonZeroUsize::MIN).get();
    editor.buffer.with_range_mut(|range| {
        for _ in 1..=count {
            range.extend_left();
        }
    });
    editor.mode.set_count(None);
}

fn extend_right(editor: &mut Editor) {
    let count = editor.mode.count().unwrap_or(NonZeroUsize::MIN).get();
    editor.buffer.with_range_mut(|range| {
        for _ in 1..=count {
            range.extend_right();
        }
    });
    editor.mode.set_count(None);
}

fn flip(editor: &mut Editor) {
    editor.buffer.with_range_mut(|range| range.flip());
    editor.mode.set_count(None);
}

fn flip_forward(editor: &mut Editor) {
    editor.buffer.with_range_mut(|range| range.flip_forward());
    editor.mode.set_count(None);
}

fn reduce(editor: &mut Editor) {
    editor.buffer.with_range_mut(|range| range.reduce());
    editor.mode.set_count(None);
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
        .saturating_sub(editor.terminal_height / 2);
    editor.buffer.scroll_to(line);
}

fn scroll_half_page_down(editor: &mut Editor) {
    let line = editor.buffer.vertical_scroll() + editor.terminal_height / 2;
    editor.buffer.scroll_to(line);
}

fn scroll_full_page_up(editor: &mut Editor) {
    let line = editor
        .buffer
        .vertical_scroll()
        .saturating_sub(editor.terminal_height);
    editor.buffer.scroll_to(line);
}

fn scroll_full_page_down(editor: &mut Editor) {
    let line = editor.buffer.vertical_scroll() + editor.terminal_height;
    editor.buffer.scroll_to(line);
}

fn insert_char(editor: &mut Editor, char: char) {
    if let Mode::Command(ref mut command_mode) = editor.mode {
        command_mode.cursor_mut().insert_char(char);
    } else {
        editor
            .buffer
            .with_range_mut(|range| range.insert_char(char));
        editor.mode.set_count(None);
    }
}

fn insert(editor: &mut Editor, text: &str) {
    if let Mode::Command(ref mut command_mode) = editor.mode {
        command_mode.cursor_mut().insert(text);
    } else {
        editor.buffer.with_range_mut(|range| range.insert(text));
        editor.mode.set_count(None);
    }
}

fn delete_before(editor: &mut Editor) {
    let count = editor.mode.count().unwrap_or(NonZeroUsize::MIN).get();
    if let Mode::Command(ref mut command_mode) = editor.mode {
        if command_mode.cursor().char_offset() == 0 {
            enter_normal_mode(editor);
        } else {
            for _ in 1..=count {
                command_mode.cursor_mut().delete_before();
            }
        }
    } else {
        editor.buffer.with_range_mut(|range| {
            for _ in 1..=count {
                range.delete_before();
            }
        });
        editor.mode.set_count(None);
    }
}

fn delete(editor: &mut Editor) {
    editor.buffer.with_range_mut(|range| range.delete());
    editor.mode.set_count(None);
}

fn delete_after(editor: &mut Editor) {
    let count = editor.mode.count().unwrap_or(NonZeroUsize::MIN).get();
    editor.buffer.with_range_mut(|range| {
        for _ in 1..=count {
            range.delete_after();
        }
    });
    editor.mode.set_count(None);
}

fn undo(editor: &mut Editor) {
    editor.buffer.undo().unwrap();
}

fn redo(editor: &mut Editor) {
    editor.buffer.redo().unwrap();
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
