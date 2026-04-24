#![allow(clippy::enum_glob_use)]

use crate::{
    editor::Editor,
    event::{Event, KeyEvent},
    key::{KeyCode, is},
    keymap::{Keymap, keymap},
    mode::{
        Mode,
        command::enter_command_mode,
        insert::{self, enter_insert_mode},
        prompt::enter_prompt_mode,
        seek::{SeekDirection, SeekInclude, SeekSelect, enter_seek_mode},
    },
    rope::{LINE_TYPE, RopeExt as _},
    window::{
        scroll_full_page_down, scroll_full_page_up, scroll_half_page_down, scroll_half_page_up,
    },
};
use regex_cursor::engines::meta::Regex;
use std::{cmp::min, num::NonZeroUsize, sync::LazyLock};

#[derive(Clone, Default)]
pub struct State {
    pub count: Option<NonZeroUsize>,
}

pub enum Action {
    AppendCountDigit(u8),
    EnterNormalMode,
    EnterCommandMode,
    EnterInsertMode,
    EnterGotoMode,
    EnterSeekMode {
        select: SeekSelect,
        include: SeekInclude,
        direction: SeekDirection,
    },
    InsertAfterHead,
    InsertAtLineNonBlankStart,
    InsertAtLineEnd,
    InsertLineAbove,
    InsertLineBelow,
    AddLineAbove,
    AddLineBelow,
    MoveLeft,
    MoveRight,
    MoveUp,
    MoveDown,
    ExtendLeft,
    ExtendRight,
    ExtendUp,
    ExtendDown,
    GotoLine,
    KeepPrimary,
    RotatePrimaryBackward,
    RotatePrimaryForward,
    Reduce,
    Flip,
    FlipForward,
    SelectAll,
    SelectRegex,
    Delete,
    Undo,
    Redo,
    ScrollHalfPageUp,
    ScrollHalfPageDown,
    ScrollFullPageUp,
    ScrollFullPageDown,
}

pub static KEYMAP: LazyLock<Keymap<Vec<Action>>> = LazyLock::new(|| {
    use Action::*;
    use SeekDirection::{Next, Prev};
    use SeekInclude::{Onto, Until};
    use SeekSelect::{Extend, Move};
    keymap! { Vec<Action>;
        "0" => vec![AppendCountDigit(0)],
        "1" => vec![AppendCountDigit(1)],
        "2" => vec![AppendCountDigit(2)],
        "3" => vec![AppendCountDigit(3)],
        "4" => vec![AppendCountDigit(4)],
        "5" => vec![AppendCountDigit(5)],
        "6" => vec![AppendCountDigit(6)],
        "7" => vec![AppendCountDigit(7)],
        "8" => vec![AppendCountDigit(8)],
        "9" => vec![AppendCountDigit(9)],
        "<esc>" => vec![EnterNormalMode],
        ":" => vec![EnterCommandMode],
        "i" => vec![EnterInsertMode],
        "I" => vec![InsertAtLineNonBlankStart],
        "a" => vec![InsertAfterHead],
        "A" => vec![InsertAtLineEnd],
        "h" => vec![MoveLeft],
        "l" => vec![MoveRight],
        "k" => vec![MoveUp],
        "j" => vec![MoveDown],
        "H" => vec![ExtendLeft],
        "L" => vec![ExtendRight],
        "K" => vec![ExtendUp],
        "J" => vec![ExtendDown],
        "<a-t>" => vec![EnterSeekMode { select: Move, include: Until, direction: Prev }],
        "<a-T>" => vec![EnterSeekMode { select: Extend, include: Until, direction: Prev }],
        "t" => vec![EnterSeekMode { select: Move, include: Until, direction: Next }],
        "T" => vec![EnterSeekMode { select: Extend, include: Until, direction: Next }],
        "<a-f>" => vec![EnterSeekMode { select: Move, include: Onto, direction: Prev }],
        "<a-F>" => vec![EnterSeekMode { select: Extend, include: Onto, direction: Prev }],
        "f" => vec![EnterSeekMode { select: Move, include: Onto, direction: Next }],
        "F" => vec![EnterSeekMode { select: Extend, include: Onto, direction: Next }],
        // TODO: With a count, `g` runs `GotoLine` instead of entering goto mode.
        "g" => vec![EnterGotoMode],
        "," => vec![KeepPrimary],
        "(" => vec![RotatePrimaryBackward],
        ")" => vec![RotatePrimaryForward],
        ";" => vec![Reduce],
        "<a-;>" => vec![Flip],
        "<a-:>" => vec![FlipForward],
        "%" => vec![SelectAll],
        "s" => vec![SelectRegex],
        "d" => vec![Delete],
        "c" => vec![Delete, EnterInsertMode],
        "O" => vec![InsertLineAbove],
        "o" => vec![InsertLineBelow],
        "<a-O>" => vec![AddLineAbove],
        "<a-o>" => vec![AddLineBelow],
        "u" => vec![Undo],
        "U" => vec![Redo],
        "<c-u>" => vec![ScrollHalfPageUp],
        "<c-d>" => vec![ScrollHalfPageDown],
        "<c-b>" => vec![ScrollFullPageUp],
        "<c-f>" => vec![ScrollFullPageDown],
    }
});

pub fn handle_event_normal(editor: &mut Editor, event: &Event) -> bool {
    use crate::mode::seek::{
        SeekDirection::{Next, Prev},
        SeekInclude::{Onto, Until},
        SeekSelect::{Extend, Move},
    };

    let mut handled = true;

    let count = |c: u8| {
        let digit = usize::from(c - b'0');
        let current = editor.mode.count().map_or(0, |count| usize::from(count));
        NonZeroUsize::new(current.saturating_mul(10).saturating_add(digit))
    };

    match event {
        Event::Key(KeyEvent { key, .. }) => match (key.modifiers, key.code) {
            (m, KeyCode::Char(c @ b'0'..=b'9')) if m.is_empty() => set_count(editor, count(c)),
            _ if is(key, "<esc>") => enter_normal_mode(editor),
            _ if is(key, ":") => enter_command_mode(editor),
            _ if is(key, "i") => enter_insert_mode(editor),
            _ if is(key, "I") => insert_at_line_non_blank_start(editor),
            _ if is(key, "a") => insert_after_head(editor),
            _ if is(key, "A") => insert_at_line_end(editor),
            _ if is(key, "h") => move_left(editor),
            _ if is(key, "l") => move_right(editor),
            _ if is(key, "k") => move_up(editor),
            _ if is(key, "j") => move_down(editor),
            _ if is(key, "H") => extend_left(editor),
            _ if is(key, "L") => extend_right(editor),
            _ if is(key, "K") => extend_up(editor),
            _ if is(key, "J") => extend_down(editor),
            _ if is(key, "<a-t>") => enter_seek_mode(editor, Move, Until, Prev),
            _ if is(key, "<a-T>") => enter_seek_mode(editor, Extend, Until, Prev),
            _ if is(key, "t") => enter_seek_mode(editor, Move, Until, Next),
            _ if is(key, "T") => enter_seek_mode(editor, Extend, Until, Next),
            _ if is(key, "<a-f>") => enter_seek_mode(editor, Move, Onto, Prev),
            _ if is(key, "<a-F>") => enter_seek_mode(editor, Extend, Onto, Prev),
            _ if is(key, "f") => enter_seek_mode(editor, Move, Onto, Next),
            _ if is(key, "F") => enter_seek_mode(editor, Extend, Onto, Next),
            _ if is(key, "g") => {
                if editor.mode.count().is_some() {
                    goto_line(editor);
                } else {
                    editor.mode = Mode::Goto;
                }
            }
            _ if is(key, ",") => keep_primary(editor),
            _ if is(key, "(") => rotate_primary_backward(editor),
            _ if is(key, ")") => rotate_primary_forward(editor),
            _ if is(key, ";") => reduce(editor),
            _ if is(key, "<a-;>") => flip(editor),
            _ if is(key, "<a-:>") => flip_forward(editor),
            _ if is(key, "%") => select_all(editor),
            _ if is(key, "s") => select_regex(editor),
            _ if is(key, "d") => delete(editor),
            _ if is(key, "c") => {
                delete(editor);
                enter_insert_mode(editor);
            }
            _ if is(key, "O") => insert_line_above(editor),
            _ if is(key, "o") => insert_line_below(editor),
            _ if is(key, "<a-O>") => add_line_above(editor),
            _ if is(key, "<a-o>") => add_line_below(editor),
            _ if is(key, "u") => undo(editor),
            _ if is(key, "U") => redo(editor),
            _ if is(key, "<c-u>") => scroll_half_page_up(editor),
            _ if is(key, "<c-d>") => scroll_half_page_down(editor),
            _ if is(key, "<c-b>") => scroll_full_page_up(editor),
            _ if is(key, "<c-f>") => scroll_full_page_down(editor),
            _ => handled = false,
        },
    }

    editor.pending_keys.clear();

    handled
}

pub fn enter_normal_mode(editor: &mut Editor) {
    editor.focused_buffer_mut().text.commit();
    editor.mode = Mode::Normal(State::default());
}

fn set_count(editor: &mut Editor, count: Option<NonZeroUsize>) {
    editor.mode.set_count(count);
}

fn extend_left(editor: &mut Editor) {
    let count = editor.mode.count().unwrap_or(NonZeroUsize::MIN).get();
    let mut window = editor.focused_window_mut();
    window
        .selection_mut()
        .for_each_mut(|mut range| range.extend_left(count));
    window.scroll_to_selection();
    editor.mode.set_count(None);
}

fn move_left(editor: &mut Editor) {
    let count = editor.mode.count().unwrap_or(NonZeroUsize::MIN).get();
    let mut window = editor.focused_window_mut();
    window
        .selection_mut()
        .for_each_mut(|mut range| range.move_left(count));
    window.scroll_to_selection();
    editor.mode.set_count(None);
}

fn extend_right(editor: &mut Editor) {
    let count = editor.mode.count().unwrap_or(NonZeroUsize::MIN).get();
    let mut window = editor.focused_window_mut();
    window
        .selection_mut()
        .for_each_mut(|mut range| range.extend_right(count));
    window.scroll_to_selection();
    editor.mode.set_count(None);
}

fn move_right(editor: &mut Editor) {
    let count = editor.mode.count().unwrap_or(NonZeroUsize::MIN).get();
    let mut window = editor.focused_window_mut();
    window
        .selection_mut()
        .for_each_mut(|mut range| range.move_right(count));
    window.scroll_to_selection();
    editor.mode.set_count(None);
}

fn extend_up(editor: &mut Editor) {
    let count = editor.mode.count().unwrap_or(NonZeroUsize::MIN).get();
    let mut window = editor.focused_window_mut();
    window
        .selection_mut()
        .for_each_mut(|mut range| range.extend_up(count));
    window.scroll_to_selection();
    editor.mode.set_count(None);
}

fn move_up(editor: &mut Editor) {
    let count = editor.mode.count().unwrap_or(NonZeroUsize::MIN).get();
    let mut window = editor.focused_window_mut();
    window
        .selection_mut()
        .for_each_mut(|mut range| range.move_up(count));
    window.scroll_to_selection();
    editor.mode.set_count(None);
}

fn extend_down(editor: &mut Editor) {
    let count = editor.mode.count().unwrap_or(NonZeroUsize::MIN).get();
    let mut window = editor.focused_window_mut();
    window
        .selection_mut()
        .for_each_mut(|mut range| range.extend_down(count));
    window.scroll_to_selection();
    editor.mode.set_count(None);
}

fn move_down(editor: &mut Editor) {
    let count = editor.mode.count().unwrap_or(NonZeroUsize::MIN).get();
    let mut window = editor.focused_window_mut();
    window
        .selection_mut()
        .for_each_mut(|mut range| range.move_down(count));
    window.scroll_to_selection();
    editor.mode.set_count(None);
}

fn flip(editor: &mut Editor) {
    let mut window = editor.focused_window_mut();
    window
        .selection_mut()
        .for_each_mut(|mut range| range.flip());
    window.scroll_to_selection();
    editor.mode.set_count(None);
}

fn flip_forward(editor: &mut Editor) {
    let mut window = editor.focused_window_mut();
    window
        .selection_mut()
        .for_each_mut(|mut range| range.flip_forward());
    window.scroll_to_selection();
    editor.mode.set_count(None);
}

fn keep_primary(editor: &mut Editor) {
    let mut window = editor.focused_window_mut();
    window.selection_mut().keep_primary();
    window.scroll_to_selection();
    editor.mode.set_count(None);
}

fn rotate_primary_backward(editor: &mut Editor) {
    let count = editor.mode.count().unwrap_or(NonZeroUsize::MIN).get();
    let mut window = editor.focused_window_mut();
    window.selection_mut().rotate_primary_backward(count);
    window.scroll_to_selection();
    editor.mode.set_count(None);
}

fn rotate_primary_forward(editor: &mut Editor) {
    let count = editor.mode.count().unwrap_or(NonZeroUsize::MIN).get();
    let mut window = editor.focused_window_mut();
    window.selection_mut().rotate_primary_forward(count);
    window.scroll_to_selection();
    editor.mode.set_count(None);
}

fn reduce(editor: &mut Editor) {
    let mut window = editor.focused_window_mut();
    window
        .selection_mut()
        .for_each_mut(|mut range| range.reduce());
    window.scroll_to_selection();
    editor.mode.set_count(None);
}

fn delete(editor: &mut Editor) {
    if editor.focused_buffer().text.readonly {
        editor.message = Some(Err(String::from("Buffer is readonly")));
        editor.mode.set_count(None);
        return;
    }
    let mut window = editor.focused_window_mut();
    window.selection_mut().delete();
    window.scroll_to_selection();
    editor.mode.set_count(None);
}

fn undo(editor: &mut Editor) {
    if editor.focused_buffer().text.readonly {
        editor.message = Some(Err(String::from("Buffer is readonly")));
        editor.mode.set_count(None);
        return;
    }
    let count = editor.mode.count().unwrap_or(NonZeroUsize::MIN).get();
    let mut window = editor.focused_window_mut();
    for _ in 1..=count {
        if !window.undo().unwrap() {
            break;
        }
    }
    window.scroll_to_selection();
    editor.mode.set_count(None);
}

fn redo(editor: &mut Editor) {
    if editor.focused_buffer().text.readonly {
        editor.message = Some(Err(String::from("Buffer is readonly")));
        editor.mode.set_count(None);
        return;
    }
    let count = editor.mode.count().unwrap_or(NonZeroUsize::MIN).get();
    let mut window = editor.focused_window_mut();
    for _ in 1..=count {
        if !window.redo().unwrap() {
            break;
        }
    }
    window.scroll_to_selection();
    editor.mode.set_count(None);
}

fn select_all(editor: &mut Editor) {
    let mut window = editor.focused_window_mut();
    window.selection_mut().select_all();
    window.scroll_to_selection();
    editor.mode.set_count(None);
}

fn select_regex(editor: &mut Editor) {
    enter_prompt_mode(editor, "select", |editor, regex_str| {
        if let Ok(regex) = Regex::new(regex_str) {
            let matched = editor
                .focused_window_mut()
                .selection_mut()
                .select_regex(&regex);
            if !matched {
                editor.message = Some(Err(String::from("Nothing selected")));
            }
        } else {
            editor.message = Some(Err(String::from("Invalid regex")));
        }
        editor.mode.set_count(None);
    });
}

fn insert_after_head(editor: &mut Editor) {
    if editor.focused_buffer().text.readonly {
        editor.message = Some(Err(String::from("Buffer is readonly")));
        return;
    }
    let mut window = editor.focused_window_mut();
    window.selection_mut().for_each_mut(|mut range| {
        range.move_right(1);
    });
    window.scroll_to_selection();
    editor.mode = Mode::Insert(insert::State::default());
}

fn insert_at_line_non_blank_start(editor: &mut Editor) {
    if editor.focused_buffer().text.readonly {
        editor.message = Some(Err(String::from("Buffer is readonly")));
        return;
    }
    let mut window = editor.focused_window_mut();
    window.selection_mut().for_each_mut(|mut range| {
        range.flip_backward();
        range.move_to_line_non_blank_start();
    });
    window.scroll_to_selection();
    editor.mode = Mode::Insert(insert::State::default());
}

fn insert_at_line_end(editor: &mut Editor) {
    if editor.focused_buffer().text.readonly {
        editor.message = Some(Err(String::from("Buffer is readonly")));
        return;
    }
    let mut window = editor.focused_window_mut();
    window.selection_mut().for_each_mut(|mut range| {
        range.move_onto_line_end();
    });
    window.scroll_to_selection();
    editor.mode = Mode::Insert(insert::State::default());
}

fn insert_line_above(editor: &mut Editor) {
    if editor.focused_buffer().text.readonly {
        editor.message = Some(Err(String::from("Buffer is readonly")));
        return;
    }
    // TODO: Insert multiple cursors (like Kakoune) when count > 1
    let count = editor.mode.count().unwrap_or(NonZeroUsize::MIN).get();
    let mut window = editor.focused_window_mut();
    // TODO: Switch to selection-level insert
    window.selection_mut().for_each_mut(|mut range| {
        range.move_to_line_start();
        for _ in 0..count {
            range.insert_char('\n');
        }
        range.move_left(1);
    });
    window.scroll_to_selection();
    editor.mode = Mode::Insert(insert::State::default());
}

fn insert_line_below(editor: &mut Editor) {
    if editor.focused_buffer().text.readonly {
        editor.message = Some(Err(String::from("Buffer is readonly")));
        return;
    }
    // TODO: Insert multiple cursors (like Kakoune) when count > 1
    let count = editor.mode.count().unwrap_or(NonZeroUsize::MIN).get();
    let mut window = editor.focused_window_mut();
    // TODO: Switch to selection-level insert
    window.selection_mut().for_each_mut(|mut range| {
        range.move_onto_line_end();
        for _ in 0..count {
            range.insert_char('\n');
        }
    });
    window.scroll_to_selection();
    editor.mode = Mode::Insert(insert::State::default());
}

fn add_line_above(editor: &mut Editor) {
    if editor.focused_buffer().text.readonly {
        editor.message = Some(Err(String::from("Buffer is readonly")));
        return;
    }
    let count = editor.mode.count().unwrap_or(NonZeroUsize::MIN).get();
    let mut window = editor.focused_window_mut();
    // TODO: Switch to selection-level insert
    window.selection_mut().for_each_mut(|mut range| {
        let snapshot = range.save();
        range.move_to_line_start();
        for _ in 0..count {
            range.insert_char('\n');
        }
        assert!(range.restore(&snapshot));
    });
    window.scroll_to_selection();
    editor.mode.set_count(None);
}

fn add_line_below(editor: &mut Editor) {
    if editor.focused_buffer().text.readonly {
        editor.message = Some(Err(String::from("Buffer is readonly")));
        return;
    }
    let count = editor.mode.count().unwrap_or(NonZeroUsize::MIN).get();
    let mut window = editor.focused_window_mut();
    // TODO: Switch to selection-level insert
    window.selection_mut().for_each_mut(|mut range| {
        let snapshot = range.save();
        range.move_onto_line_end();
        for _ in 0..count {
            range.insert_char('\n');
        }
        assert!(range.restore(&snapshot));
    });
    window.scroll_to_selection();
    editor.mode.set_count(None);
}

fn goto_line(editor: &mut Editor) {
    let count = editor.mode.count().unwrap_or(NonZeroUsize::MIN).get();
    let mut window = editor.focused_window_mut();
    let rope = window.buffer().text.rope().clone();
    let len_lines = rope.len_lines_indigo();
    if len_lines == 0 {
        editor.mode.set_count(None);
        return;
    }
    let line_index = min(count - 1, len_lines - 1);
    let byte_offset = rope.line_to_byte_idx(line_index, LINE_TYPE);
    window
        .selection_mut()
        .for_each_mut(|mut range| range.move_to(byte_offset));
    window.scroll_to_selection();
    editor.mode.set_count(None);
}

pub fn handle_actions_normal(editor: &mut Editor, actions: &[Action]) {
    for action in actions {
        handle_action_normal(editor, action);
    }
}

pub fn handle_action_normal(editor: &mut Editor, action: &Action) {
    match action {
        Action::AppendCountDigit(digit) => {
            let digit = usize::from(*digit);
            let current = editor.mode.count().map_or(0, |count| usize::from(count));
            let new_count = NonZeroUsize::new(current.saturating_mul(10).saturating_add(digit));
            set_count(editor, new_count);
        }
        Action::EnterNormalMode => enter_normal_mode(editor),
        Action::EnterCommandMode => enter_command_mode(editor),
        Action::EnterInsertMode => enter_insert_mode(editor),
        Action::EnterGotoMode => editor.mode = Mode::Goto,
        Action::EnterSeekMode {
            select,
            include,
            direction,
        } => {
            enter_seek_mode(editor, *select, *include, *direction);
        }
        Action::InsertAfterHead => insert_after_head(editor),
        Action::InsertAtLineNonBlankStart => insert_at_line_non_blank_start(editor),
        Action::InsertAtLineEnd => insert_at_line_end(editor),
        Action::InsertLineAbove => insert_line_above(editor),
        Action::InsertLineBelow => insert_line_below(editor),
        Action::AddLineAbove => add_line_above(editor),
        Action::AddLineBelow => add_line_below(editor),
        Action::MoveLeft => move_left(editor),
        Action::MoveRight => move_right(editor),
        Action::MoveUp => move_up(editor),
        Action::MoveDown => move_down(editor),
        Action::ExtendLeft => extend_left(editor),
        Action::ExtendRight => extend_right(editor),
        Action::ExtendUp => extend_up(editor),
        Action::ExtendDown => extend_down(editor),
        Action::GotoLine => goto_line(editor),
        Action::KeepPrimary => keep_primary(editor),
        Action::RotatePrimaryBackward => rotate_primary_backward(editor),
        Action::RotatePrimaryForward => rotate_primary_forward(editor),
        Action::Reduce => reduce(editor),
        Action::Flip => flip(editor),
        Action::FlipForward => flip_forward(editor),
        Action::SelectAll => select_all(editor),
        Action::SelectRegex => select_regex(editor),
        Action::Delete => delete(editor),
        Action::Undo => undo(editor),
        Action::Redo => redo(editor),
        Action::ScrollHalfPageUp => scroll_half_page_up(editor),
        Action::ScrollHalfPageDown => scroll_half_page_down(editor),
        Action::ScrollFullPageUp => scroll_full_page_up(editor),
        Action::ScrollFullPageDown => scroll_full_page_down(editor),
    }
}
