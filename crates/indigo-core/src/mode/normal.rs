#![allow(clippy::enum_glob_use)]

use crate::{
    editor::Editor,
    keymap::{Keymap, KeymapResult, keymap},
    mode::{
        Mode,
        command::enter_command_mode,
        insert, prompt, replace,
        seek::{self, SeekDirection, SeekInclude, SeekSelect},
    },
    rope::{LINE_TYPE, RopeExt as _},
    window::{
        scroll_full_page_down, scroll_full_page_up, scroll_half_page_down, scroll_half_page_up,
    },
};
use regex_cursor::engines::meta::Regex;
use std::{cmp::min, num::NonZeroUsize, sync::LazyLock};

#[cfg(any(feature = "arbitrary", test))]
use arbitrary::Arbitrary;

#[cfg_attr(any(feature = "arbitrary", test), derive(Arbitrary))]
#[derive(Debug)]
pub enum Action {
    AppendCountDigit(u8),
    ClearCount,
    EnterNormalMode,
    EnterCommandMode,
    EnterInsertMode,
    EnterSeekMode {
        select: SeekSelect,
        include: SeekInclude,
        direction: SeekDirection,
    },
    EnterReplaceMode,
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
    GotoMoveToLine,
    GotoMoveToStart,
    GotoExtendToStart,
    GotoMoveToBottom,
    GotoExtendToBottom,
    GotoMoveToEnd,
    GotoExtendToEnd,
    GotoMoveToLineStart,
    GotoExtendToLineStart,
    GotoMoveToLineNonBlankStart,
    GotoExtendToLineNonBlankStart,
    GotoMoveUntilLineEnd,
    GotoExtendUntilLineEnd,
    KeepPrimary,
    RotatePrimaryBackward,
    RotatePrimaryForward,
    Reduce,
    Flip,
    FlipForward,
    SelectAll,
    ExpandToFullLines,
    SplitIntoLines,
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
        "r" => vec![EnterReplaceMode],
        // TODO: With a count, `g` runs `GotoLine` instead of entering goto mode.
        "gk" => vec![GotoMoveToStart],
        "gK" => vec![GotoExtendToStart],
        "gj" => vec![GotoMoveToBottom],
        "gJ" => vec![GotoExtendToBottom],
        "ge" => vec![GotoMoveToEnd],
        "gE" => vec![GotoExtendToEnd],
        "gh" => vec![GotoMoveToLineStart],
        "gH" => vec![GotoExtendToLineStart],
        "gi" => vec![GotoMoveToLineNonBlankStart],
        "gI" => vec![GotoExtendToLineNonBlankStart],
        "gl" => vec![GotoMoveUntilLineEnd],
        "gL" => vec![GotoExtendUntilLineEnd],
        "," => vec![KeepPrimary],
        "(" => vec![RotatePrimaryBackward],
        ")" => vec![RotatePrimaryForward],
        ";" => vec![Reduce],
        "<a-;>" => vec![Flip],
        "<a-:>" => vec![FlipForward],
        "%" => vec![SelectAll],
        "x" => vec![ExpandToFullLines],
        "s" => vec![SelectRegex],
        "<a-s>" => vec![SplitIntoLines],
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
        _keys => {
            KeymapResult::Fallback(vec![ClearCount])
        },
    }
});

pub fn handle_actions(editor: &mut Editor, actions: &[Action]) {
    for action in actions {
        handle_action(editor, action);
    }
}

pub fn handle_action(editor: &mut Editor, action: &Action) {
    match action {
        Action::AppendCountDigit(digit) => {
            let digit = usize::from(*digit);
            let old_count = editor.count.map_or(0, |count| usize::from(count));
            let new_count = NonZeroUsize::new(old_count.saturating_mul(10).saturating_add(digit));
            editor.count = new_count;
        }
        Action::ClearCount => editor.count = None,
        Action::EnterNormalMode => enter(editor),
        Action::EnterCommandMode => enter_command_mode(editor),
        Action::EnterInsertMode => insert::enter(editor),
        Action::EnterSeekMode {
            select,
            include,
            direction,
        } => {
            seek::enter(editor, *select, *include, *direction);
        }
        Action::EnterReplaceMode => replace::enter(editor),
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
        Action::GotoMoveToLine => goto_move_to_line(editor),
        Action::GotoMoveToStart => goto_move_to_start(editor),
        Action::GotoExtendToStart => goto_extend_to_start(editor),
        Action::GotoMoveToBottom => goto_move_to_bottom(editor),
        Action::GotoExtendToBottom => goto_extend_to_bottom(editor),
        Action::GotoMoveToEnd => goto_move_to_end(editor),
        Action::GotoExtendToEnd => goto_extend_to_end(editor),
        Action::GotoMoveToLineStart => goto_move_to_line_start(editor),
        Action::GotoExtendToLineStart => goto_extend_to_line_start(editor),
        Action::GotoMoveToLineNonBlankStart => goto_move_to_line_non_blank_start(editor),
        Action::GotoExtendToLineNonBlankStart => goto_extend_to_line_non_blank_start(editor),
        Action::GotoMoveUntilLineEnd => goto_move_until_line_end(editor),
        Action::GotoExtendUntilLineEnd => goto_extend_until_line_end(editor),
        Action::KeepPrimary => keep_primary(editor),
        Action::RotatePrimaryBackward => rotate_primary_backward(editor),
        Action::RotatePrimaryForward => rotate_primary_forward(editor),
        Action::Reduce => reduce(editor),
        Action::Flip => flip(editor),
        Action::FlipForward => flip_forward(editor),
        Action::SelectAll => select_all(editor),
        Action::ExpandToFullLines => expand_to_full_lines(editor),
        Action::SplitIntoLines => split_into_lines(editor),
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

pub fn handle_keys(editor: &mut Editor) -> bool {
    match KEYMAP.get_keys(&editor.pending_keys) {
        KeymapResult::Mapped(actions) => {
            editor.pending_keys.clear();
            handle_actions(editor, actions);
            true
        }
        KeymapResult::Fallback(actions) => {
            editor.pending_keys.clear();
            handle_actions(editor, &actions);
            true
        }
        KeymapResult::Unmapped => {
            editor.pending_keys.clear();
            false
        }
        KeymapResult::Pending => true,
    }
}

pub fn enter(editor: &mut Editor) {
    editor.focused_buffer_mut().text.commit();
    editor.mode = Mode::Normal;
    editor.count = None;
}

fn extend_left(editor: &mut Editor) {
    let count = editor.count.unwrap_or(NonZeroUsize::MIN).get();
    let mut window = editor.focused_window_mut();
    window
        .selection_mut()
        .for_each_mut(|mut range| range.extend_left(count));
    window.scroll_to_selection();
    editor.count = None;
}

fn move_left(editor: &mut Editor) {
    let count = editor.count.unwrap_or(NonZeroUsize::MIN).get();
    let mut window = editor.focused_window_mut();
    window
        .selection_mut()
        .for_each_mut(|mut range| range.move_left(count));
    window.scroll_to_selection();
    editor.count = None;
}

fn extend_right(editor: &mut Editor) {
    let count = editor.count.unwrap_or(NonZeroUsize::MIN).get();
    let mut window = editor.focused_window_mut();
    window
        .selection_mut()
        .for_each_mut(|mut range| range.extend_right(count));
    window.scroll_to_selection();
    editor.count = None;
}

fn move_right(editor: &mut Editor) {
    let count = editor.count.unwrap_or(NonZeroUsize::MIN).get();
    let mut window = editor.focused_window_mut();
    window
        .selection_mut()
        .for_each_mut(|mut range| range.move_right(count));
    window.scroll_to_selection();
    editor.count = None;
}

fn extend_up(editor: &mut Editor) {
    let count = editor.count.unwrap_or(NonZeroUsize::MIN).get();
    let mut window = editor.focused_window_mut();
    window
        .selection_mut()
        .for_each_mut(|mut range| range.extend_up(count));
    window.scroll_to_selection();
    editor.count = None;
}

fn move_up(editor: &mut Editor) {
    let count = editor.count.unwrap_or(NonZeroUsize::MIN).get();
    let mut window = editor.focused_window_mut();
    window
        .selection_mut()
        .for_each_mut(|mut range| range.move_up(count));
    window.scroll_to_selection();
    editor.count = None;
}

fn extend_down(editor: &mut Editor) {
    let count = editor.count.unwrap_or(NonZeroUsize::MIN).get();
    let mut window = editor.focused_window_mut();
    window
        .selection_mut()
        .for_each_mut(|mut range| range.extend_down(count));
    window.scroll_to_selection();
    editor.count = None;
}

fn move_down(editor: &mut Editor) {
    let count = editor.count.unwrap_or(NonZeroUsize::MIN).get();
    let mut window = editor.focused_window_mut();
    window
        .selection_mut()
        .for_each_mut(|mut range| range.move_down(count));
    window.scroll_to_selection();
    editor.count = None;
}

fn flip(editor: &mut Editor) {
    let mut window = editor.focused_window_mut();
    window
        .selection_mut()
        .for_each_mut(|mut range| range.flip());
    window.scroll_to_selection();
    editor.count = None;
}

fn flip_forward(editor: &mut Editor) {
    let mut window = editor.focused_window_mut();
    window
        .selection_mut()
        .for_each_mut(|mut range| range.flip_forward());
    window.scroll_to_selection();
    editor.count = None;
}

fn keep_primary(editor: &mut Editor) {
    let mut window = editor.focused_window_mut();
    window.selection_mut().keep_primary();
    window.scroll_to_selection();
    editor.count = None;
}

fn rotate_primary_backward(editor: &mut Editor) {
    let count = editor.count.unwrap_or(NonZeroUsize::MIN).get();
    let mut window = editor.focused_window_mut();
    window.selection_mut().rotate_primary_backward(count);
    window.scroll_to_selection();
    editor.count = None;
}

fn rotate_primary_forward(editor: &mut Editor) {
    let count = editor.count.unwrap_or(NonZeroUsize::MIN).get();
    let mut window = editor.focused_window_mut();
    window.selection_mut().rotate_primary_forward(count);
    window.scroll_to_selection();
    editor.count = None;
}

fn reduce(editor: &mut Editor) {
    let mut window = editor.focused_window_mut();
    window
        .selection_mut()
        .for_each_mut(|mut range| range.reduce());
    window.scroll_to_selection();
    editor.count = None;
}

fn delete(editor: &mut Editor) {
    if editor.focused_buffer().text.readonly {
        editor.message = Some(Err(String::from("Buffer is readonly")));
        editor.count = None;
        return;
    }
    let mut window = editor.focused_window_mut();
    window.selection_mut().delete();
    window.scroll_to_selection();
    editor.count = None;
}

fn undo(editor: &mut Editor) {
    if editor.focused_buffer().text.readonly {
        editor.message = Some(Err(String::from("Buffer is readonly")));
        editor.count = None;
        return;
    }
    let count = editor.count.unwrap_or(NonZeroUsize::MIN).get();
    let mut window = editor.focused_window_mut();
    for _ in 1..=count {
        if !window.undo().unwrap() {
            break;
        }
    }
    window.scroll_to_selection();
    editor.count = None;
}

fn redo(editor: &mut Editor) {
    if editor.focused_buffer().text.readonly {
        editor.message = Some(Err(String::from("Buffer is readonly")));
        editor.count = None;
        return;
    }
    let count = editor.count.unwrap_or(NonZeroUsize::MIN).get();
    let mut window = editor.focused_window_mut();
    for _ in 1..=count {
        if !window.redo().unwrap() {
            break;
        }
    }
    window.scroll_to_selection();
    editor.count = None;
}

fn select_all(editor: &mut Editor) {
    let mut window = editor.focused_window_mut();
    window.selection_mut().select_all();
    window.scroll_to_selection();
    editor.count = None;
}

fn expand_to_full_lines(editor: &mut Editor) {
    let mut window = editor.focused_window_mut();
    window
        .selection_mut()
        .for_each_mut(|mut range| range.expand_to_full_lines());
    window.scroll_to_selection();
    editor.count = None;
}

fn split_into_lines(editor: &mut Editor) {
    let mut window = editor.focused_window_mut();
    window.selection_mut().split_into_lines();
    window.scroll_to_selection();
    editor.count = None;
}

fn select_regex(editor: &mut Editor) {
    prompt::enter(editor, "select", |editor, regex_str| {
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
        editor.count = None;
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
    editor.mode = Mode::Insert;
    editor.count = None;
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
    editor.mode = Mode::Insert;
    editor.count = None;
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
    editor.mode = Mode::Insert;
    editor.count = None;
}

fn insert_line_above(editor: &mut Editor) {
    if editor.focused_buffer().text.readonly {
        editor.message = Some(Err(String::from("Buffer is readonly")));
        return;
    }
    // TODO: Insert multiple cursors (like Kakoune) when count > 1
    let count = editor.count.unwrap_or(NonZeroUsize::MIN).get();
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
    editor.mode = Mode::Insert;
    editor.count = None;
}

fn insert_line_below(editor: &mut Editor) {
    if editor.focused_buffer().text.readonly {
        editor.message = Some(Err(String::from("Buffer is readonly")));
        return;
    }
    // TODO: Insert multiple cursors (like Kakoune) when count > 1
    let count = editor.count.unwrap_or(NonZeroUsize::MIN).get();
    let mut window = editor.focused_window_mut();
    // TODO: Switch to selection-level insert
    window.selection_mut().for_each_mut(|mut range| {
        range.move_onto_line_end();
        for _ in 0..count {
            range.insert_char('\n');
        }
    });
    window.scroll_to_selection();
    editor.mode = Mode::Insert;
    editor.count = None;
}

fn add_line_above(editor: &mut Editor) {
    if editor.focused_buffer().text.readonly {
        editor.message = Some(Err(String::from("Buffer is readonly")));
        return;
    }
    let count = editor.count.unwrap_or(NonZeroUsize::MIN).get();
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
    editor.count = None;
}

fn add_line_below(editor: &mut Editor) {
    if editor.focused_buffer().text.readonly {
        editor.message = Some(Err(String::from("Buffer is readonly")));
        return;
    }
    let count = editor.count.unwrap_or(NonZeroUsize::MIN).get();
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
    editor.count = None;
}

fn goto_move_to_line(editor: &mut Editor) {
    let count = editor.count.unwrap_or(NonZeroUsize::MIN).get();
    let mut window = editor.focused_window_mut();
    let rope = window.buffer().text.rope().clone();
    let len_lines = rope.len_lines_indigo();
    if len_lines == 0 {
        editor.count = None;
        return;
    }
    let line_index = min(count - 1, len_lines - 1);
    let byte_offset = rope.line_to_byte_idx(line_index, LINE_TYPE);
    window
        .selection_mut()
        .for_each_mut(|mut range| range.move_to(byte_offset));
    window.scroll_to_selection();
    editor.count = None;
}

fn goto_move_to_start(editor: &mut Editor) {
    editor
        .focused_window_mut()
        .selection_mut()
        .for_each_mut(|mut range| range.move_to_start());
    editor.focused_window_mut().scroll_to_selection();
    editor.count = None;
}

fn goto_extend_to_start(editor: &mut Editor) {
    editor
        .focused_window_mut()
        .selection_mut()
        .for_each_mut(|mut range| range.extend_to_start());
    editor.focused_window_mut().scroll_to_selection();
    editor.count = None;
}

fn goto_move_to_bottom(editor: &mut Editor) {
    editor
        .focused_window_mut()
        .selection_mut()
        .for_each_mut(|mut range| range.move_to_bottom());
    editor.focused_window_mut().scroll_to_selection();
    editor.count = None;
}

fn goto_extend_to_bottom(editor: &mut Editor) {
    editor
        .focused_window_mut()
        .selection_mut()
        .for_each_mut(|mut range| range.extend_to_bottom());
    editor.focused_window_mut().scroll_to_selection();
    editor.count = None;
}

fn goto_move_to_end(editor: &mut Editor) {
    editor
        .focused_window_mut()
        .selection_mut()
        .for_each_mut(|mut range| range.move_to_end());
    editor.focused_window_mut().scroll_to_selection();
    editor.count = None;
}

fn goto_extend_to_end(editor: &mut Editor) {
    editor
        .focused_window_mut()
        .selection_mut()
        .for_each_mut(|mut range| range.extend_to_end());
    editor.focused_window_mut().scroll_to_selection();
    editor.count = None;
}

fn goto_move_to_line_start(editor: &mut Editor) {
    editor
        .focused_window_mut()
        .selection_mut()
        .for_each_mut(|mut range| range.move_to_line_start());
    editor.count = None;
}

fn goto_extend_to_line_start(editor: &mut Editor) {
    editor
        .focused_window_mut()
        .selection_mut()
        .for_each_mut(|mut range| range.extend_to_line_start());
    editor.count = None;
}

fn goto_move_to_line_non_blank_start(editor: &mut Editor) {
    editor
        .focused_window_mut()
        .selection_mut()
        .for_each_mut(|mut range| range.move_to_line_non_blank_start());
    editor.count = None;
}

fn goto_extend_to_line_non_blank_start(editor: &mut Editor) {
    editor
        .focused_window_mut()
        .selection_mut()
        .for_each_mut(|mut range| range.extend_to_line_non_blank_start());
    editor.count = None;
}

fn goto_move_until_line_end(editor: &mut Editor) {
    editor
        .focused_window_mut()
        .selection_mut()
        .for_each_mut(|mut range| range.move_until_line_end());
    editor.count = None;
}

fn goto_extend_until_line_end(editor: &mut Editor) {
    editor
        .focused_window_mut()
        .selection_mut()
        .for_each_mut(|mut range| range.extend_until_line_end());
    editor.count = None;
}
