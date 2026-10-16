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
    selection::PasteMode,
    window::{
        scroll_full_page_down, scroll_full_page_up, scroll_half_page_down, scroll_half_page_up,
    },
};
use regex_cursor::engines::meta::Regex;
use std::{cmp::min, num::NonZeroUsize, sync::LazyLock};

#[cfg(feature = "arbitrary")]
use arbitrary::Arbitrary;

#[cfg_attr(feature = "arbitrary", derive(Arbitrary))]
#[derive(Debug)]
pub enum Action {
    AppendCountDigit(u8),
    ClearCount,
    Commit,
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
    RotateSelectionBackward,
    RotateSelectionForward,
    Collapse,
    Flip,
    FlipForward,
    SelectAll,
    ExpandToFullLines,
    SplitIntoLines,
    SelectRegex,
    Delete,
    Yank,
    PasteAfter,
    PasteBefore,
    PasteReplace,
    PasteAllAfter,
    PasteAllBefore,
    PasteAllReplace,
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
        "<esc>" => vec![EnterNormalMode],

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

        "h" => vec![MoveLeft], /// Move left
        "l" => vec![MoveRight], /// Move right
        "k" => vec![MoveUp], /// Move up
        "j" => vec![MoveDown], /// Move down
        "H" => vec![ExtendLeft], /// Extend left
        "L" => vec![ExtendRight], /// Extend right
        "K" => vec![ExtendUp], /// Extend up
        "J" => vec![ExtendDown], /// Extend down

        "i" => vec![EnterInsertMode], /// Enter insert mode at cursor
        "a" => vec![InsertAfterHead], /// Enter insert mode after cursor
        "I" => vec![InsertAtLineNonBlankStart], /// Enter insert mode at first non-blank
        "A" => vec![InsertAtLineEnd], /// Enter insert mode at line end

        "r" => vec![EnterReplaceMode], /// Replace characters
        "y" => vec![Yank], /// Yank
        "c" => vec![Yank, Delete, EnterInsertMode], /// Change
        "d" => vec![Yank, Delete, Commit], /// Delete
        "<a-c>" => vec![Delete, EnterInsertMode], /// Change without yanking
        "<a-d>" => vec![Delete, Commit], /// Delete without yanking

        "p" => vec![PasteAfter, Commit], /// Paste after
        "P" => vec![PasteBefore, Commit], /// Paste before
        "R" => vec![PasteReplace, Commit], /// Paste replacing selection
        "<a-p>" => vec![PasteAllAfter, Commit], /// Paste all after
        "<a-P>" => vec![PasteAllBefore, Commit], /// Paste all before
        "<a-R>" => vec![PasteAllReplace, Commit], /// Paste all replacing selection

        "u" => vec![Undo], /// Undo
        "U" => vec![Redo], /// Redo

        // TODO: With a count, `g` runs `GotoLine` instead of entering goto mode.
        "gk" => vec![GotoMoveToStart], /// Move to buffer start
        "gj" => vec![GotoMoveToBottom], /// Move to last line
        "ge" => vec![GotoMoveToEnd], /// Move to buffer end
        "gh" => vec![GotoMoveToLineStart], /// Move to line start
        "gi" => vec![GotoMoveToLineNonBlankStart], /// Move to first non-blank
        "gl" => vec![GotoMoveUntilLineEnd], /// Move to line end
        "gK" => vec![GotoExtendToStart], /// Extend to buffer start
        "gJ" => vec![GotoExtendToBottom], /// Extend to last line
        "gE" => vec![GotoExtendToEnd], /// Extend to buffer end
        "gH" => vec![GotoExtendToLineStart], /// Extend to line start
        "gI" => vec![GotoExtendToLineNonBlankStart], /// Extend to first non-blank
        "gL" => vec![GotoExtendUntilLineEnd], /// Extend to line end

        "f" => vec![EnterSeekMode { select: Move, include: Onto, direction: Next }], /// Move onto next character
        "<a-f>" => vec![EnterSeekMode { select: Move, include: Onto, direction: Prev }], /// Move onto previous character
        "t" => vec![EnterSeekMode { select: Move, include: Until, direction: Next }], /// Move until next character
        "<a-t>" => vec![EnterSeekMode { select: Move, include: Until, direction: Prev }], /// Move until previous character
        "F" => vec![EnterSeekMode { select: Extend, include: Onto, direction: Next }], /// Extend onto next character
        "<a-F>" => vec![EnterSeekMode { select: Extend, include: Onto, direction: Prev }], /// Extend onto previous character
        "T" => vec![EnterSeekMode { select: Extend, include: Until, direction: Next }], /// Extend until next character
        "<a-T>" => vec![EnterSeekMode { select: Extend, include: Until, direction: Prev }], /// Extend until previous character

        "%" => vec![SelectAll], /// Select all
        "s" => vec![SelectRegex], /// Select regex matches
        "<a-s>" => vec![SplitIntoLines], /// Split into lines
        "x" => vec![ExpandToFullLines], /// Expand to full lines
        "<a-;>" => vec![Flip], /// Flip selection
        "<a-:>" => vec![FlipForward], /// Flip selection forward
        "(" => vec![RotateSelectionBackward], /// Rotate selection backward
        ")" => vec![RotateSelectionForward], /// Rotate selection forward
        "," => vec![KeepPrimary], /// Keep only primary selection range
        ";" => vec![Collapse], /// Collapse selection ranges

        "o" => vec![InsertLineBelow], /// Enter insert mode on new line below
        "O" => vec![InsertLineAbove], /// Enter insert mode on new line above
        "<a-o>" => vec![AddLineBelow, Commit], /// Add line below
        "<a-O>" => vec![AddLineAbove, Commit], /// Add line above

        ":" => vec![EnterCommandMode], /// Enter command mode

        "<c-u>" => vec![ScrollHalfPageUp], /// Scroll half page up
        "<c-d>" => vec![ScrollHalfPageDown], /// Scroll half page down
        "<c-b>" => vec![ScrollFullPageUp], /// Scroll page up
        "<c-f>" => vec![ScrollFullPageDown], /// Scroll page down

        _keys => {
            KeymapResult::Fallback(vec![ClearCount])
        }, /// Clear count
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
        Action::Commit => editor.focused_buffer_mut().text.commit(),
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
        Action::RotateSelectionBackward => rotate_selection_backward(editor),
        Action::RotateSelectionForward => rotate_selection_forward(editor),
        Action::Collapse => collapse(editor),
        Action::Flip => flip(editor),
        Action::FlipForward => flip_forward(editor),
        Action::SelectAll => select_all(editor),
        Action::ExpandToFullLines => expand_to_full_lines(editor),
        Action::SplitIntoLines => split_into_lines(editor),
        Action::SelectRegex => select_regex(editor),
        Action::Delete => delete(editor),
        Action::Yank => yank(editor),
        Action::PasteAfter => paste(editor, PasteMode::After),
        Action::PasteBefore => paste(editor, PasteMode::Before),
        Action::PasteReplace => paste(editor, PasteMode::Replace),
        Action::PasteAllAfter => paste_all(editor, PasteMode::After),
        Action::PasteAllBefore => paste_all(editor, PasteMode::Before),
        Action::PasteAllReplace => paste_all(editor, PasteMode::Replace),
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
    let count = editor.count.take().unwrap_or(NonZeroUsize::MIN).get();
    let mut window = editor.focused_window_mut();
    window
        .selection_move()
        .for_each_move(|mut range| range.extend_left(count));
    window.selection_move().merge_overlapping();
    window.scroll_to_selection();
}

fn move_left(editor: &mut Editor) {
    let count = editor.count.take().unwrap_or(NonZeroUsize::MIN).get();
    let mut window = editor.focused_window_mut();
    window
        .selection_move()
        .for_each_move(|mut range| range.move_left(count));
    window.selection_move().merge_overlapping();
    window.scroll_to_selection();
}

fn extend_right(editor: &mut Editor) {
    let count = editor.count.take().unwrap_or(NonZeroUsize::MIN).get();
    let mut window = editor.focused_window_mut();
    window
        .selection_move()
        .for_each_move(|mut range| range.extend_right(count));
    window.selection_move().merge_overlapping();
    window.scroll_to_selection();
}

fn move_right(editor: &mut Editor) {
    let count = editor.count.take().unwrap_or(NonZeroUsize::MIN).get();
    let mut window = editor.focused_window_mut();
    window
        .selection_move()
        .for_each_move(|mut range| range.move_right(count));
    window.selection_move().merge_overlapping();
    window.scroll_to_selection();
}

fn extend_up(editor: &mut Editor) {
    let count = editor.count.take().unwrap_or(NonZeroUsize::MIN).get();
    let mut window = editor.focused_window_mut();
    window
        .selection_move()
        .for_each_move(|mut range| range.extend_up(count));
    window.selection_move().merge_overlapping();
    window.scroll_to_selection();
}

fn move_up(editor: &mut Editor) {
    let count = editor.count.take().unwrap_or(NonZeroUsize::MIN).get();
    let mut window = editor.focused_window_mut();
    window
        .selection_move()
        .for_each_move(|mut range| range.move_up(count));
    window.selection_move().merge_overlapping();
    window.scroll_to_selection();
}

fn extend_down(editor: &mut Editor) {
    let count = editor.count.take().unwrap_or(NonZeroUsize::MIN).get();
    let mut window = editor.focused_window_mut();
    window
        .selection_move()
        .for_each_move(|mut range| range.extend_down(count));
    window.selection_move().merge_overlapping();
    window.scroll_to_selection();
}

fn move_down(editor: &mut Editor) {
    let count = editor.count.take().unwrap_or(NonZeroUsize::MIN).get();
    let mut window = editor.focused_window_mut();
    window
        .selection_move()
        .for_each_move(|mut range| range.move_down(count));
    window.selection_move().merge_overlapping();
    window.scroll_to_selection();
}

fn flip(editor: &mut Editor) {
    let mut window = editor.focused_window_mut();
    window
        .selection_move()
        .for_each_move(|mut range| range.flip());
    window.scroll_to_selection();
    editor.count = None;
}

fn flip_forward(editor: &mut Editor) {
    let mut window = editor.focused_window_mut();
    window
        .selection_move()
        .for_each_move(|mut range| range.flip_forward());
    window.scroll_to_selection();
    editor.count = None;
}

fn keep_primary(editor: &mut Editor) {
    let mut window = editor.focused_window_mut();
    window.selection_move().keep_primary();
    window.scroll_to_selection();
    editor.count = None;
}

fn rotate_selection_backward(editor: &mut Editor) {
    let count = editor.count.take().unwrap_or(NonZeroUsize::MIN).get();
    let mut window = editor.focused_window_mut();
    window.selection_move().rotate_backward(count);
    window.scroll_to_selection();
}

fn rotate_selection_forward(editor: &mut Editor) {
    let count = editor.count.take().unwrap_or(NonZeroUsize::MIN).get();
    let mut window = editor.focused_window_mut();
    window.selection_move().rotate_forward(count);
    window.scroll_to_selection();
}

fn collapse(editor: &mut Editor) {
    let mut window = editor.focused_window_mut();
    window
        .selection_move()
        .for_each_move(|mut range| range.collapse());
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
    window.selection_edit().delete();
    window.scroll_to_selection();
    editor.count = None;
}

fn yank(editor: &mut Editor) {
    let values = editor
        .focused_window()
        .selection()
        .contents()
        .map(String::from)
        .collect();
    editor.registers.insert('"', values);
    editor.count = None;
}

fn paste(editor: &mut Editor, paste_mode: PasteMode) {
    let count = editor.count.take().unwrap_or(NonZeroUsize::MIN).get();
    if editor.focused_buffer().text.readonly {
        editor.message = Some(Err(String::from("Buffer is readonly")));
        return;
    }
    let count = if paste_mode == PasteMode::Replace {
        1
    } else {
        count
    };
    let values = editor.registers.get(&'"').cloned().unwrap_or_default();
    let mut window = editor.focused_window_mut();
    for _ in 1..=count {
        window.selection_edit().paste(&values, paste_mode);
    }
    window.scroll_to_selection();
}

fn paste_all(editor: &mut Editor, paste_mode: PasteMode) {
    editor.count = None;
    if editor.focused_buffer().text.readonly {
        editor.message = Some(Err(String::from("Buffer is readonly")));
        return;
    }
    let values = editor.registers.get(&'"').cloned().unwrap_or_default();
    if values.is_empty() {
        editor.message = Some(Err(String::from("Nothing to paste")));
        return;
    }
    let mut window = editor.focused_window_mut();
    window.selection_edit().paste_all(&values, paste_mode);
    window.scroll_to_selection();
}

fn undo(editor: &mut Editor) {
    let count = editor.count.take().unwrap_or(NonZeroUsize::MIN).get();
    if editor.focused_buffer().text.readonly {
        editor.message = Some(Err(String::from("Buffer is readonly")));
        return;
    }
    let mut window = editor.focused_window_mut();
    for _ in 1..=count {
        if !window.undo().unwrap() {
            break;
        }
    }
    window.scroll_to_selection();
}

fn redo(editor: &mut Editor) {
    let count = editor.count.take().unwrap_or(NonZeroUsize::MIN).get();
    if editor.focused_buffer().text.readonly {
        editor.message = Some(Err(String::from("Buffer is readonly")));
        return;
    }
    let mut window = editor.focused_window_mut();
    for _ in 1..=count {
        if !window.redo().unwrap() {
            break;
        }
    }
    window.scroll_to_selection();
}

fn select_all(editor: &mut Editor) {
    let mut window = editor.focused_window_mut();
    window.selection_move().select_all();
    window.scroll_to_selection();
    editor.count = None;
}

fn expand_to_full_lines(editor: &mut Editor) {
    let mut window = editor.focused_window_mut();
    window
        .selection_move()
        .for_each_move(|mut range| range.expand_to_full_lines());
    window.selection_move().merge_overlapping();
    window.scroll_to_selection();
    editor.count = None;
}

fn split_into_lines(editor: &mut Editor) {
    let mut window = editor.focused_window_mut();
    window.selection_move().split_into_lines();
    window.scroll_to_selection();
    editor.count = None;
}

fn select_regex(editor: &mut Editor) {
    prompt::enter(editor, "select", |editor, regex_str| {
        if regex_str.is_empty() {
            editor.message = Some(Err(String::from("Nothing selected")));
            editor.count = None;
            return;
        }
        let regex = Regex::builder()
            .syntax(
                regex_cursor::regex_automata::util::syntax::Config::new()
                    .multi_line(true)
                    .dot_matches_new_line(true),
            )
            .build(regex_str);
        if let Ok(regex) = regex {
            let matched = editor
                .focused_window_mut()
                .selection_move()
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
        editor.count = None;
        return;
    }
    let mut window = editor.focused_window_mut();
    window.selection_edit().for_each_edit(|mut range| {
        range.prepare_append();
    });
    window.scroll_to_selection();
    editor.mode = Mode::Insert;
    editor.count = None;
}

fn insert_at_line_non_blank_start(editor: &mut Editor) {
    if editor.focused_buffer().text.readonly {
        editor.message = Some(Err(String::from("Buffer is readonly")));
        editor.count = None;
        return;
    }
    let mut window = editor.focused_window_mut();
    window.selection_move().for_each_move(|mut range| {
        range.move_to_line_non_blank_start();
    });
    window.scroll_to_selection();
    editor.mode = Mode::Insert;
    editor.count = None;
}

fn insert_at_line_end(editor: &mut Editor) {
    if editor.focused_buffer().text.readonly {
        editor.message = Some(Err(String::from("Buffer is readonly")));
        editor.count = None;
        return;
    }
    let mut window = editor.focused_window_mut();
    window.selection_move().for_each_move(|mut range| {
        range.move_onto_line_end();
    });
    window.scroll_to_selection();
    editor.mode = Mode::Insert;
    editor.count = None;
}

fn insert_line_above(editor: &mut Editor) {
    // TODO: Insert multiple cursors (like Kakoune) when count > 1
    let count = editor.count.take().unwrap_or(NonZeroUsize::MIN).get();
    if editor.focused_buffer().text.readonly {
        editor.message = Some(Err(String::from("Buffer is readonly")));
        return;
    }
    let mut window = editor.focused_window_mut();
    // TODO: Switch to selection-level insert
    window.selection_edit().for_each_edit(|mut range| {
        range.move_to_line_start();
        for _ in 0..count {
            range.insert_char('\n');
        }
        range.move_left(1);
    });
    window.scroll_to_selection();
    editor.mode = Mode::Insert;
}

fn insert_line_below(editor: &mut Editor) {
    // TODO: Insert multiple cursors (like Kakoune) when count > 1
    let count = editor.count.take().unwrap_or(NonZeroUsize::MIN).get();
    if editor.focused_buffer().text.readonly {
        editor.message = Some(Err(String::from("Buffer is readonly")));
        return;
    }
    let mut window = editor.focused_window_mut();
    // TODO: Switch to selection-level insert
    window.selection_edit().for_each_edit(|mut range| {
        range.move_onto_line_end();
        for _ in 0..count {
            range.insert_char('\n');
        }
    });
    window.scroll_to_selection();
    editor.mode = Mode::Insert;
}

fn add_line_above(editor: &mut Editor) {
    let count = editor.count.take().unwrap_or(NonZeroUsize::MIN).get();
    if editor.focused_buffer().text.readonly {
        editor.message = Some(Err(String::from("Buffer is readonly")));
        return;
    }
    let mut window = editor.focused_window_mut();
    // TODO: Switch to selection-level insert
    window.selection_edit().for_each_edit(|mut range| {
        let snapshot = range.save();
        range.move_to_line_start();
        for _ in 0..count {
            range.insert_char('\n');
        }
        assert!(range.restore(&snapshot));
    });
    window.selection_move().merge_overlapping();
    window.scroll_to_selection();
}

fn add_line_below(editor: &mut Editor) {
    let count = editor.count.take().unwrap_or(NonZeroUsize::MIN).get();
    if editor.focused_buffer().text.readonly {
        editor.message = Some(Err(String::from("Buffer is readonly")));
        return;
    }
    let mut window = editor.focused_window_mut();
    // TODO: Switch to selection-level insert
    window.selection_edit().for_each_edit(|mut range| {
        let snapshot = range.save();
        range.move_onto_line_end();
        for _ in 0..count {
            range.insert_char('\n');
        }
        assert!(range.restore(&snapshot));
    });
    window.selection_move().merge_overlapping();
    window.scroll_to_selection();
}

fn goto_move_to_line(editor: &mut Editor) {
    let count = editor.count.take().unwrap_or(NonZeroUsize::MIN).get();
    let mut window = editor.focused_window_mut();
    let rope = window.buffer().text.rope().clone();
    let line_index = min(count - 1, rope.len_lines_unix() - 1);
    let byte_index = rope.line_to_byte_idx(line_index, LINE_TYPE);
    window
        .selection_move()
        .for_each_move(|mut range| range.move_to(byte_index));
    window.selection_move().merge_overlapping();
    window.scroll_to_selection();
}

fn goto_move_to_start(editor: &mut Editor) {
    let mut window = editor.focused_window_mut();
    window
        .selection_move()
        .for_each_move(|mut range| range.move_to_start());
    window.selection_move().merge_overlapping();
    window.scroll_to_selection();
    editor.count = None;
}

fn goto_extend_to_start(editor: &mut Editor) {
    let mut window = editor.focused_window_mut();
    window
        .selection_move()
        .for_each_move(|mut range| range.extend_to_start());
    window.selection_move().merge_overlapping();
    window.scroll_to_selection();
    editor.count = None;
}

fn goto_move_to_bottom(editor: &mut Editor) {
    let mut window = editor.focused_window_mut();
    window
        .selection_move()
        .for_each_move(|mut range| range.move_to_bottom());
    window.selection_move().merge_overlapping();
    window.scroll_to_selection();
    editor.count = None;
}

fn goto_extend_to_bottom(editor: &mut Editor) {
    let mut window = editor.focused_window_mut();
    window
        .selection_move()
        .for_each_move(|mut range| range.extend_to_bottom());
    window.selection_move().merge_overlapping();
    window.scroll_to_selection();
    editor.count = None;
}

fn goto_move_to_end(editor: &mut Editor) {
    let mut window = editor.focused_window_mut();
    window
        .selection_move()
        .for_each_move(|mut range| range.move_to_end());
    window.selection_move().merge_overlapping();
    window.scroll_to_selection();
    editor.count = None;
}

fn goto_extend_to_end(editor: &mut Editor) {
    let mut window = editor.focused_window_mut();
    window
        .selection_move()
        .for_each_move(|mut range| range.extend_to_end());
    window.selection_move().merge_overlapping();
    window.scroll_to_selection();
    editor.count = None;
}

fn goto_move_to_line_start(editor: &mut Editor) {
    let mut window = editor.focused_window_mut();
    window
        .selection_move()
        .for_each_move(|mut range| range.move_to_line_start());
    window.selection_move().merge_overlapping();
    editor.count = None;
}

fn goto_extend_to_line_start(editor: &mut Editor) {
    let mut window = editor.focused_window_mut();
    window
        .selection_move()
        .for_each_move(|mut range| range.extend_to_line_start());
    window.selection_move().merge_overlapping();
    editor.count = None;
}

fn goto_move_to_line_non_blank_start(editor: &mut Editor) {
    let mut window = editor.focused_window_mut();
    window
        .selection_move()
        .for_each_move(|mut range| range.move_to_line_non_blank_start());
    window.selection_move().merge_overlapping();
    editor.count = None;
}

fn goto_extend_to_line_non_blank_start(editor: &mut Editor) {
    let mut window = editor.focused_window_mut();
    window
        .selection_move()
        .for_each_move(|mut range| range.extend_to_line_non_blank_start());
    window.selection_move().merge_overlapping();
    editor.count = None;
}

fn goto_move_until_line_end(editor: &mut Editor) {
    let mut window = editor.focused_window_mut();
    window
        .selection_move()
        .for_each_move(|mut range| range.move_until_line_end());
    window.selection_move().merge_overlapping();
    editor.count = None;
}

fn goto_extend_until_line_end(editor: &mut Editor) {
    let mut window = editor.focused_window_mut();
    window
        .selection_move()
        .for_each_move(|mut range| range.extend_until_line_end());
    window.selection_move().merge_overlapping();
    editor.count = None;
}
