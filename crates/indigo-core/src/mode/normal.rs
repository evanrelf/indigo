#![allow(clippy::enum_glob_use)]

use crate::{
    keymap::{Keymap, KeymapResult, keymap},
    mode::{
        Mode,
        command::enter_command_mode,
        insert, prompt, replace,
        seek::{self, SeekDirection, SeekInclude, SeekSelect},
    },
    rope::{LINE_TYPE, RopeExt as _},
    selection::PasteMode,
    session::Session,
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

pub fn handle_actions(session: &mut Session, actions: &[Action]) {
    for action in actions {
        handle_action(session, action);
    }
}

pub fn handle_action(session: &mut Session, action: &Action) {
    match action {
        Action::AppendCountDigit(digit) => {
            let digit = usize::from(*digit);
            let old_count = session.count.map_or(0, |count| usize::from(count));
            let new_count = NonZeroUsize::new(old_count.saturating_mul(10).saturating_add(digit));
            session.count = new_count;
        }
        Action::ClearCount => session.count = None,
        Action::Commit => session.focused_buffer_mut().text.commit(),
        Action::EnterNormalMode => enter(session),
        Action::EnterCommandMode => enter_command_mode(session),
        Action::EnterInsertMode => insert::enter(session),
        Action::EnterSeekMode {
            select,
            include,
            direction,
        } => {
            seek::enter(session, *select, *include, *direction);
        }
        Action::EnterReplaceMode => replace::enter(session),
        Action::InsertAfterHead => insert_after_head(session),
        Action::InsertAtLineNonBlankStart => insert_at_line_non_blank_start(session),
        Action::InsertAtLineEnd => insert_at_line_end(session),
        Action::InsertLineAbove => insert_line_above(session),
        Action::InsertLineBelow => insert_line_below(session),
        Action::AddLineAbove => add_line_above(session),
        Action::AddLineBelow => add_line_below(session),
        Action::MoveLeft => move_left(session),
        Action::MoveRight => move_right(session),
        Action::MoveUp => move_up(session),
        Action::MoveDown => move_down(session),
        Action::ExtendLeft => extend_left(session),
        Action::ExtendRight => extend_right(session),
        Action::ExtendUp => extend_up(session),
        Action::ExtendDown => extend_down(session),
        Action::GotoMoveToLine => goto_move_to_line(session),
        Action::GotoMoveToStart => goto_move_to_start(session),
        Action::GotoExtendToStart => goto_extend_to_start(session),
        Action::GotoMoveToBottom => goto_move_to_bottom(session),
        Action::GotoExtendToBottom => goto_extend_to_bottom(session),
        Action::GotoMoveToEnd => goto_move_to_end(session),
        Action::GotoExtendToEnd => goto_extend_to_end(session),
        Action::GotoMoveToLineStart => goto_move_to_line_start(session),
        Action::GotoExtendToLineStart => goto_extend_to_line_start(session),
        Action::GotoMoveToLineNonBlankStart => goto_move_to_line_non_blank_start(session),
        Action::GotoExtendToLineNonBlankStart => goto_extend_to_line_non_blank_start(session),
        Action::GotoMoveUntilLineEnd => goto_move_until_line_end(session),
        Action::GotoExtendUntilLineEnd => goto_extend_until_line_end(session),
        Action::KeepPrimary => keep_primary(session),
        Action::RotateSelectionBackward => rotate_selection_backward(session),
        Action::RotateSelectionForward => rotate_selection_forward(session),
        Action::Collapse => collapse(session),
        Action::Flip => flip(session),
        Action::FlipForward => flip_forward(session),
        Action::SelectAll => select_all(session),
        Action::ExpandToFullLines => expand_to_full_lines(session),
        Action::SplitIntoLines => split_into_lines(session),
        Action::SelectRegex => select_regex(session),
        Action::Delete => delete(session),
        Action::Yank => yank(session),
        Action::PasteAfter => paste(session, PasteMode::After),
        Action::PasteBefore => paste(session, PasteMode::Before),
        Action::PasteReplace => paste(session, PasteMode::Replace),
        Action::PasteAllAfter => paste_all(session, PasteMode::After),
        Action::PasteAllBefore => paste_all(session, PasteMode::Before),
        Action::PasteAllReplace => paste_all(session, PasteMode::Replace),
        Action::Undo => undo(session),
        Action::Redo => redo(session),
        Action::ScrollHalfPageUp => scroll_half_page_up(session),
        Action::ScrollHalfPageDown => scroll_half_page_down(session),
        Action::ScrollFullPageUp => scroll_full_page_up(session),
        Action::ScrollFullPageDown => scroll_full_page_down(session),
    }
}

pub fn handle_keys(session: &mut Session) -> bool {
    match KEYMAP.get_keys(&session.pending_keys) {
        KeymapResult::Mapped(actions) => {
            session.pending_keys.clear();
            handle_actions(session, actions);
            true
        }
        KeymapResult::Fallback(actions) => {
            session.pending_keys.clear();
            handle_actions(session, &actions);
            true
        }
        KeymapResult::Unmapped => {
            session.pending_keys.clear();
            false
        }
        KeymapResult::Pending => true,
    }
}

pub fn enter(session: &mut Session) {
    session.focused_buffer_mut().text.commit();
    session.mode = Mode::Normal;
    session.count = None;
}

fn extend_left(session: &mut Session) {
    let count = session.count.take().unwrap_or(NonZeroUsize::MIN).get();
    let mut window = session.focused_window_mut();
    window
        .selection_move()
        .for_each_move(|mut range| range.extend_left(count));
    window.selection_move().merge_overlapping();
    window.scroll_to_selection();
}

fn move_left(session: &mut Session) {
    let count = session.count.take().unwrap_or(NonZeroUsize::MIN).get();
    let mut window = session.focused_window_mut();
    window
        .selection_move()
        .for_each_move(|mut range| range.move_left(count));
    window.selection_move().merge_overlapping();
    window.scroll_to_selection();
}

fn extend_right(session: &mut Session) {
    let count = session.count.take().unwrap_or(NonZeroUsize::MIN).get();
    let mut window = session.focused_window_mut();
    window
        .selection_move()
        .for_each_move(|mut range| range.extend_right(count));
    window.selection_move().merge_overlapping();
    window.scroll_to_selection();
}

fn move_right(session: &mut Session) {
    let count = session.count.take().unwrap_or(NonZeroUsize::MIN).get();
    let mut window = session.focused_window_mut();
    window
        .selection_move()
        .for_each_move(|mut range| range.move_right(count));
    window.selection_move().merge_overlapping();
    window.scroll_to_selection();
}

fn extend_up(session: &mut Session) {
    let count = session.count.take().unwrap_or(NonZeroUsize::MIN).get();
    let mut window = session.focused_window_mut();
    window
        .selection_move()
        .for_each_move(|mut range| range.extend_up(count));
    window.selection_move().merge_overlapping();
    window.scroll_to_selection();
}

fn move_up(session: &mut Session) {
    let count = session.count.take().unwrap_or(NonZeroUsize::MIN).get();
    let mut window = session.focused_window_mut();
    window
        .selection_move()
        .for_each_move(|mut range| range.move_up(count));
    window.selection_move().merge_overlapping();
    window.scroll_to_selection();
}

fn extend_down(session: &mut Session) {
    let count = session.count.take().unwrap_or(NonZeroUsize::MIN).get();
    let mut window = session.focused_window_mut();
    window
        .selection_move()
        .for_each_move(|mut range| range.extend_down(count));
    window.selection_move().merge_overlapping();
    window.scroll_to_selection();
}

fn move_down(session: &mut Session) {
    let count = session.count.take().unwrap_or(NonZeroUsize::MIN).get();
    let mut window = session.focused_window_mut();
    window
        .selection_move()
        .for_each_move(|mut range| range.move_down(count));
    window.selection_move().merge_overlapping();
    window.scroll_to_selection();
}

fn flip(session: &mut Session) {
    let mut window = session.focused_window_mut();
    window
        .selection_move()
        .for_each_move(|mut range| range.flip());
    window.scroll_to_selection();
    session.count = None;
}

fn flip_forward(session: &mut Session) {
    let mut window = session.focused_window_mut();
    window
        .selection_move()
        .for_each_move(|mut range| range.flip_forward());
    window.scroll_to_selection();
    session.count = None;
}

fn keep_primary(session: &mut Session) {
    let mut window = session.focused_window_mut();
    window.selection_move().keep_primary();
    window.scroll_to_selection();
    session.count = None;
}

fn rotate_selection_backward(session: &mut Session) {
    let count = session.count.take().unwrap_or(NonZeroUsize::MIN).get();
    let mut window = session.focused_window_mut();
    window.selection_move().rotate_backward(count);
    window.scroll_to_selection();
}

fn rotate_selection_forward(session: &mut Session) {
    let count = session.count.take().unwrap_or(NonZeroUsize::MIN).get();
    let mut window = session.focused_window_mut();
    window.selection_move().rotate_forward(count);
    window.scroll_to_selection();
}

fn collapse(session: &mut Session) {
    let mut window = session.focused_window_mut();
    window
        .selection_move()
        .for_each_move(|mut range| range.collapse());
    window.scroll_to_selection();
    session.count = None;
}

fn delete(session: &mut Session) {
    if session.focused_buffer().text.readonly {
        session.message = Some(Err(String::from("Buffer is readonly")));
        session.count = None;
        return;
    }
    let mut window = session.focused_window_mut();
    window.selection_edit().delete();
    window.scroll_to_selection();
    session.count = None;
}

fn yank(session: &mut Session) {
    let values = session
        .focused_window()
        .selection()
        .contents()
        .map(String::from)
        .collect();
    session.registers.insert('"', values);
    session.count = None;
}

fn paste(session: &mut Session, paste_mode: PasteMode) {
    let count = session.count.take().unwrap_or(NonZeroUsize::MIN).get();
    if session.focused_buffer().text.readonly {
        session.message = Some(Err(String::from("Buffer is readonly")));
        return;
    }
    let count = if paste_mode == PasteMode::Replace {
        1
    } else {
        count
    };
    let values = session.registers.get(&'"').cloned().unwrap_or_default();
    let mut window = session.focused_window_mut();
    for _ in 1..=count {
        window.selection_edit().paste(&values, paste_mode);
    }
    window.scroll_to_selection();
}

fn paste_all(session: &mut Session, paste_mode: PasteMode) {
    session.count = None;
    if session.focused_buffer().text.readonly {
        session.message = Some(Err(String::from("Buffer is readonly")));
        return;
    }
    let values = session.registers.get(&'"').cloned().unwrap_or_default();
    if values.is_empty() {
        session.message = Some(Err(String::from("Nothing to paste")));
        return;
    }
    let mut window = session.focused_window_mut();
    window.selection_edit().paste_all(&values, paste_mode);
    window.scroll_to_selection();
}

fn undo(session: &mut Session) {
    let count = session.count.take().unwrap_or(NonZeroUsize::MIN).get();
    if session.focused_buffer().text.readonly {
        session.message = Some(Err(String::from("Buffer is readonly")));
        return;
    }
    let mut window = session.focused_window_mut();
    for _ in 1..=count {
        if !window.undo().unwrap() {
            break;
        }
    }
    window.scroll_to_selection();
}

fn redo(session: &mut Session) {
    let count = session.count.take().unwrap_or(NonZeroUsize::MIN).get();
    if session.focused_buffer().text.readonly {
        session.message = Some(Err(String::from("Buffer is readonly")));
        return;
    }
    let mut window = session.focused_window_mut();
    for _ in 1..=count {
        if !window.redo().unwrap() {
            break;
        }
    }
    window.scroll_to_selection();
}

fn select_all(session: &mut Session) {
    let mut window = session.focused_window_mut();
    window.selection_move().select_all();
    window.scroll_to_selection();
    session.count = None;
}

fn expand_to_full_lines(session: &mut Session) {
    let mut window = session.focused_window_mut();
    window
        .selection_move()
        .for_each_move(|mut range| range.expand_to_full_lines());
    window.selection_move().merge_overlapping();
    window.scroll_to_selection();
    session.count = None;
}

fn split_into_lines(session: &mut Session) {
    let mut window = session.focused_window_mut();
    window.selection_move().split_into_lines();
    window.scroll_to_selection();
    session.count = None;
}

fn select_regex(session: &mut Session) {
    prompt::enter(session, "select", |session, regex_str| {
        if regex_str.is_empty() {
            session.message = Some(Err(String::from("Nothing selected")));
            session.count = None;
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
            let matched = session
                .focused_window_mut()
                .selection_move()
                .select_regex(&regex);
            if !matched {
                session.message = Some(Err(String::from("Nothing selected")));
            }
        } else {
            session.message = Some(Err(String::from("Invalid regex")));
        }
        session.count = None;
    });
}

fn insert_after_head(session: &mut Session) {
    if session.focused_buffer().text.readonly {
        session.message = Some(Err(String::from("Buffer is readonly")));
        session.count = None;
        return;
    }
    let mut window = session.focused_window_mut();
    window.selection_edit().for_each_edit(|mut range| {
        range.prepare_append();
    });
    window.scroll_to_selection();
    session.mode = Mode::Insert;
    session.count = None;
}

fn insert_at_line_non_blank_start(session: &mut Session) {
    if session.focused_buffer().text.readonly {
        session.message = Some(Err(String::from("Buffer is readonly")));
        session.count = None;
        return;
    }
    let mut window = session.focused_window_mut();
    window.selection_move().for_each_move(|mut range| {
        range.move_to_line_non_blank_start();
    });
    window.scroll_to_selection();
    session.mode = Mode::Insert;
    session.count = None;
}

fn insert_at_line_end(session: &mut Session) {
    if session.focused_buffer().text.readonly {
        session.message = Some(Err(String::from("Buffer is readonly")));
        session.count = None;
        return;
    }
    let mut window = session.focused_window_mut();
    window.selection_move().for_each_move(|mut range| {
        range.move_onto_line_end();
    });
    window.scroll_to_selection();
    session.mode = Mode::Insert;
    session.count = None;
}

fn insert_line_above(session: &mut Session) {
    // TODO: Insert multiple cursors (like Kakoune) when count > 1
    let count = session.count.take().unwrap_or(NonZeroUsize::MIN).get();
    if session.focused_buffer().text.readonly {
        session.message = Some(Err(String::from("Buffer is readonly")));
        return;
    }
    let mut window = session.focused_window_mut();
    // TODO: Switch to selection-level insert
    window.selection_edit().for_each_edit(|mut range| {
        range.move_to_line_start();
        for _ in 0..count {
            range.insert_char('\n');
        }
        range.move_left(1);
    });
    window.scroll_to_selection();
    session.mode = Mode::Insert;
}

fn insert_line_below(session: &mut Session) {
    // TODO: Insert multiple cursors (like Kakoune) when count > 1
    let count = session.count.take().unwrap_or(NonZeroUsize::MIN).get();
    if session.focused_buffer().text.readonly {
        session.message = Some(Err(String::from("Buffer is readonly")));
        return;
    }
    let mut window = session.focused_window_mut();
    // TODO: Switch to selection-level insert
    window.selection_edit().for_each_edit(|mut range| {
        range.move_onto_line_end();
        for _ in 0..count {
            range.insert_char('\n');
        }
    });
    window.scroll_to_selection();
    session.mode = Mode::Insert;
}

fn add_line_above(session: &mut Session) {
    let count = session.count.take().unwrap_or(NonZeroUsize::MIN).get();
    if session.focused_buffer().text.readonly {
        session.message = Some(Err(String::from("Buffer is readonly")));
        return;
    }
    let mut window = session.focused_window_mut();
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

fn add_line_below(session: &mut Session) {
    let count = session.count.take().unwrap_or(NonZeroUsize::MIN).get();
    if session.focused_buffer().text.readonly {
        session.message = Some(Err(String::from("Buffer is readonly")));
        return;
    }
    let mut window = session.focused_window_mut();
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

fn goto_move_to_line(session: &mut Session) {
    let count = session.count.take().unwrap_or(NonZeroUsize::MIN).get();
    let mut window = session.focused_window_mut();
    let rope = window.buffer().text.rope().clone();
    let line_index = min(count - 1, rope.len_lines_unix() - 1);
    let byte_index = rope.line_to_byte_idx(line_index, LINE_TYPE);
    window
        .selection_move()
        .for_each_move(|mut range| range.move_to(byte_index));
    window.selection_move().merge_overlapping();
    window.scroll_to_selection();
}

fn goto_move_to_start(session: &mut Session) {
    let mut window = session.focused_window_mut();
    window
        .selection_move()
        .for_each_move(|mut range| range.move_to_start());
    window.selection_move().merge_overlapping();
    window.scroll_to_selection();
    session.count = None;
}

fn goto_extend_to_start(session: &mut Session) {
    let mut window = session.focused_window_mut();
    window
        .selection_move()
        .for_each_move(|mut range| range.extend_to_start());
    window.selection_move().merge_overlapping();
    window.scroll_to_selection();
    session.count = None;
}

fn goto_move_to_bottom(session: &mut Session) {
    let mut window = session.focused_window_mut();
    window
        .selection_move()
        .for_each_move(|mut range| range.move_to_bottom());
    window.selection_move().merge_overlapping();
    window.scroll_to_selection();
    session.count = None;
}

fn goto_extend_to_bottom(session: &mut Session) {
    let mut window = session.focused_window_mut();
    window
        .selection_move()
        .for_each_move(|mut range| range.extend_to_bottom());
    window.selection_move().merge_overlapping();
    window.scroll_to_selection();
    session.count = None;
}

fn goto_move_to_end(session: &mut Session) {
    let mut window = session.focused_window_mut();
    window
        .selection_move()
        .for_each_move(|mut range| range.move_to_end());
    window.selection_move().merge_overlapping();
    window.scroll_to_selection();
    session.count = None;
}

fn goto_extend_to_end(session: &mut Session) {
    let mut window = session.focused_window_mut();
    window
        .selection_move()
        .for_each_move(|mut range| range.extend_to_end());
    window.selection_move().merge_overlapping();
    window.scroll_to_selection();
    session.count = None;
}

fn goto_move_to_line_start(session: &mut Session) {
    let mut window = session.focused_window_mut();
    window
        .selection_move()
        .for_each_move(|mut range| range.move_to_line_start());
    window.selection_move().merge_overlapping();
    session.count = None;
}

fn goto_extend_to_line_start(session: &mut Session) {
    let mut window = session.focused_window_mut();
    window
        .selection_move()
        .for_each_move(|mut range| range.extend_to_line_start());
    window.selection_move().merge_overlapping();
    session.count = None;
}

fn goto_move_to_line_non_blank_start(session: &mut Session) {
    let mut window = session.focused_window_mut();
    window
        .selection_move()
        .for_each_move(|mut range| range.move_to_line_non_blank_start());
    window.selection_move().merge_overlapping();
    session.count = None;
}

fn goto_extend_to_line_non_blank_start(session: &mut Session) {
    let mut window = session.focused_window_mut();
    window
        .selection_move()
        .for_each_move(|mut range| range.extend_to_line_non_blank_start());
    window.selection_move().merge_overlapping();
    session.count = None;
}

fn goto_move_until_line_end(session: &mut Session) {
    let mut window = session.focused_window_mut();
    window
        .selection_move()
        .for_each_move(|mut range| range.move_until_line_end());
    window.selection_move().merge_overlapping();
    session.count = None;
}

fn goto_extend_until_line_end(session: &mut Session) {
    let mut window = session.focused_window_mut();
    window
        .selection_move()
        .for_each_move(|mut range| range.extend_until_line_end());
    window.selection_move().merge_overlapping();
    session.count = None;
}
