#![allow(clippy::enum_glob_use)]

// TODO: Break out view keys (e.g. `vc`, `V`) into a new `view` mode.

use crate::{
    client::ClientMut,
    mode::{Mode, command::enter_command_mode, insert, prompt},
    selection::PasteMode,
};
use indigo_key::{Key, KeyCode, Keymap, KeymapResult, Mapping, keymap};
use indigo_text::rope::{LINE_TYPE, RopeExt as _};
use regex_cursor::engines::meta::Regex;
use std::{cmp::min, num::NonZeroUsize, sync::Arc};

#[cfg(feature = "arbitrary")]
use arbitrary::Arbitrary;

#[cfg_attr(feature = "arbitrary", derive(Arbitrary))]
#[derive(Debug)]
pub enum Action {
    AppendCountDigit(u8),
    Commit,
    EnterNormalMode,
    EnterCommandMode,
    EnterInsertMode,
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
    Seek {
        select: SeekSelect,
        include: SeekInclude,
        direction: SeekDirection,
        byte: u8,
    },
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
    Replace(u8),
    Undo,
    Redo,
    ScrollHalfPageUp,
    ScrollHalfPageDown,
    ScrollFullPageUp,
    ScrollFullPageDown,
    ScrollCenter,
    Suspend,
    QuitHint,
    #[cfg_attr(feature = "arbitrary", arbitrary(skip))]
    Todo(&'static str),
}

#[cfg_attr(feature = "arbitrary", derive(Arbitrary))]
#[derive(Clone, Copy, Debug)]
pub enum SeekSelect {
    Move,
    Extend,
}

#[cfg_attr(feature = "arbitrary", derive(Arbitrary))]
#[derive(Clone, Copy, Debug)]
pub enum SeekInclude {
    Until,
    Onto,
}

#[cfg_attr(feature = "arbitrary", derive(Arbitrary))]
#[derive(Clone, Copy, Debug)]
pub enum SeekDirection {
    Prev,
    Next,
}

#[expect(clippy::too_many_lines)]
#[must_use]
pub fn keymap() -> Keymap<Vec<Action>> {
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

        "q" => vec![Todo("q")], /// TODO: Move to previous word start
        "w" => vec![Todo("w")], /// TODO: Move to next word start
        "e" => vec![Todo("e")], /// TODO: Move to next word end
        "<a-q>" => vec![Todo("<a-q>")], /// TODO: Move to previous WORD start
        "<a-w>" => vec![Todo("<a-w>")], /// TODO: Move to next WORD start
        "<a-e>" => vec![Todo("<a-e>")], /// TODO: Move to next WORD end
        "Q" => vec![Todo("Q")], /// TODO: Extend to previous word start
        "W" => vec![Todo("W")], /// TODO: Extend to next word start
        "E" => vec![Todo("E")], /// TODO: Extend to next word end
        "<a-Q>" => vec![Todo("<a-Q>")], /// TODO: Extend to previous WORD start
        "<a-W>" => vec![Todo("<a-W>")], /// TODO: Extend to next WORD start
        "<a-E>" => vec![Todo("<a-E>")], /// TODO: Extend to next WORD end

        "m" => vec![Todo("m")], /// TODO: Move to next matching bracket
        "<a-m>" => vec![Todo("<a-m>")], /// TODO: Move to previous matching bracket
        "M" => vec![Todo("M")], /// TODO: Extend to next matching bracket
        "<a-M>" => vec![Todo("<a-M>")], /// TODO: Extend to previous matching bracket

        "i" => vec![EnterInsertMode], /// Enter insert mode at cursor
        "a" => vec![InsertAfterHead], /// Enter insert mode after cursor
        "I" => vec![InsertAtLineNonBlankStart], /// Enter insert mode at first non-blank
        "A" => vec![InsertAtLineEnd], /// Enter insert mode at line end

        "r<any>" => Mapping::Any(|key| Some(vec![Replace(key_char(key)?), Commit])), /// Replace characters
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

        "<a-j>" => vec![Todo("<a-j>")], /// TODO: Join lines
        "<a-J>" => vec![Todo("<a-J>")], /// TODO: Join lines and select spaces

        "\\>" => vec![Todo("\\>")], /// TODO: Indent
        "\\<" => vec![Todo("\\<")], /// TODO: Unindent
        "<a-\\>>" => vec![Todo("<a-\\>>")], /// TODO: Indent including empty lines
        "<a-\\<>" => vec![Todo("<a-\\<>")], /// TODO: Unindent and keep incomplete indent
        "&" => vec![Todo("&")], /// TODO: Align selections
        "<a-&>" => vec![Todo("<a-&>")], /// TODO: Copy indent from primary selection

        "`" => vec![Todo("`")], /// TODO: Convert to lowercase
        "~" => vec![Todo("~")], /// TODO: Convert to uppercase
        "<a-`>" => vec![Todo("<a-`>")], /// TODO: Swap case
        "@" => vec![Todo("@")], /// TODO: Convert tabs to spaces
        "<a-@>" => vec![Todo("<a-@>")], /// TODO: Convert spaces to tabs

        "<a-)>" => vec![Todo("<a-)>")], /// TODO: Rotate selection contents forward
        "<a-(>" => vec![Todo("<a-(>")], /// TODO: Rotate selection contents backward

        "." => vec![Todo(".")], /// TODO: Repeat last insert
        "<a-.>" => vec![Todo("<a-.>")], /// TODO: Repeat last object or seek selection

        "|" => vec![Todo("|")], /// TODO: Pipe through filter and replace
        "<a-|>" => vec![Todo("<a-|>")], /// TODO: Pipe through filter and ignore output
        "$" => vec![Todo("$")], /// TODO: Keep selections where filter exits successfully
        "!" => vec![Todo("!")], /// TODO: Insert command output before
        "<a-!>" => vec![Todo("<a-!>")], /// TODO: Append command output after

        "u" => vec![Undo], /// Undo
        "U" => vec![Redo], /// Redo

        "<a-u>" => vec![Todo("<a-u>")], /// TODO: Undo selection change
        "<a-U>" => vec![Todo("<a-U>")], /// TODO: Redo selection change
        "<c-k>" => vec![Todo("<c-k>")], /// TODO: Move backward in change history
        "<c-j>" => vec![Todo("<c-j>")], /// TODO: Move forward in change history

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
        "gt" => vec![Todo("gt")], /// TODO: Move to first displayed line
        "gc" => vec![Todo("gc")], /// TODO: Move to middle displayed line
        "gb" => vec![Todo("gb")], /// TODO: Move to last displayed line
        "ga" => vec![Todo("ga")], /// TODO: Go to alternate buffer
        "gf" => vec![Todo("gf")], /// TODO: Open file under selection
        "g." => vec![Todo("g.")], /// TODO: Move to last modification

        "f<any>" => Mapping::Any(|key| Some(vec![Seek { select: Move, include: Onto, direction: Next, byte: key_char(key)? }])), /// Move onto next character
        "<a-f><any>" => Mapping::Any(|key| Some(vec![Seek { select: Move, include: Onto, direction: Prev, byte: key_char(key)? }])), /// Move onto previous character
        "t<any>" => Mapping::Any(|key| Some(vec![Seek { select: Move, include: Until, direction: Next, byte: key_char(key)? }])), /// Move until next character
        "<a-t><any>" => Mapping::Any(|key| Some(vec![Seek { select: Move, include: Until, direction: Prev, byte: key_char(key)? }])), /// Move until previous character
        "F<any>" => Mapping::Any(|key| Some(vec![Seek { select: Extend, include: Onto, direction: Next, byte: key_char(key)? }])), /// Extend onto next character
        "<a-F><any>" => Mapping::Any(|key| Some(vec![Seek { select: Extend, include: Onto, direction: Prev, byte: key_char(key)? }])), /// Extend onto previous character
        "T<any>" => Mapping::Any(|key| Some(vec![Seek { select: Extend, include: Until, direction: Next, byte: key_char(key)? }])), /// Extend until next character
        "<a-T><any>" => Mapping::Any(|key| Some(vec![Seek { select: Extend, include: Until, direction: Prev, byte: key_char(key)? }])), /// Extend until previous character

        "<a-a>" => vec![Todo("<a-a>")], /// TODO: Select whole object
        "<a-i>" => vec![Todo("<a-i>")], /// TODO: Select inner object
        "[" => vec![Todo("[")], /// TODO: Move to object start
        "]" => vec![Todo("]")], /// TODO: Move to object end
        "{" => vec![Todo("{")], /// TODO: Extend to object start
        "}" => vec![Todo("}")], /// TODO: Extend to object end
        "<a-[>" => vec![Todo("<a-[>")], /// TODO: Move to inner object start
        "<a-]>" => vec![Todo("<a-]>")], /// TODO: Move to inner object end
        "<a-{>" => vec![Todo("<a-{>")], /// TODO: Extend to inner object start
        "<a-}>" => vec![Todo("<a-}>")], /// TODO: Extend to inner object end

        "/" => vec![Todo("/")], /// TODO: Search forward
        "<a-/>" => vec![Todo("<a-/>")], /// TODO: Search backward
        "?" => vec![Todo("?")], /// TODO: Extend to next match
        "<a-?>" => vec![Todo("<a-?>")], /// TODO: Extend to previous match
        "n" => vec![Todo("n")], /// TODO: Select next match
        "<a-n>" => vec![Todo("<a-n>")], /// TODO: Select previous match
        "N" => vec![Todo("N")], /// TODO: Add next match to selection
        "<a-N>" => vec![Todo("<a-N>")], /// TODO: Add previous match to selection
        "*" => vec![Todo("*")], /// TODO: Set search pattern from selection
        "<a-*>" => vec![Todo("<a-*>")], /// TODO: Set search pattern from selection verbatim

        "%" => vec![SelectAll], /// Select all
        "s" => vec![SelectRegex], /// Select regex matches
        "<a-s>" => vec![SplitIntoLines], /// Split into lines
        "S" => vec![Todo("S")], /// TODO: Split on regex
        "<a-S>" => vec![Todo("<a-S>")], /// TODO: Select first and last characters
        "<a-k>" => vec![Todo("<a-k>")], /// TODO: Keep selection ranges matching regex
        "<a-K>" => vec![Todo("<a-K>")], /// TODO: Remove selection ranges matching regex

        "C" => vec![Todo("C")], /// TODO: Copy selection ranges down
        "<a-C>" => vec![Todo("<a-C>")], /// TODO: Copy selection ranges up
        "+" => vec![Todo("+")], /// TODO: Duplicate each selection range
        "<a-+>" => vec![Todo("<a-+>")], /// TODO: Merge overlapping selection ranges
        "<a-_>" => vec![Todo("<a-_>")], /// TODO: Merge contiguous selection ranges
        "_" => vec![Todo("_")], /// TODO: Trim surrounding whitespace
        "x" => vec![ExpandToFullLines], /// Expand to full lines
        "X" => vec![Todo("X")], /// TODO: Extend to full lines
        "<a-x>" => vec![Todo("<a-x>")], /// TODO: Trim to full lines

        "<a-;>" => vec![Flip], /// Flip selection
        "<a-:>" => vec![FlipForward], /// Flip selection forward
        "(" => vec![RotateSelectionBackward], /// Rotate selection backward
        ")" => vec![RotateSelectionForward], /// Rotate selection forward
        "," => vec![KeepPrimary], /// Keep only primary selection range
        "<a-,>" => vec![Todo("<a-,>")], /// TODO: Remove primary selection range
        ";" => vec![Collapse], /// Collapse selection ranges

        "Z" => vec![Todo("Z")], /// TODO: Save selection to register
        "z" => vec![Todo("z")], /// TODO: Restore selection from register
        "<a-Z>" => vec![Todo("<a-Z>")], /// TODO: Combine selection with register
        "<a-z>" => vec![Todo("<a-z>")], /// TODO: Combine register with selection

        "<c-o>" => vec![Todo("<c-o>")], /// TODO: Jump backward
        "<c-i>" => vec![Todo("<c-i>")], /// TODO: Jump forward
        "<c-s>" => vec![Todo("<c-s>")], /// TODO: Save selection to jump list

        "o" => vec![InsertLineBelow], /// Enter insert mode on new line below
        "O" => vec![InsertLineAbove], /// Enter insert mode on new line above
        "<a-o>" => vec![AddLineBelow, Commit], /// Add line below
        "<a-O>" => vec![AddLineAbove, Commit], /// Add line above

        ":" => vec![EnterCommandMode], /// Enter command mode
        "\\ " => vec![Todo("\\ ")], /// TODO: Enter user mode

        "<c-u>" => vec![ScrollHalfPageUp], /// Scroll half page up
        "<c-d>" => vec![ScrollHalfPageDown], /// Scroll half page down
        "<c-b>" => vec![ScrollFullPageUp], /// Scroll page up
        "<c-f>" => vec![ScrollFullPageDown], /// Scroll page down

        "vc" => vec![ScrollCenter], /// Scroll to center cursor
        "V" => vec![Todo("V")], /// TODO: Enter locked view mode

        "<c-z>" => vec![Suspend], /// Suspend
        "<c-c>" => vec![QuitHint],
    }
}

pub fn handle_actions(client: &mut ClientMut, actions: &[Action]) {
    for action in actions {
        handle_action(client, action);
    }
}

pub fn handle_action(client: &mut ClientMut, action: &Action) {
    match action {
        Action::AppendCountDigit(digit) => {
            let digit = usize::from(*digit);
            let old_count = client.count.map_or(0, |count| usize::from(count));
            let new_count = NonZeroUsize::new(old_count.saturating_mul(10).saturating_add(digit));
            client.count = new_count;
        }
        Action::Commit => client.focused_buffer_mut().text.commit(),
        Action::EnterNormalMode => enter(client),
        Action::EnterCommandMode => enter_command_mode(client),
        Action::EnterInsertMode => insert::enter(client),
        Action::InsertAfterHead => insert_after_head(client),
        Action::InsertAtLineNonBlankStart => insert_at_line_non_blank_start(client),
        Action::InsertAtLineEnd => insert_at_line_end(client),
        Action::InsertLineAbove => insert_line_above(client),
        Action::InsertLineBelow => insert_line_below(client),
        Action::AddLineAbove => add_line_above(client),
        Action::AddLineBelow => add_line_below(client),
        Action::MoveLeft => move_left(client),
        Action::MoveRight => move_right(client),
        Action::MoveUp => move_up(client),
        Action::MoveDown => move_down(client),
        Action::ExtendLeft => extend_left(client),
        Action::ExtendRight => extend_right(client),
        Action::ExtendUp => extend_up(client),
        Action::ExtendDown => extend_down(client),
        Action::GotoMoveToLine => goto_move_to_line(client),
        Action::GotoMoveToStart => goto_move_to_start(client),
        Action::GotoExtendToStart => goto_extend_to_start(client),
        Action::GotoMoveToBottom => goto_move_to_bottom(client),
        Action::GotoExtendToBottom => goto_extend_to_bottom(client),
        Action::GotoMoveToEnd => goto_move_to_end(client),
        Action::GotoExtendToEnd => goto_extend_to_end(client),
        Action::GotoMoveToLineStart => goto_move_to_line_start(client),
        Action::GotoExtendToLineStart => goto_extend_to_line_start(client),
        Action::GotoMoveToLineNonBlankStart => goto_move_to_line_non_blank_start(client),
        Action::GotoExtendToLineNonBlankStart => goto_extend_to_line_non_blank_start(client),
        Action::GotoMoveUntilLineEnd => goto_move_until_line_end(client),
        Action::GotoExtendUntilLineEnd => goto_extend_until_line_end(client),
        Action::Seek {
            select,
            include,
            direction,
            byte,
        } => seek(client, *select, *include, *direction, *byte),
        Action::KeepPrimary => keep_primary(client),
        Action::RotateSelectionBackward => rotate_selection_backward(client),
        Action::RotateSelectionForward => rotate_selection_forward(client),
        Action::Collapse => collapse(client),
        Action::Flip => flip(client),
        Action::FlipForward => flip_forward(client),
        Action::SelectAll => select_all(client),
        Action::ExpandToFullLines => expand_to_full_lines(client),
        Action::SplitIntoLines => split_into_lines(client),
        Action::SelectRegex => select_regex(client),
        Action::Delete => delete(client),
        Action::Yank => yank(client),
        Action::PasteAfter => paste(client, PasteMode::After),
        Action::PasteBefore => paste(client, PasteMode::Before),
        Action::PasteReplace => paste(client, PasteMode::Replace),
        Action::PasteAllAfter => paste_all(client, PasteMode::After),
        Action::PasteAllBefore => paste_all(client, PasteMode::Before),
        Action::PasteAllReplace => paste_all(client, PasteMode::Replace),
        Action::Replace(byte) => replace(client, *byte),
        Action::Undo => undo(client),
        Action::Redo => redo(client),
        Action::ScrollHalfPageUp => client.focused_window_mut().scroll_half_page_up(),
        Action::ScrollHalfPageDown => client.focused_window_mut().scroll_half_page_down(),
        Action::ScrollFullPageUp => client.focused_window_mut().scroll_full_page_up(),
        Action::ScrollFullPageDown => client.focused_window_mut().scroll_full_page_down(),
        Action::ScrollCenter => client.focused_window_mut().scroll_center_selection(),
        Action::Suspend => client.suspend = true,
        Action::QuitHint => quit_hint(client),
        Action::Todo(keys) => todo(client, keys),
    }
}

pub fn handle_keys(client: &mut ClientMut) -> bool {
    let keymap = Arc::clone(&client.keymaps.normal);
    match keymap.get_keys(&client.pending_keys) {
        KeymapResult::Mapped(actions) => {
            client.pending_keys.clear();
            handle_actions(client, actions);
            true
        }
        KeymapResult::Computed(actions) => {
            client.pending_keys.clear();
            handle_actions(client, &actions);
            true
        }
        KeymapResult::Unmapped => {
            client.pending_keys.clear();
            client.count = None;
            false
        }
        KeymapResult::Pending => true,
    }
}

fn key_char(key: Key) -> Option<u8> {
    match key.code {
        KeyCode::Return if key.modifiers.is_empty() => Some(b'\n'),
        KeyCode::Tab if key.modifiers.is_empty() => Some(b'\t'),
        _ => key.typed_char(),
    }
}

fn seek(
    client: &mut ClientMut,
    select: SeekSelect,
    include: SeekInclude,
    direction: SeekDirection,
    byte: u8,
) {
    use SeekDirection::{Next, Prev};
    use SeekInclude::{Onto, Until};
    use SeekSelect::{Extend, Move};

    let count = client.count.take().unwrap_or(NonZeroUsize::MIN).get();

    let mut window = client.focused_window_mut();

    window
        .selection_move()
        .for_each_move(|mut range| match (select, include, direction) {
            (Move, Until, Prev) => range.move_until_prev_byte(byte, count),
            (Extend, Until, Prev) => range.extend_until_prev_byte(byte, count),
            (Move, Until, Next) => range.move_until_next_byte(byte, count),
            (Extend, Until, Next) => range.extend_until_next_byte(byte, count),
            (Move, Onto, Prev) => range.move_onto_prev_byte(byte, count),
            (Extend, Onto, Prev) => range.extend_onto_prev_byte(byte, count),
            (Move, Onto, Next) => range.move_onto_next_byte(byte, count),
            (Extend, Onto, Next) => range.extend_onto_next_byte(byte, count),
        });
    window.selection_move().merge_overlapping();

    window.scroll_to_selection();
}

pub fn enter(client: &mut ClientMut) {
    client.focused_buffer_mut().text.commit();
    client.mode = Mode::Normal;
    client.count = None;
}

fn extend_left(client: &mut ClientMut) {
    let count = client.count.take().unwrap_or(NonZeroUsize::MIN).get();
    let mut window = client.focused_window_mut();
    window
        .selection_move()
        .for_each_move(|mut range| range.extend_left(count));
    window.selection_move().merge_overlapping();
    window.scroll_to_selection();
}

fn move_left(client: &mut ClientMut) {
    let count = client.count.take().unwrap_or(NonZeroUsize::MIN).get();
    let mut window = client.focused_window_mut();
    window
        .selection_move()
        .for_each_move(|mut range| range.move_left(count));
    window.selection_move().merge_overlapping();
    window.scroll_to_selection();
}

fn extend_right(client: &mut ClientMut) {
    let count = client.count.take().unwrap_or(NonZeroUsize::MIN).get();
    let mut window = client.focused_window_mut();
    window
        .selection_move()
        .for_each_move(|mut range| range.extend_right(count));
    window.selection_move().merge_overlapping();
    window.scroll_to_selection();
}

fn move_right(client: &mut ClientMut) {
    let count = client.count.take().unwrap_or(NonZeroUsize::MIN).get();
    let mut window = client.focused_window_mut();
    window
        .selection_move()
        .for_each_move(|mut range| range.move_right(count));
    window.selection_move().merge_overlapping();
    window.scroll_to_selection();
}

fn extend_up(client: &mut ClientMut) {
    let count = client.count.take().unwrap_or(NonZeroUsize::MIN).get();
    let mut window = client.focused_window_mut();
    window
        .selection_move()
        .for_each_move(|mut range| range.extend_up(count));
    window.selection_move().merge_overlapping();
    window.scroll_to_selection();
}

fn move_up(client: &mut ClientMut) {
    let count = client.count.take().unwrap_or(NonZeroUsize::MIN).get();
    let mut window = client.focused_window_mut();
    window
        .selection_move()
        .for_each_move(|mut range| range.move_up(count));
    window.selection_move().merge_overlapping();
    window.scroll_to_selection();
}

fn extend_down(client: &mut ClientMut) {
    let count = client.count.take().unwrap_or(NonZeroUsize::MIN).get();
    let mut window = client.focused_window_mut();
    window
        .selection_move()
        .for_each_move(|mut range| range.extend_down(count));
    window.selection_move().merge_overlapping();
    window.scroll_to_selection();
}

fn move_down(client: &mut ClientMut) {
    let count = client.count.take().unwrap_or(NonZeroUsize::MIN).get();
    let mut window = client.focused_window_mut();
    window
        .selection_move()
        .for_each_move(|mut range| range.move_down(count));
    window.selection_move().merge_overlapping();
    window.scroll_to_selection();
}

fn quit_hint(client: &mut ClientMut) {
    client.set_message("Type :q<ret> to quit");
    client.count = None;
}

fn flip(client: &mut ClientMut) {
    let mut window = client.focused_window_mut();
    window
        .selection_move()
        .for_each_move(|mut range| range.flip());
    window.scroll_to_selection();
    client.count = None;
}

fn flip_forward(client: &mut ClientMut) {
    let mut window = client.focused_window_mut();
    window
        .selection_move()
        .for_each_move(|mut range| range.flip_forward());
    window.scroll_to_selection();
    client.count = None;
}

fn keep_primary(client: &mut ClientMut) {
    let mut window = client.focused_window_mut();
    window.selection_move().keep_primary();
    window.scroll_to_selection();
    client.count = None;
}

fn rotate_selection_backward(client: &mut ClientMut) {
    let count = client.count.take().unwrap_or(NonZeroUsize::MIN).get();
    let mut window = client.focused_window_mut();
    window.selection_move().rotate_backward(count);
    window.scroll_to_selection();
}

fn rotate_selection_forward(client: &mut ClientMut) {
    let count = client.count.take().unwrap_or(NonZeroUsize::MIN).get();
    let mut window = client.focused_window_mut();
    window.selection_move().rotate_forward(count);
    window.scroll_to_selection();
}

fn collapse(client: &mut ClientMut) {
    let mut window = client.focused_window_mut();
    window
        .selection_move()
        .for_each_move(|mut range| range.collapse());
    window.scroll_to_selection();
    client.count = None;
}

fn delete(client: &mut ClientMut) {
    if client.focused_buffer().text.readonly {
        client.set_error("Buffer is readonly");
        client.count = None;
        return;
    }
    let mut window = client.focused_window_mut();
    window.selection_edit().delete();
    window.scroll_to_selection();
    client.count = None;
}

fn yank(client: &mut ClientMut) {
    let values = client
        .focused_window()
        .selection()
        .contents()
        .map(String::from)
        .collect();
    client.session_mut().registers.insert('"', values);
    client.count = None;
}

fn paste(client: &mut ClientMut, paste_mode: PasteMode) {
    let count = client.count.take().unwrap_or(NonZeroUsize::MIN).get();
    if client.focused_buffer().text.readonly {
        client.set_error("Buffer is readonly");
        return;
    }
    let count = if paste_mode == PasteMode::Replace {
        1
    } else {
        count
    };
    let values = client
        .session()
        .registers
        .get(&'"')
        .cloned()
        .unwrap_or_default();
    let mut window = client.focused_window_mut();
    for _ in 1..=count {
        window.selection_edit().paste(&values, paste_mode);
    }
    window.scroll_to_selection();
}

fn paste_all(client: &mut ClientMut, paste_mode: PasteMode) {
    client.count = None;
    if client.focused_buffer().text.readonly {
        client.set_error("Buffer is readonly");
        return;
    }
    let values = client
        .session()
        .registers
        .get(&'"')
        .cloned()
        .unwrap_or_default();
    if values.is_empty() {
        client.set_error("Nothing to paste");
        return;
    }
    let mut window = client.focused_window_mut();
    window.selection_edit().paste_all(&values, paste_mode);
    window.scroll_to_selection();
}

fn replace(client: &mut ClientMut, byte: u8) {
    if client.focused_buffer().text.readonly {
        client.set_error("Buffer is readonly");
        client.count = None;
        return;
    }
    let mut window = client.focused_window_mut();
    window.selection_edit().replace_each(byte);
    window.scroll_to_selection();
    client.count = None;
}

fn undo(client: &mut ClientMut) {
    let count = client.count.take().unwrap_or(NonZeroUsize::MIN).get();
    if client.focused_buffer().text.readonly {
        client.set_error("Buffer is readonly");
        return;
    }
    let mut window = client.focused_window_mut();
    for _ in 1..=count {
        if !window.undo().unwrap() {
            break;
        }
    }
    window.scroll_to_selection();
}

fn redo(client: &mut ClientMut) {
    let count = client.count.take().unwrap_or(NonZeroUsize::MIN).get();
    if client.focused_buffer().text.readonly {
        client.set_error("Buffer is readonly");
        return;
    }
    let mut window = client.focused_window_mut();
    for _ in 1..=count {
        if !window.redo().unwrap() {
            break;
        }
    }
    window.scroll_to_selection();
}

fn select_all(client: &mut ClientMut) {
    let mut window = client.focused_window_mut();
    window.selection_move().select_all();
    window.scroll_to_selection();
    client.count = None;
}

fn expand_to_full_lines(client: &mut ClientMut) {
    let mut window = client.focused_window_mut();
    window
        .selection_move()
        .for_each_move(|mut range| range.expand_to_full_lines());
    window.selection_move().merge_overlapping();
    window.scroll_to_selection();
    client.count = None;
}

fn split_into_lines(client: &mut ClientMut) {
    let mut window = client.focused_window_mut();
    window.selection_move().split_into_lines();
    window.scroll_to_selection();
    client.count = None;
}

fn select_regex(client: &mut ClientMut) {
    prompt::enter(client, "select", |client, regex_str| {
        if regex_str.is_empty() {
            client.set_error("Nothing selected");
            client.count = None;
            return;
        }
        let regex = Regex::builder()
            .syntax(
                regex_cursor::regex_automata::util::syntax::Config::new()
                    .multi_line(true)
                    .dot_matches_new_line(true),
            )
            .build(regex_str);
        match regex {
            Ok(regex) => {
                let matched = client
                    .focused_window_mut()
                    .selection_move()
                    .select_regex(&regex);
                if !matched {
                    client.set_error("Nothing selected");
                }
            }
            Err(error) => {
                // Syntax errors notate the pattern over several lines, ending in "error: <reason>".
                let error = error
                    .syntax_error()
                    .map_or_else(|| error.to_string(), ToString::to_string);
                let last_line = error.lines().last().unwrap_or_default();
                let reason = last_line.strip_prefix("error: ").unwrap_or(last_line);
                client.set_error(format!("Invalid regex: {reason}"));
            }
        }
        client.count = None;
    });
}

fn insert_after_head(client: &mut ClientMut) {
    if client.focused_buffer().text.readonly {
        client.set_error("Buffer is readonly");
        client.count = None;
        return;
    }
    let mut window = client.focused_window_mut();
    window.selection_edit().for_each_edit(|mut range| {
        range.prepare_append();
    });
    window.scroll_to_selection();
    client.mode = Mode::Insert;
    client.count = None;
}

fn insert_at_line_non_blank_start(client: &mut ClientMut) {
    if client.focused_buffer().text.readonly {
        client.set_error("Buffer is readonly");
        client.count = None;
        return;
    }
    let mut window = client.focused_window_mut();
    window.selection_move().for_each_move(|mut range| {
        range.move_to_line_non_blank_start();
    });
    window.scroll_to_selection();
    client.mode = Mode::Insert;
    client.count = None;
}

fn insert_at_line_end(client: &mut ClientMut) {
    if client.focused_buffer().text.readonly {
        client.set_error("Buffer is readonly");
        client.count = None;
        return;
    }
    let mut window = client.focused_window_mut();
    window.selection_move().for_each_move(|mut range| {
        range.move_onto_line_end();
    });
    window.scroll_to_selection();
    client.mode = Mode::Insert;
    client.count = None;
}

fn insert_line_above(client: &mut ClientMut) {
    // TODO: Insert multiple cursors (like Kakoune) when count > 1
    let count = client.count.take().unwrap_or(NonZeroUsize::MIN).get();
    if client.focused_buffer().text.readonly {
        client.set_error("Buffer is readonly");
        return;
    }
    let mut window = client.focused_window_mut();
    // TODO: Switch to selection-level insert
    window.selection_edit().for_each_edit(|mut range| {
        range.move_to_line_start();
        for _ in 0..count {
            range.insert_char('\n');
        }
        range.move_left(1);
    });
    window.scroll_to_selection();
    client.mode = Mode::Insert;
}

fn insert_line_below(client: &mut ClientMut) {
    // TODO: Insert multiple cursors (like Kakoune) when count > 1
    let count = client.count.take().unwrap_or(NonZeroUsize::MIN).get();
    if client.focused_buffer().text.readonly {
        client.set_error("Buffer is readonly");
        return;
    }
    let mut window = client.focused_window_mut();
    // TODO: Switch to selection-level insert
    window.selection_edit().for_each_edit(|mut range| {
        range.move_onto_line_end();
        for _ in 0..count {
            range.insert_char('\n');
        }
    });
    window.scroll_to_selection();
    client.mode = Mode::Insert;
}

fn add_line_above(client: &mut ClientMut) {
    let count = client.count.take().unwrap_or(NonZeroUsize::MIN).get();
    if client.focused_buffer().text.readonly {
        client.set_error("Buffer is readonly");
        return;
    }
    let mut window = client.focused_window_mut();
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

fn add_line_below(client: &mut ClientMut) {
    let count = client.count.take().unwrap_or(NonZeroUsize::MIN).get();
    if client.focused_buffer().text.readonly {
        client.set_error("Buffer is readonly");
        return;
    }
    let mut window = client.focused_window_mut();
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

fn goto_move_to_line(client: &mut ClientMut) {
    let count = client.count.take().unwrap_or(NonZeroUsize::MIN).get();
    let mut window = client.focused_window_mut();
    let rope = window.buffer().text.rope().clone();
    let line_index = min(count - 1, rope.len_lines_unix() - 1);
    let byte_index = rope.line_to_byte_idx(line_index, LINE_TYPE);
    window
        .selection_move()
        .for_each_move(|mut range| range.move_to(byte_index));
    window.selection_move().merge_overlapping();
    window.scroll_to_selection();
}

fn goto_move_to_start(client: &mut ClientMut) {
    let mut window = client.focused_window_mut();
    window
        .selection_move()
        .for_each_move(|mut range| range.move_to_start());
    window.selection_move().merge_overlapping();
    window.scroll_to_selection();
    client.count = None;
}

fn goto_extend_to_start(client: &mut ClientMut) {
    let mut window = client.focused_window_mut();
    window
        .selection_move()
        .for_each_move(|mut range| range.extend_to_start());
    window.selection_move().merge_overlapping();
    window.scroll_to_selection();
    client.count = None;
}

fn goto_move_to_bottom(client: &mut ClientMut) {
    let mut window = client.focused_window_mut();
    window
        .selection_move()
        .for_each_move(|mut range| range.move_to_bottom());
    window.selection_move().merge_overlapping();
    window.scroll_to_selection();
    client.count = None;
}

fn goto_extend_to_bottom(client: &mut ClientMut) {
    let mut window = client.focused_window_mut();
    window
        .selection_move()
        .for_each_move(|mut range| range.extend_to_bottom());
    window.selection_move().merge_overlapping();
    window.scroll_to_selection();
    client.count = None;
}

fn goto_move_to_end(client: &mut ClientMut) {
    let mut window = client.focused_window_mut();
    window
        .selection_move()
        .for_each_move(|mut range| range.move_to_end());
    window.selection_move().merge_overlapping();
    window.scroll_to_selection();
    client.count = None;
}

fn goto_extend_to_end(client: &mut ClientMut) {
    let mut window = client.focused_window_mut();
    window
        .selection_move()
        .for_each_move(|mut range| range.extend_to_end());
    window.selection_move().merge_overlapping();
    window.scroll_to_selection();
    client.count = None;
}

fn goto_move_to_line_start(client: &mut ClientMut) {
    let mut window = client.focused_window_mut();
    window
        .selection_move()
        .for_each_move(|mut range| range.move_to_line_start());
    window.selection_move().merge_overlapping();
    client.count = None;
}

fn goto_extend_to_line_start(client: &mut ClientMut) {
    let mut window = client.focused_window_mut();
    window
        .selection_move()
        .for_each_move(|mut range| range.extend_to_line_start());
    window.selection_move().merge_overlapping();
    client.count = None;
}

fn goto_move_to_line_non_blank_start(client: &mut ClientMut) {
    let mut window = client.focused_window_mut();
    window
        .selection_move()
        .for_each_move(|mut range| range.move_to_line_non_blank_start());
    window.selection_move().merge_overlapping();
    client.count = None;
}

fn goto_extend_to_line_non_blank_start(client: &mut ClientMut) {
    let mut window = client.focused_window_mut();
    window
        .selection_move()
        .for_each_move(|mut range| range.extend_to_line_non_blank_start());
    window.selection_move().merge_overlapping();
    client.count = None;
}

fn goto_move_until_line_end(client: &mut ClientMut) {
    let mut window = client.focused_window_mut();
    window
        .selection_move()
        .for_each_move(|mut range| range.move_until_line_end());
    window.selection_move().merge_overlapping();
    client.count = None;
}

fn goto_extend_until_line_end(client: &mut ClientMut) {
    let mut window = client.focused_window_mut();
    window
        .selection_move()
        .for_each_move(|mut range| range.extend_until_line_end());
    window.selection_move().merge_overlapping();
    client.count = None;
}

fn todo(client: &mut ClientMut, keys: &str) {
    client.set_error(format!("{keys} is not implemented yet"));
    client.count = None;
}
