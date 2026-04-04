use crate::{
    editor::Editor,
    event::{Event, KeyEvent},
    key::{KeyCode, is},
    mode::{
        Mode,
        command::enter_command_mode,
        goto::enter_goto_mode,
        insert::{self, enter_insert_mode},
        prompt::enter_prompt_mode,
        seek::enter_seek_mode,
    },
    range::RangeMut,
    rope::{LINE_TYPE, RopeExt as _},
    window::{
        scroll_full_page_down, scroll_full_page_up, scroll_half_page_down, scroll_half_page_up,
    },
};
use ropey::Rope;
use regex_cursor::engines::meta::Regex;
use std::{cmp::min, num::NonZeroUsize};

#[derive(Clone, Default)]
pub struct State {
    pub count: Option<NonZeroUsize>,
    pub pending_replace: bool,
}

pub fn handle_event_normal(editor: &mut Editor, event: &Event) -> bool {
    use crate::mode::seek::{
        SeekDirection::{Next, Prev},
        SeekInclude::{Onto, Until},
        SeekSelect::{Extend, Move},
    };

    let mut handled = true;

    // Handle pending replace: next char replaces each char in selection
    if let Mode::Normal(state) = &editor.mode
        && state.pending_replace
    {
        let Event::Key(KeyEvent { key, .. }) = event;
        if is(key, "<esc>") {
            // Cancel replace
            if let Mode::Normal(state) = &mut editor.mode {
                state.pending_replace = false;
            }
        } else if let KeyCode::Char(c) = key.code
            && key.modifiers.is_empty()
        {
            replace_with_char(editor, char::from(c));
        } else if is(key, "<ret>") {
            replace_with_char(editor, '\n');
        }
        return true;
    }

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
                    enter_goto_mode(editor);
                }
            }
            _ if is(key, ",") => keep_primary(editor),
            _ if is(key, ";") => reduce(editor),
            _ if is(key, "<a-;>") => flip(editor),
            _ if is(key, "<a-:>") => flip_forward(editor),
            _ if is(key, "%") => select_all(editor),
            _ if is(key, "s") => select_regex(editor),
            _ if is(key, "x") => select_lines(editor),
            _ if is(key, "X") => extend_lines(editor),
            _ if is(key, "`") => to_lowercase(editor),
            _ if is(key, "~") => to_uppercase(editor),
            _ if is(key, "<a-`>") => swap_case(editor),
            _ if is(key, "r") => enter_replace(editor),
            _ if is(key, "d") => delete(editor),
            _ if is(key, "c") => {
                delete(editor);
                enter_insert_mode(editor);
            }
            _ if is(key, "o") => open_line_below(editor),
            _ if is(key, "O") => open_line_above(editor),
            _ if is(key, "<a-o>") => add_empty_line_below(editor),
            _ if is(key, "<a-O>") => add_empty_line_above(editor),
            _ if is(key, "u") => undo(editor),
            _ if is(key, "U") => redo(editor),
            _ if is(key, "<c-u>") => scroll_half_page_up(editor),
            _ if is(key, "<c-d>") => scroll_half_page_down(editor),
            _ if is(key, "<c-b>") => scroll_full_page_up(editor),
            _ if is(key, "<c-f>") => scroll_full_page_down(editor),
            _ => handled = false,
        },
    }

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

fn reduce(editor: &mut Editor) {
    let mut window = editor.focused_window_mut();
    window
        .selection_mut()
        .for_each_mut(|mut range| range.reduce());
    window.scroll_to_selection();
    editor.mode.set_count(None);
}

fn enter_replace(editor: &mut Editor) {
    if editor.focused_buffer().text.readonly {
        editor.message = Some(Err(String::from("Buffer is readonly")));
        editor.mode.set_count(None);
        return;
    }
    if let Mode::Normal(state) = &mut editor.mode {
        state.pending_replace = true;
    }
}

fn replace_with_char(editor: &mut Editor, replacement: char) {
    let mut window = editor.focused_window_mut();
    window.selection_mut().for_each_mut(|mut range| {
        let len = range.grapheme_length();
        let replacement_text: String = std::iter::repeat_n(replacement, len).collect();
        range.delete();
        range.insert(&replacement_text);
    });
    window.scroll_to_selection();
    if let Mode::Normal(state) = &mut editor.mode {
        state.pending_replace = false;
        state.count = None;
    }
}

fn to_lowercase(editor: &mut Editor) {
    if editor.focused_buffer().text.readonly {
        editor.message = Some(Err(String::from("Buffer is readonly")));
        editor.mode.set_count(None);
        return;
    }
    let mut window = editor.focused_window_mut();
    window.selection_mut().for_each_mut(|mut range| {
        let text = range.slice().to_string().to_lowercase();
        range.delete();
        range.insert(&text);
    });
    window.scroll_to_selection();
    editor.mode.set_count(None);
}

fn to_uppercase(editor: &mut Editor) {
    if editor.focused_buffer().text.readonly {
        editor.message = Some(Err(String::from("Buffer is readonly")));
        editor.mode.set_count(None);
        return;
    }
    let mut window = editor.focused_window_mut();
    window.selection_mut().for_each_mut(|mut range| {
        let text = range.slice().to_string().to_uppercase();
        range.delete();
        range.insert(&text);
    });
    window.scroll_to_selection();
    editor.mode.set_count(None);
}

fn swap_case(editor: &mut Editor) {
    if editor.focused_buffer().text.readonly {
        editor.message = Some(Err(String::from("Buffer is readonly")));
        editor.mode.set_count(None);
        return;
    }
    let mut window = editor.focused_window_mut();
    window.selection_mut().for_each_mut(|mut range| {
        let text: String = range
            .slice()
            .chars()
            .flat_map(|c| {
                if c.is_uppercase() {
                    c.to_lowercase().collect::<Vec<_>>()
                } else {
                    c.to_uppercase().collect::<Vec<_>>()
                }
            })
            .collect();
        range.delete();
        range.insert(&text);
    });
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
    window.selection_mut().for_each_mut(|mut range| {
        range.delete();
    });
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

/// `x`: Snap to line boundaries, then drag cursor (head) down.
/// First press on a non-aligned selection just aligns. If already aligned, extends down.
/// Follows byline.kak behavior.
fn select_lines(editor: &mut Editor) {
    let count = editor.mode.count().unwrap_or(NonZeroUsize::MIN).get();
    let mut window = editor.focused_window_mut();
    window.selection_mut().for_each_mut(|mut range| {
        let rope = range.text().rope().clone();
        let len_lines = rope.len_lines_indigo();
        if len_lines == 0 {
            return;
        }

        // Determine current tail/head lines
        let (tail_line, head_line, _forward) = line_range_info(&range, &rope);

        // Check if already line-aligned
        let tail_byte = range.tail().byte_offset();
        let head_byte = range.head().byte_offset();
        let aligned = is_line_aligned(tail_byte, head_byte, tail_line, head_line, &rope);

        // Drag head down by count (or just snap if not aligned)
        let steps = if aligned { count } else { count.saturating_sub(1) };
        let new_head_line = min(head_line + steps, len_lines.saturating_sub(1));

        set_line_range(&mut range, tail_line, new_head_line, &rope);
    });
    window.scroll_to_selection();
    editor.mode.set_count(None);
}

/// `X`: Snap to line boundaries, then drag cursor (head) up.
/// Follows byline.kak behavior.
fn extend_lines(editor: &mut Editor) {
    let count = editor.mode.count().unwrap_or(NonZeroUsize::MIN).get();
    let mut window = editor.focused_window_mut();
    window.selection_mut().for_each_mut(|mut range| {
        let rope = range.text().rope().clone();
        let len_lines = rope.len_lines_indigo();
        if len_lines == 0 {
            return;
        }

        let (tail_line, head_line, _forward) = line_range_info(&range, &rope);
        let tail_byte = range.tail().byte_offset();
        let head_byte = range.head().byte_offset();
        let aligned = is_line_aligned(tail_byte, head_byte, tail_line, head_line, &rope);

        let steps = if aligned { count } else { count.saturating_sub(1) };
        let new_head_line = head_line.saturating_sub(steps);

        set_line_range(&mut range, tail_line, new_head_line, &rope);
    });
    window.scroll_to_selection();
    editor.mode.set_count(None);
}

/// Determine the tail line, head line, and direction of a range.
fn line_range_info(range: &RangeMut<'_>, rope: &Rope) -> (usize, usize, bool) {
    let forward = range.is_forward();
    let tail_byte = range.tail().byte_offset();
    let head_byte = range.head().byte_offset();

    let tail_line = rope.byte_to_line_idx(tail_byte, LINE_TYPE);
    let head_line = if head_byte > 0 && (forward && head_byte > tail_byte || !forward && head_byte < tail_byte) {
        // For the "far end" of the range, use byte-1 to stay on the correct line
        // (since the end offset points past the last char)
        rope.byte_to_line_idx(head_byte.saturating_sub(1), LINE_TYPE)
    } else {
        rope.byte_to_line_idx(head_byte, LINE_TYPE)
    };

    (tail_line, head_line, forward)
}

/// Check if a range is already snapped to line boundaries.
fn is_line_aligned(tail_byte: usize, head_byte: usize, tail_line: usize, head_line: usize, rope: &Rope) -> bool {
    let (start_line, end_line) = if tail_line <= head_line {
        (tail_line, head_line)
    } else {
        (head_line, tail_line)
    };
    let start_byte = min(tail_byte, head_byte);
    let end_byte = tail_byte.max(head_byte);

    let expected_start = rope.line_to_byte_idx(start_line, LINE_TYPE);
    let expected_end = line_end_byte(end_line, rope);

    start_byte == expected_start && end_byte == expected_end
}

/// Get the byte offset for the end of a line (start of next line, or rope length).
fn line_end_byte(line: usize, rope: &Rope) -> usize {
    let len_lines = rope.len_lines_indigo();
    if line + 1 < len_lines {
        rope.line_to_byte_idx(line + 1, LINE_TYPE)
    } else {
        rope.len()
    }
}

/// Set a range to cover from `tail_line` to `head_line` (both line-aligned).
/// Handles forward, backward, and same-line cases.
fn set_line_range(range: &mut RangeMut<'_>, tail_line: usize, head_line: usize, rope: &Rope) {
    if head_line >= tail_line {
        // Forward: tail at line start, head at line end
        let tail_byte = rope.line_to_byte_idx(tail_line, LINE_TYPE);
        let head_end = line_end_byte(head_line, rope);
        range.move_to(tail_byte);
        if head_end > tail_byte + 1 {
            range.extend_to(head_end.saturating_sub(1));
        }
    } else {
        // Backward: tail at end of tail_line, head at start of head_line
        let tail_end = line_end_byte(tail_line, rope);
        let head_start = rope.line_to_byte_idx(head_line, LINE_TYPE);
        // Position at tail end first, then extend backward to head start
        range.move_to(tail_end.saturating_sub(1));
        range.extend_to(head_start);
    }
}

fn open_line_below(editor: &mut Editor) {
    if editor.focused_buffer().text.readonly {
        editor.message = Some(Err(String::from("Buffer is readonly")));
        return;
    }
    let mut window = editor.focused_window_mut();
    window.selection_mut().for_each_mut(|mut range| {
        range.move_onto_line_end();
        range.insert_char('\n');
    });
    window.scroll_to_selection();
    editor.mode = Mode::Insert(insert::State::default());
}

fn add_empty_line_below(editor: &mut Editor) {
    if editor.focused_buffer().text.readonly {
        editor.message = Some(Err(String::from("Buffer is readonly")));
        return;
    }
    let mut window = editor.focused_window_mut();
    window.selection_mut().for_each_mut(|mut range| {
        let saved = range.head().byte_offset();
        range.move_onto_line_end();
        let ops = range.insert_char('\n');
        range.move_to(ops.transform_byte_offset(saved));
    });
    window.scroll_to_selection();
    editor.mode.set_count(None);
}

fn add_empty_line_above(editor: &mut Editor) {
    if editor.focused_buffer().text.readonly {
        editor.message = Some(Err(String::from("Buffer is readonly")));
        return;
    }
    let mut window = editor.focused_window_mut();
    window.selection_mut().for_each_mut(|mut range| {
        let saved = range.head().byte_offset();
        range.move_to_line_start();
        let ops = range.insert_char('\n');
        range.move_to(ops.transform_byte_offset(saved));
    });
    window.scroll_to_selection();
    editor.mode.set_count(None);
}

fn open_line_above(editor: &mut Editor) {
    if editor.focused_buffer().text.readonly {
        editor.message = Some(Err(String::from("Buffer is readonly")));
        return;
    }
    let mut window = editor.focused_window_mut();
    window.selection_mut().for_each_mut(|mut range| {
        range.move_to_line_start();
        range.insert_char('\n');
        let offset = range.start().byte_offset() - 1;
        range.move_to(offset);
    });
    window.scroll_to_selection();
    editor.mode = Mode::Insert(insert::State::default());
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
