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
    rope::{LINE_TYPE, RopeExt as _},
    window::{
        scroll_full_page_down, scroll_full_page_up, scroll_half_page_down, scroll_half_page_up,
    },
};
use regex_cursor::engines::meta::Regex;
use std::{cmp::min, num::NonZeroUsize};

#[derive(Clone, Default)]
pub struct State {
    pub count: Option<NonZeroUsize>,
}

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
                    enter_goto_mode(editor);
                }
            }
            _ if is(key, ",") => keep_primary(editor),
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

fn insert_line_above(editor: &mut Editor) {
    if editor.focused_buffer().text.readonly {
        editor.message = Some(Err(String::from("Buffer is readonly")));
        return;
    }
    // TODO: Insert multiple cursors (like Kakoune) when count > 1
    let count = editor.mode.count().unwrap_or(NonZeroUsize::MIN).get();
    let mut window = editor.focused_window_mut();
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
