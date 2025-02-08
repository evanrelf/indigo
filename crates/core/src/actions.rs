#![expect(clippy::redundant_closure_for_method_calls)]

use crate::{
    editor::Editor,
    mode::{Mode, NormalMode},
    range::RangeExt as _,
};
use std::cmp::max;

pub fn enter_normal_mode(editor: &mut Editor) {
    editor.mode = Mode::Normal(NormalMode { count: 0 });
}

pub fn enter_insert_mode(editor: &mut Editor) {
    editor.with_range_mut(|range| range.reduce());
    editor.mode = Mode::Insert;
}

pub fn move_left(editor: &mut Editor) {
    let distance = max(1, editor.mode.count());
    editor.with_range_mut(|range| range.move_left(distance));
    editor.mode.set_count(0);
}

pub fn move_right(editor: &mut Editor) {
    let distance = max(1, editor.mode.count());
    editor.with_range_mut(|range| range.move_right(distance));
    editor.mode.set_count(0);
}

pub fn move_to(_editor: &mut Editor, _index: usize) {
    // TODO: Allow changing range indices
}

pub fn extend_left(editor: &mut Editor) {
    let distance = max(1, editor.mode.count());
    editor.with_range_mut(|range| range.extend_left(distance));
    editor.mode.set_count(0);
}

pub fn extend_right(editor: &mut Editor) {
    let distance = max(1, editor.mode.count());
    editor.with_range_mut(|range| range.extend_right(distance));
    editor.mode.set_count(0);
}

pub fn extend_to(_editor: &mut Editor, _index: usize) {
    // TODO: Allow changing range indices
}

pub fn flip(editor: &mut Editor) {
    editor.with_range_mut(|range| range.flip());
    editor.mode.set_count(0);
}

pub fn flip_forward(editor: &mut Editor) {
    editor.with_range_mut(|range| range.flip_forward());
    editor.mode.set_count(0);
}

pub fn reduce(editor: &mut Editor) {
    editor.with_range_mut(|range| range.reduce());
    editor.mode.set_count(0);
}

pub fn scroll_up(editor: &mut Editor) {
    let line = editor.vertical_scroll().saturating_sub(3);
    editor.scroll_to(line);
}

pub fn scroll_down(editor: &mut Editor) {
    let line = editor.vertical_scroll() + 3;
    editor.scroll_to(line);
}

pub fn scroll_half_page_up(editor: &mut Editor) {
    let line = editor.vertical_scroll().saturating_sub(editor.height / 2);
    editor.scroll_to(line);
}

pub fn scroll_half_page_down(editor: &mut Editor) {
    let line = editor.vertical_scroll() + editor.height / 2;
    editor.scroll_to(line);
}

pub fn scroll_full_page_up(editor: &mut Editor) {
    let line = editor.vertical_scroll().saturating_sub(editor.height);
    editor.scroll_to(line);
}

pub fn scroll_full_page_down(editor: &mut Editor) {
    let line = editor.vertical_scroll() + editor.height;
    editor.scroll_to(line);
}

pub fn insert_char(editor: &mut Editor, c: char) {
    editor.with_range_mut(|range| range.insert_char(c));
    editor.mode.set_count(0);
}

pub fn backspace(editor: &mut Editor) {
    editor.with_range_mut(|range| range.backspace());
    editor.mode.set_count(0);
}

pub fn delete(editor: &mut Editor) {
    editor.with_range_mut(|range| range.delete());
    editor.mode.set_count(0);
}
