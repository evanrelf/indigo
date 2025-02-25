use crate::{
    editor::Editor,
    mode::{CommandMode, Mode, NormalMode},
};
use std::num::NonZeroUsize;

pub fn enter_normal_mode(editor: &mut Editor) {
    editor.mode = Mode::Normal(NormalMode::default());
}

pub fn enter_insert_mode(editor: &mut Editor) {
    editor.with_range_mut(|range| range.reduce());
    editor.mode = Mode::Insert;
}

pub fn enter_command_mode(editor: &mut Editor) {
    editor.mode = Mode::Command(CommandMode::default());
}

pub fn move_left(editor: &mut Editor) {
    let count = editor.mode.count();
    editor.with_range_mut(|range| {
        range.extend_left(count);
        range.reduce();
    });
    editor.mode.set_count(NonZeroUsize::MIN);
}

pub fn move_right(editor: &mut Editor) {
    let count = editor.mode.count();
    editor.with_range_mut(|range| {
        range.extend_right(count);
        range.reduce();
    });
    editor.mode.set_count(NonZeroUsize::MIN);
}

pub fn move_to(_editor: &mut Editor, _index: usize) {
    // TODO: Allow changing range indices
}

pub fn extend_left(editor: &mut Editor) {
    let count = editor.mode.count();
    editor.with_range_mut(|range| range.extend_left(count));
    editor.mode.set_count(NonZeroUsize::MIN);
}

pub fn extend_right(editor: &mut Editor) {
    let count = editor.mode.count();
    editor.with_range_mut(|range| range.extend_right(count));
    editor.mode.set_count(NonZeroUsize::MIN);
}

pub fn extend_to(_editor: &mut Editor, _index: usize) {
    // TODO: Allow changing range indices
}

pub fn flip(editor: &mut Editor) {
    editor.with_range_mut(|range| range.flip());
    editor.mode.set_count(NonZeroUsize::MIN);
}

pub fn flip_forward(editor: &mut Editor) {
    editor.with_range_mut(|range| range.flip_forward());
    editor.mode.set_count(NonZeroUsize::MIN);
}

pub fn reduce(editor: &mut Editor) {
    editor.with_range_mut(|range| range.reduce());
    editor.mode.set_count(NonZeroUsize::MIN);
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

pub fn insert_char(editor: &mut Editor, char: char) {
    editor.with_range_mut(|range| range.insert_char(char));
    editor.mode.set_count(NonZeroUsize::MIN);
}

pub fn insert(editor: &mut Editor, string: &str) {
    editor.with_range_mut(|range| range.insert(string));
    editor.mode.set_count(NonZeroUsize::MIN);
}

pub fn delete_before(editor: &mut Editor) {
    let count = editor.mode.count();
    editor.with_range_mut(|range| range.delete_before(count));
    editor.mode.set_count(NonZeroUsize::MIN);
}

pub fn delete(editor: &mut Editor) {
    editor.with_range_mut(|range| range.delete());
    editor.mode.set_count(NonZeroUsize::MIN);
}

pub fn delete_after(editor: &mut Editor) {
    let count = editor.mode.count();
    editor.with_range_mut(|range| range.delete_after(count));
    editor.mode.set_count(NonZeroUsize::MIN);
}
