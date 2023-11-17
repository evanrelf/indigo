use crate::{File, FileKey, RopeExt};
use std::cmp::min;

slotmap::new_key_type! { pub struct WindowKey; }

#[derive(Debug)]
pub struct Window<'editor> {
    file: &'editor File,
    state: &'editor WindowState,
}

impl<'editor> Window<'editor> {
    pub(crate) fn new(file: &'editor File, state: &'editor WindowState) -> Self {
        Self { file, state }
    }

    #[must_use]
    pub fn file(&self) -> &File {
        self.file
    }

    #[must_use]
    pub fn vertical_scroll(&self) -> usize {
        self.state.vertical_scroll(self.file)
    }

    #[must_use]
    pub fn horizontal_scroll(&self) -> usize {
        self.state.horizontal_scroll(self.file)
    }

    fn assert_valid(&self) {
        self.state.assert_valid(self.file);
    }
}

#[derive(Debug)]
pub struct WindowMut<'editor> {
    file: &'editor mut File,
    state: &'editor mut WindowState,
}

impl<'editor> WindowMut<'editor> {
    pub(crate) fn new(file: &'editor mut File, state: &'editor mut WindowState) -> Self {
        Self { file, state }
    }

    #[must_use]
    pub fn file(&self) -> &File {
        self.file
    }

    #[must_use]
    pub fn file_mut(&mut self) -> &mut File {
        self.file
    }

    #[must_use]
    pub fn vertical_scroll(&self) -> usize {
        self.state.vertical_scroll(self.file)
    }

    #[must_use]
    pub fn horizontal_scroll(&self) -> usize {
        self.state.horizontal_scroll(self.file)
    }

    pub fn scroll_up(&mut self, distance: usize) {
        self.scroll_to_line(self.vertical_scroll().saturating_sub(distance));
    }

    pub fn scroll_down(&mut self, distance: usize) {
        self.scroll_to_line(self.vertical_scroll() + distance);
    }

    pub fn scroll_left(&mut self, distance: usize) {
        self.scroll_to_column(self.horizontal_scroll().saturating_sub(distance));
    }

    pub fn scroll_right(&mut self, distance: usize) {
        self.scroll_to_column(self.horizontal_scroll() + distance);
    }

    pub fn scroll_to_line(&mut self, line: usize) {
        self.state.scroll_to_line(line, self.file);
    }

    pub fn scroll_to_column(&mut self, column: usize) {
        self.state.scroll_to_column(column, self.file);
    }

    pub fn scroll_to_selection(&mut self, area_height: u16) {
        self.state.scroll_to_selection(area_height, self.file);
    }

    fn assert_valid(&self) {
        self.state.assert_valid(self.file);
    }
}

#[derive(Clone, Debug)]
pub(crate) struct WindowState {
    file_key: FileKey,
    prev_vertical_scroll: usize,
    prev_horizontal_scroll: usize,
}

impl WindowState {
    #[must_use]
    pub fn new(file_key: FileKey) -> Self {
        Self {
            file_key,
            prev_vertical_scroll: 0,
            prev_horizontal_scroll: 0,
        }
    }

    #[must_use]
    pub fn file_key(&self) -> FileKey {
        self.file_key
    }

    #[must_use]
    fn vertical_scroll(&self, file: &File) -> usize {
        let max_scroll = file
            .buffer()
            .contents()
            .len_lines_indigo()
            .saturating_sub(1);
        min(self.prev_vertical_scroll, max_scroll)
    }

    #[must_use]
    fn horizontal_scroll(&self, _file: &File) -> usize {
        self.prev_horizontal_scroll
    }

    fn scroll_to_line(&mut self, line: usize, file: &File) {
        let max_scroll = file
            .buffer()
            .contents()
            .len_lines_indigo()
            .saturating_sub(1);
        self.prev_vertical_scroll = min(line, max_scroll);
    }

    fn scroll_to_column(&mut self, column: usize, _file: &File) {
        self.prev_horizontal_scroll = column;
    }

    fn scroll_to_selection(&mut self, area_height: u16, file: &File) {
        let line = file.buffer().selection().primary().cursor().line;

        let top = self.vertical_scroll(file);
        let bottom = (top + usize::try_from(area_height).unwrap()) - 1;

        if line < top {
            self.prev_vertical_scroll = line;
        } else if line > bottom {
            self.prev_vertical_scroll = top + (line - bottom);
        };
    }

    fn assert_valid(&self, file: &File) {
        assert!(
            self.vertical_scroll(file) < file.buffer().contents().len_lines_indigo(),
            "`vertical_scroll` doesn't go beyond the last line of the file"
        );

        file.assert_valid();
    }
}
