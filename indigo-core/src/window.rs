use crate::{File, FileKey, RopeExt};

slotmap::new_key_type! { pub struct WindowKey; }

#[derive(Debug)]
pub struct Window<'editor> {
    pub(crate) file: &'editor File,
    pub(crate) state: &'editor WindowState,
}

impl Window<'_> {
    pub fn file(&self) -> &File {
        &self.file
    }

    pub fn vertical_scroll(&self) -> usize {
        self.state.vertical_scroll(self.file)
    }

    pub fn horizontal_scroll(&self) -> usize {
        self.state.horizontal_scroll(self.file)
    }
}

#[derive(Debug)]
pub struct WindowMut<'editor> {
    pub(crate) file: &'editor mut File,
    pub(crate) state: &'editor mut WindowState,
}

impl WindowMut<'_> {
    pub fn file(&self) -> &File {
        &self.file
    }

    pub fn file_mut(&mut self) -> &mut File {
        &mut self.file
    }

    pub fn vertical_scroll(&self) -> usize {
        self.state.vertical_scroll(self.file)
    }

    pub fn horizontal_scroll(&self) -> usize {
        self.state.horizontal_scroll(self.file)
    }

    pub fn scroll_up(&mut self, distance: usize) {
        self.scroll_to_line(self.state.vertical_scroll.saturating_sub(distance));
    }

    pub fn scroll_down(&mut self, distance: usize) {
        self.scroll_to_line(self.state.vertical_scroll + distance);
    }

    pub fn scroll_left(&mut self, distance: usize) {
        self.scroll_to_column(self.state.horizontal_scroll.saturating_sub(distance));
    }

    pub fn scroll_right(&mut self, distance: usize) {
        self.scroll_to_column(self.state.horizontal_scroll + distance);
    }

    pub fn scroll_to_line(&mut self, line: usize) {
        todo!()
    }

    pub fn scroll_to_column(&mut self, column: usize) {
        self.state.horizontal_scroll = column;
    }

    pub fn scroll_to_selection(&mut self, area_height: u16) {
        todo!()
    }

    fn assert_valid(&self) {
        self.state.assert_valid(self.file);
    }
}

#[derive(Clone, Copy, Debug)]
pub(crate) struct WindowState {
    pub(crate) file_key: FileKey,
    pub(crate) vertical_scroll: usize,
    pub(crate) horizontal_scroll: usize,
}

impl WindowState {
    pub(crate) fn new(file_key: FileKey) -> Self {
        Self {
            file_key,
            vertical_scroll: 0,
            horizontal_scroll: 0,
        }
    }

    fn vertical_scroll(&self, file: &File) -> usize {
        todo!()
    }

    fn horizontal_scroll(&self, file: &File) -> usize {
        todo!()
    }

    fn assert_valid(&self, file: &File) {
        assert!(
            self.vertical_scroll(file) < file.buffer().contents().len_lines_indigo(),
            "`vertical_scroll` doesn't go beyond the last line of the file"
        );

        file.assert_valid();
    }
}
