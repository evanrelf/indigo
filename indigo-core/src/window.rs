use crate::FileKey;

slotmap::new_key_type! { pub struct WindowKey; }

#[derive(Clone, Copy, Debug)]
pub struct Window {
    file: FileKey,
    vertical_scroll: usize,
    horizontal_scroll: usize,
}

impl Window {
    #[must_use]
    pub fn new(file: FileKey) -> Self {
        Self {
            file,
            vertical_scroll: 0,
            horizontal_scroll: 0,
        }
    }

    #[must_use]
    pub fn file(&self) -> FileKey {
        self.file
    }

    #[must_use]
    pub fn vertical_scroll(&self) -> usize {
        self.vertical_scroll
    }

    #[must_use]
    pub fn horizontal_scroll(&self) -> usize {
        self.horizontal_scroll
    }

    pub fn scroll_up(&mut self, distance: usize) {
        self.scroll_to_line(self.vertical_scroll.saturating_sub(distance));
    }

    pub fn scroll_down(&mut self, distance: usize) {
        self.scroll_to_line(self.vertical_scroll + distance);
    }

    pub fn scroll_left(&mut self, distance: usize) {
        self.scroll_to_column(self.horizontal_scroll.saturating_sub(distance));
    }

    pub fn scroll_right(&mut self, distance: usize) {
        self.scroll_to_column(self.horizontal_scroll + distance);
    }

    pub fn scroll_to_line(&mut self, line: usize) {
        self.vertical_scroll = line;
    }

    pub fn scroll_to_column(&mut self, column: usize) {
        self.horizontal_scroll = column;
    }

    pub fn assert_valid(&self) {
        // TODO: `vertical_scroll` doesn't go beyond the last line of the buffer's rope
    }
}
