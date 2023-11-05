use crate::{rope::RopeExt as _, selection::Selection};
use anyhow::Context as _;
use ropey::Rope;
use std::{
    cmp::min,
    fs::File,
    io::BufReader,
    path::{Path, PathBuf},
};

// TODO: Update `vertical_scroll` (and maybe `horizontal_scroll`) whenever the buffer contents
// changes, to ensure you haven't scrolled beyond the end of the buffer.

#[derive(Clone, Debug, Default)]
pub struct Buffer {
    path: PathBuf,
    contents: Rope,
    selection: Selection,
    is_modified: bool,
    is_read_only: bool,
    vertical_scroll: usize,
    horizontal_scroll: usize,
}

impl Buffer {
    #[must_use]
    pub fn path(&self) -> &PathBuf {
        &self.path
    }

    #[must_use]
    pub fn contents(&self) -> &Rope {
        &self.contents
    }

    #[must_use]
    pub fn selection(&self) -> &Selection {
        &self.selection
    }

    #[must_use]
    pub fn is_modified(&self) -> bool {
        self.is_modified
    }

    #[must_use]
    pub fn is_read_only(&self) -> bool {
        self.is_read_only
    }

    #[must_use]
    pub fn vertical_scroll(&self) -> usize {
        self.vertical_scroll
    }

    #[must_use]
    pub fn horizontal_scroll(&self) -> usize {
        self.horizontal_scroll
    }

    pub fn open<P>(path: P) -> anyhow::Result<Self>
    where
        P: AsRef<Path> + Clone,
    {
        let file = File::open(path.clone()).context("Failed to open file")?;

        let is_read_only = file
            .metadata()
            .context("Failed to get file metadata")?
            .permissions()
            .readonly();

        let rope =
            Rope::from_reader(BufReader::new(file)).context("Failed to read file into rope")?;

        Ok(Self {
            path: path.as_ref().to_path_buf(),
            contents: rope,
            selection: Selection::default(),
            is_modified: false,
            is_read_only,
            vertical_scroll: 0,
            horizontal_scroll: 0,
        })
    }

    #[must_use]
    pub fn scroll_up(&self, distance: usize) -> Self {
        self.scroll_to_line(self.vertical_scroll.saturating_sub(distance))
    }

    pub fn scroll_up_mut(&mut self, distance: usize) {
        self.scroll_to_line_mut(self.vertical_scroll.saturating_sub(distance));
    }

    #[must_use]
    pub fn scroll_down(&self, distance: usize) -> Self {
        self.scroll_to_line(self.vertical_scroll + distance)
    }

    pub fn scroll_down_mut(&mut self, distance: usize) {
        self.scroll_to_line_mut(self.vertical_scroll + distance);
    }

    #[must_use]
    pub fn scroll_left(&self, distance: usize) -> Self {
        self.scroll_to_column(self.horizontal_scroll.saturating_sub(distance))
    }

    pub fn scroll_left_mut(&mut self, distance: usize) {
        self.scroll_to_column_mut(self.horizontal_scroll.saturating_sub(distance));
    }

    #[must_use]
    pub fn scroll_right(&self, distance: usize) -> Self {
        self.scroll_to_column(self.horizontal_scroll + distance)
    }

    pub fn scroll_right_mut(&mut self, distance: usize) {
        self.scroll_to_column_mut(self.horizontal_scroll + distance);
    }

    #[must_use]
    pub fn scroll_to_line(&self, line: usize) -> Self {
        let last_line = self.contents.len_lines_indigo().saturating_sub(1);
        Self {
            vertical_scroll: min(line, last_line),
            ..self.clone()
        }
    }

    pub fn scroll_to_line_mut(&mut self, line: usize) {
        let last_line = self.contents.len_lines_indigo().saturating_sub(1);
        self.vertical_scroll = min(line, last_line);
    }

    #[must_use]
    pub fn scroll_to_column(&self, column: usize) -> Self {
        Self {
            horizontal_scroll: column,
            ..self.clone()
        }
    }

    pub fn scroll_to_column_mut(&mut self, column: usize) {
        self.horizontal_scroll = column;
    }

    #[must_use]
    pub fn is_valid(&self) -> bool {
        todo!()
    }
}
