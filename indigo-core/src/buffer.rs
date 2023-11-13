use crate::{rope::RopeExt as _, selection::Selection};
use anyhow::Context as _;
use camino::Utf8PathBuf;
use ropey::Rope;
use std::{cmp::min, fs::File, io::BufReader, path::PathBuf};

#[derive(Clone, Debug)]
pub struct Buffer {
    path: Utf8PathBuf,
    contents: Rope,
    selection: Selection,
    is_modified: bool,
    is_read_only: bool,
    vertical_scroll: usize,
    horizontal_scroll: usize,
}

impl Buffer {
    #[must_use]
    pub fn path(&self) -> &Utf8PathBuf {
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

    pub fn open(path: Utf8PathBuf) -> anyhow::Result<Self> {
        let file = File::open(PathBuf::from(path.clone())).context("Failed to open file")?;

        let is_read_only = file
            .metadata()
            .context("Failed to get file metadata")?
            .permissions()
            .readonly();

        let rope =
            Rope::from_reader(BufReader::new(file)).context("Failed to read file into rope")?;

        let empty = rope.len_chars() == 0;

        Ok(Self {
            path,
            contents: if empty { Rope::from("\n") } else { rope },
            selection: Selection::default(),
            is_modified: empty,
            is_read_only,
            vertical_scroll: 0,
            horizontal_scroll: 0,
        })
    }

    pub fn scroll_up(&mut self, distance: usize) {
        self.scroll_to_line(self.vertical_scroll.saturating_sub(distance));
    }

    #[must_use]
    pub fn scroll_up_immut(&self, distance: usize) -> Self {
        self.scroll_to_line_immut(self.vertical_scroll.saturating_sub(distance))
    }

    pub fn scroll_down(&mut self, distance: usize) {
        self.scroll_to_line(self.vertical_scroll + distance);
    }

    #[must_use]
    pub fn scroll_down_immut(&self, distance: usize) -> Self {
        self.scroll_to_line_immut(self.vertical_scroll + distance)
    }

    pub fn scroll_left(&mut self, distance: usize) {
        self.scroll_to_column(self.horizontal_scroll.saturating_sub(distance));
    }

    #[must_use]
    pub fn scroll_left_immut(&self, distance: usize) -> Self {
        self.scroll_to_column_immut(self.horizontal_scroll.saturating_sub(distance))
    }

    pub fn scroll_right(&mut self, distance: usize) {
        self.scroll_to_column(self.horizontal_scroll + distance);
    }

    #[must_use]
    pub fn scroll_right_immut(&self, distance: usize) -> Self {
        self.scroll_to_column_immut(self.horizontal_scroll + distance)
    }

    pub fn scroll_to_line(&mut self, line: usize) {
        let last_line = self.contents.len_lines_indigo().saturating_sub(1);
        self.vertical_scroll = min(line, last_line);
    }

    #[must_use]
    pub fn scroll_to_line_immut(&self, line: usize) -> Self {
        let mut x = self.clone();
        x.scroll_to_line(line);
        x
    }

    pub fn scroll_to_column(&mut self, column: usize) {
        self.horizontal_scroll = column;
    }

    #[must_use]
    pub fn scroll_to_column_immut(&self, column: usize) -> Self {
        let mut x = self.clone();
        x.scroll_to_column(column);
        x
    }

    pub fn scroll_to_selection(&mut self, area_height: u16) {
        let line = self.selection.primary().cursor().line;

        let top = self.vertical_scroll;
        let bottom = (top + usize::try_from(area_height).unwrap()) - 1;

        if line < top {
            self.vertical_scroll = line;
        } else if line > bottom {
            self.vertical_scroll = top + (line - bottom);
        };
    }

    #[must_use]
    pub fn scroll_to_selection_immut(&self, area_height: u16) -> Self {
        let mut x = self.clone();
        x.scroll_to_selection(area_height);
        x
    }

    pub fn assert_valid(&self) {
        assert!(
            self.contents.len_chars() > 0,
            "`contents` rope is not empty"
        );

        assert!(
            self.vertical_scroll < self.contents.len_lines_indigo(),
            "`vertical_scroll` doesn't go beyond the last line of the `contents` rope"
        );

        // TODO: `selection` is valid in `contents` rope

        // TODO: Buffer validation

        self.selection.assert_valid();
    }
}

impl Default for Buffer {
    fn default() -> Self {
        Self {
            path: Utf8PathBuf::default(),
            contents: Rope::from("\n"),
            selection: Selection::default(),
            is_modified: false,
            is_read_only: false,
            vertical_scroll: 0,
            horizontal_scroll: 0,
        }
    }
}
