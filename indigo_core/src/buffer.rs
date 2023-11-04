use crate::{rope::RopeExt as _, selection::Selection};
use anyhow::Context as _;
use ropey::Rope;
use std::{
    cmp::min,
    fs::File,
    io::BufReader,
    path::{Path, PathBuf},
};

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

    #[must_use]
    pub fn scroll_down(&self, distance: usize) -> Self {
        self.scroll_to_line(self.vertical_scroll + distance)
    }

    #[must_use]
    pub fn scroll_left(&self, distance: usize) -> Self {
        self.scroll_to_column(self.horizontal_scroll.saturating_sub(distance))
    }

    #[must_use]
    pub fn scroll_right(&self, distance: usize) -> Self {
        self.scroll_to_column(self.horizontal_scroll + distance)
    }

    #[must_use]
    pub fn scroll_to_line(&self, line: usize) -> Self {
        let last_line = self.contents.len_lines_indigo().saturating_sub(1);
        Self {
            vertical_scroll: min(line, last_line),
            ..self.clone()
        }
    }

    #[must_use]
    pub fn scroll_to_column(&self, column: usize) -> Self {
        Self {
            horizontal_scroll: column,
            ..self.clone()
        }
    }

    #[must_use]
    pub fn is_valid(&self) -> bool {
        todo!()
    }
}
