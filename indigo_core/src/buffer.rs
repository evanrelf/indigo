use crate::selection::Selection;
use ropey::Rope;
use std::{
    cmp::min,
    fs::File,
    io::{self, BufReader, BufWriter},
    path::{Path, PathBuf},
};

#[derive(Clone, Default)]
pub struct Buffer {
    pub path: Option<PathBuf>,
    contents: Rope,
    is_modified: bool,
    selection: Selection,
    vertical_scroll_offset: usize,
    horizontal_scroll_offset: usize,
}

impl Buffer {
    #[must_use]
    pub fn contents(&self) -> &Rope {
        &self.contents
    }

    #[must_use]
    pub fn is_modified(&self) -> bool {
        self.is_modified
    }

    #[must_use]
    pub fn selection(&self) -> &Selection {
        &self.selection
    }

    #[must_use]
    pub fn vertical_scroll_offset(&self) -> usize {
        self.vertical_scroll_offset
    }

    #[must_use]
    pub fn horizontal_scroll_offset(&self) -> usize {
        self.horizontal_scroll_offset
    }

    pub fn open(path: impl AsRef<Path>) -> Result<Self, io::Error> {
        let path_buf = path.as_ref().to_path_buf();
        let file = File::open(path)?;
        let reader = BufReader::new(file);

        Ok(Self {
            path: Some(path_buf),
            contents: Rope::from_reader(reader)?,
            ..Self::default()
        })
    }

    pub fn save(&mut self) -> Result<(), io::Error> {
        if let Some(path) = &self.path {
            let file = File::open(path)?;
            let writer = BufWriter::new(file);
            let result = self.contents.write_to(writer);
            if result.is_ok() {
                self.is_modified = false;
            }
            result
        } else {
            Err(io::Error::new(
                io::ErrorKind::Other,
                "no path associated with buffer",
            ))
        }
    }

    #[must_use]
    pub fn scroll_to_line(&self, line: usize) -> Self {
        let last_line = self.contents.len_lines().saturating_sub(2);

        Self {
            vertical_scroll_offset: min(line, last_line),
            ..self.clone()
        }
    }

    #[must_use]
    pub fn scroll_to_column(&self, column: usize) -> Self {
        // TODO: don't allow overscroll
        Self {
            horizontal_scroll_offset: column,
            ..self.clone()
        }
    }

    #[must_use]
    pub fn scroll_up(&self, distance: usize) -> Self {
        self.scroll_to_line(self.vertical_scroll_offset.saturating_sub(distance))
    }

    #[must_use]
    pub fn scroll_down(&self, distance: usize) -> Self {
        self.scroll_to_line(self.vertical_scroll_offset + distance)
    }

    #[must_use]
    pub fn scroll_left(&self, distance: usize) -> Self {
        self.scroll_to_column(self.horizontal_scroll_offset.saturating_sub(distance))
    }

    #[must_use]
    pub fn scroll_right(&self, distance: usize) -> Self {
        self.scroll_to_column(self.horizontal_scroll_offset + distance)
    }

    #[cfg(debug_assertions)]
    pub fn assert_invariants(&self) {
        use crate::validate::Validate as _;

        debug_assert!(
            {
                let mut valid = true;
                for range in self.selection().ranges() {
                    if !range.is_valid(Some(&self.contents)) {
                        valid = false;
                        break;
                    }
                }
                valid
            },
            "selection must be valid in the rope"
        )
        // TODO: add assertions for scroll offsets not going outside of buffer
    }
}
