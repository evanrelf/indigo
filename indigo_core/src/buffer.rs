use crate::selection::{self, Selection};
use anyhow::anyhow;
use camino::Utf8PathBuf;
use ropey::Rope;
use std::{
    cmp::{max, min},
    fs::File,
    io::{BufReader, BufWriter},
    path::Path,
};

#[derive(Clone, Default)]
pub struct Buffer {
    pub path: Option<Utf8PathBuf>,
    pub contents: Rope,
    is_modified: bool,
    selection: Selection,
    vertical_scroll_offset: usize,
    horizontal_scroll_offset: usize,
}

impl Buffer {
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

    #[must_use]
    pub fn update_selection<'rope>(
        &'rope self,
        selection_fn: impl Fn(&'rope Rope, &Selection) -> Selection,
    ) -> Self {
        let mut new = self.clone();
        new.selection = selection_fn(&self.contents, &self.selection);
        new
    }

    pub fn open(path: impl AsRef<Path>) -> Result<Self, anyhow::Error> {
        let path_buf = Utf8PathBuf::try_from(path.as_ref().to_path_buf())?;
        let file = File::open(path)?;
        let reader = BufReader::new(file);

        Ok(Self {
            path: Some(path_buf),
            contents: Rope::from_reader(reader)?,
            ..Self::default()
        })
    }

    pub fn save(&mut self) -> Result<(), anyhow::Error> {
        if let Some(path) = &self.path {
            let file = File::open(path)?;
            let writer = BufWriter::new(file);
            self.contents.write_to(writer)?;
            self.is_modified = false;
            Ok(())
        } else {
            Err(anyhow!("no path associated with buffer"))
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
        let mut longest_line = 0;

        for line in self.contents.lines_at(self.vertical_scroll_offset) {
            longest_line = max(line.len_chars(), longest_line);
        }

        Self {
            horizontal_scroll_offset: min(column, longest_line),
            ..self.clone()
        }
    }

    #[must_use]
    pub fn scroll_to_selection(&self, height: u16) -> Self {
        let line = self.selection.primary_range().1.head().line;

        let top = self.vertical_scroll_offset;
        let bottom = top + height as usize - 1;

        let vertical_scroll_offset = if line < top {
            line
        } else if line > bottom {
            top + (line - bottom)
        } else {
            self.vertical_scroll_offset
        };

        Self {
            vertical_scroll_offset,
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

    pub fn assert_invariants(&self) {
        assert!(
            selection::selection_is_valid(&self.selection, Some(&self.contents)),
            "selection must be valid in the rope"
        );

        assert!(
            {
                let mut longest_line = 0;
                for line in self.contents.lines() {
                    longest_line = max(line.len_chars(), longest_line);
                }
                self.horizontal_scroll_offset <= longest_line
            },
            "horizontal scroll offset must not exceed length of longest line"
        );

        assert!(
            self.vertical_scroll_offset <= self.contents.len_lines().saturating_sub(2),
            "vertical scroll offset must not exceed length of buffer"
        );
    }
}
