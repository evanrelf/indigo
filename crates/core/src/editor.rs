use crate::cursor::CursorState;
use crate::{Cursor, CursorMut, RopeExt as _};
use camino::Utf8PathBuf;
use ropey::Rope;
use std::{cmp::min, fs::File, io::BufReader};

pub struct Editor {
    text: Rope,
    cursor: CursorState,
    vertical_scroll: usize,
}

impl Editor {
    pub fn new(path: Utf8PathBuf) -> anyhow::Result<Self> {
        let file = File::open(path)?;
        let rope = Rope::from_reader(BufReader::new(file))?;
        Ok(Self {
            text: rope,
            cursor: CursorState::default(),
            vertical_scroll: 0,
        })
    }

    #[must_use]
    pub fn text(&self) -> &Rope {
        &self.text
    }

    #[must_use]
    pub fn cursor(&self) -> Cursor {
        Cursor {
            rope: &self.text,
            state: self.cursor,
        }
    }

    #[must_use]
    pub fn cursor_at(&self, char_index: usize) -> Option<Cursor> {
        Cursor::from_char_index(&self.text, char_index)
    }

    #[must_use]
    pub fn cursor_mut(&mut self) -> CursorMut {
        CursorMut {
            rope: &mut self.text,
            state: self.cursor,
        }
    }

    #[must_use]
    pub fn cursor_mut_at(&mut self, char_index: usize) -> Option<CursorMut> {
        CursorMut::from_char_index(&mut self.text, char_index)
    }

    pub fn with_cursor<T>(&mut self, func: impl Fn(&mut CursorMut) -> T) -> T {
        let mut cursor = self.cursor_mut();
        let result = func(&mut cursor);
        self.cursor = cursor.state;
        result
    }

    #[must_use]
    pub fn vertical_scroll(&self) -> usize {
        self.vertical_scroll
    }

    pub fn scroll_to(&mut self, line: usize) {
        let last_line = self.text().len_lines_indigo().saturating_sub(1);
        self.vertical_scroll = min(line, last_line);
    }
}
