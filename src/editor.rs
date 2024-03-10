use crate::{buffer::Buffer, mode::Mode, position::Position, rope::RopeExt as _};
use camino::Utf8PathBuf;
use std::{
    cmp::min,
    fs::File,
    io::{BufWriter, Write as _},
};

#[derive(Debug)]
pub struct Editor {
    pub path: Option<Utf8PathBuf>,
    pub buffer: Buffer,
    pub scroll: Position,
    pub mode: Mode,
}

impl Editor {
    pub fn save(&self) -> anyhow::Result<()> {
        if let Some(path) = &self.path {
            let file = File::create(path)?;
            let mut writer = BufWriter::with_capacity(self.buffer.text.len_bytes(), file);
            self.buffer.text.write_to(&mut writer)?;
            writer.flush()?;
        } else {
            anyhow::bail!("No path");
        }

        Ok(())
    }

    pub fn scroll_to(&mut self, line: usize, column: usize) {
        let last_line = self.buffer.text.len_lines_indigo().saturating_sub(1);
        self.scroll.line = min(line, last_line);
        self.scroll.column = column;
    }

    pub fn scroll_up(&mut self, distance: usize) {
        self.scroll_to(
            self.scroll.line.saturating_sub(distance),
            self.scroll.column,
        );
    }

    pub fn scroll_down(&mut self, distance: usize) {
        self.scroll_to(self.scroll.line + distance, self.scroll.column);
    }

    pub fn scroll_left(&mut self, distance: usize) {
        self.scroll_to(
            self.scroll.line,
            self.scroll.column.saturating_sub(distance),
        );
    }

    pub fn scroll_right(&mut self, distance: usize) {
        self.scroll_to(self.scroll.line, self.scroll.column + distance);
    }
}

impl Default for Editor {
    fn default() -> Self {
        Self {
            path: None,
            buffer: Buffer::default(),
            scroll: Position { line: 0, column: 0 },
            mode: Mode::Normal,
        }
    }
}
