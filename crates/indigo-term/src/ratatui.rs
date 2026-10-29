//! Use `indigo-term` as a `ratatui` backend

#![allow(clippy::uninlined_format_args)]
#![expect(unused_variables)]

use crate::{
    Terminal,
    output::{
        CLEAR_LINE, CLEAR_SCREEN, CLEAR_TO_END_OF_LINE, CLEAR_TO_END_OF_SCREEN,
        CLEAR_TO_START_OF_SCREEN, CURSOR_HIDE, CURSOR_SHOW, CursorPosition,
    },
};
use ratatui_core::{
    backend::{Backend, ClearType, WindowSize},
    buffer::Cell,
    layout::{Position, Size},
};
use std::io::{self, Write};

impl Backend for Terminal {
    type Error = io::Error;

    fn draw<'a, I>(&mut self, content: I) -> io::Result<()>
    where
        I: Iterator<Item = (u16, u16, &'a Cell)>,
    {
        todo!()
    }

    fn append_lines(&mut self, n: u16) -> io::Result<()> {
        for _ in 0..n {
            writeln!(self)?;
        }
        Write::flush(self)
    }

    fn hide_cursor(&mut self) -> io::Result<()> {
        write!(self, "{}", CURSOR_HIDE)?;
        Write::flush(self)
    }

    fn show_cursor(&mut self) -> io::Result<()> {
        write!(self, "{}", CURSOR_SHOW)?;
        Write::flush(self)
    }

    fn get_cursor_position(&mut self) -> io::Result<Position> {
        let (row, column) = self
            .query_cursor_position()?
            .ok_or_else(|| io::Error::other("terminal did not report cursor position"))?;
        Ok(Position {
            x: column.saturating_sub(1),
            y: row.saturating_sub(1),
        })
    }

    fn set_cursor_position<P: Into<Position>>(&mut self, position: P) -> io::Result<()> {
        let Position { x, y } = position.into();
        write!(self, "{}", CursorPosition(y + 1, x + 1))?;
        Write::flush(self)
    }

    fn clear(&mut self) -> io::Result<()> {
        self.clear_region(ClearType::All)
    }

    fn clear_region(&mut self, clear_type: ClearType) -> io::Result<()> {
        let sequence = match clear_type {
            ClearType::All => CLEAR_SCREEN,
            ClearType::AfterCursor => CLEAR_TO_END_OF_SCREEN,
            ClearType::BeforeCursor => CLEAR_TO_START_OF_SCREEN,
            ClearType::CurrentLine => CLEAR_LINE,
            ClearType::UntilNewLine => CLEAR_TO_END_OF_LINE,
        };
        write!(self, "{}", sequence)?;
        Write::flush(self)
    }

    fn size(&self) -> io::Result<Size> {
        let (height, width) = Self::size(self)?;
        Ok(Size { width, height })
    }

    fn window_size(&mut self) -> io::Result<WindowSize> {
        let (height, width) = Self::size(self)?;
        let (pixel_height, pixel_width) = self.pixel_size()?;
        Ok(WindowSize {
            columns_rows: Size { width, height },
            pixels: Size {
                width: pixel_width,
                height: pixel_height,
            },
        })
    }

    fn flush(&mut self) -> io::Result<()> {
        Write::flush(self)
    }
}
