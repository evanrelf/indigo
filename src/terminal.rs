use crate::{
    buffer::Buffer,
    command::CommandLine,
    editor::{self, Editor, Mode},
};
use crossterm::ExecutableCommand;
use std::borrow::Cow;
use std::{
    io::{Stdout, Write},
    panic,
};
use tui::widgets::Widget;

pub fn run(editor: Editor) {
    with_terminal(|terminal| run_with_terminal(editor, terminal));
}

fn run_with_terminal<B>(mut editor: Editor, terminal: &mut tui::Terminal<B>)
where
    B: tui::backend::Backend,
{
    while !editor.quit() {
        terminal
            .draw(|frame| frame.render_widget(&editor, frame.size()))
            .unwrap();

        let terminal_event = crossterm::event::read().unwrap();

        let editor_event = match terminal_event {
            crossterm::event::Event::Key(key_event) => Some(editor::Event::Key(key_event)),
            crossterm::event::Event::Mouse(mouse_event) => Some(editor::Event::Mouse(mouse_event)),
            crossterm::event::Event::Resize(_, _) => None,
        };

        if let Some(editor_event) = editor_event {
            editor.handle_event(editor_event);
        }
    }
}

fn with_terminal<F>(f: F)
where
    F: FnOnce(&mut tui::Terminal<tui::backend::CrosstermBackend<Stdout>>),
{
    let mut terminal = {
        let stdout = std::io::stdout();
        let backend = tui::backend::CrosstermBackend::new(stdout);
        tui::Terminal::new(backend).unwrap()
    };

    enter_terminal();

    f(&mut terminal);

    exit_terminal(false);
}

fn enter_terminal() {
    let mut stdout = std::io::stdout();
    crossterm::terminal::enable_raw_mode().unwrap();
    stdout
        .execute(crossterm::terminal::EnterAlternateScreen)
        .unwrap();
    stdout.execute(crossterm::cursor::Hide).unwrap();
    stdout
        .execute(crossterm::event::EnableMouseCapture)
        .unwrap();
    panic::set_hook(Box::new(|panic_info| {
        exit_terminal(true);
        eprintln!("{}", panic_info);
    }));
}

fn exit_terminal(panicking: bool) {
    let mut stdout = std::io::stdout();
    stdout
        .execute(crossterm::event::DisableMouseCapture)
        .unwrap();
    stdout.execute(crossterm::cursor::Show).unwrap();
    stdout
        .execute(crossterm::terminal::LeaveAlternateScreen)
        .unwrap();
    stdout.flush().unwrap();
    crossterm::terminal::disable_raw_mode().unwrap();
    if !panicking {
        let _ = panic::take_hook();
    }
}

impl Widget for &Editor {
    fn render(self, area: tui::layout::Rect, buffer: &mut tui::buffer::Buffer) {
        use tui::{
            layout::{Constraint, Direction, Layout},
            style::{Color, Style},
            widgets::Block,
        };

        let chunks = Layout::default()
            .direction(Direction::Vertical)
            .constraints(
                [
                    Constraint::Min(0),
                    Constraint::Length(1),
                    Constraint::Length(1),
                ]
                .as_ref(),
            )
            .split(area);

        // Render tildes
        for y in chunks[0].top()..chunks[0].bottom() {
            buffer.get_mut(area.x, y).set_char('~');
        }

        // Render status
        let mode = match self.mode() {
            Mode::Normal => "normal",
            Mode::Command => "command",
            Mode::Insert => "insert",
        };

        let cursor = {
            let selection = self.buffers()[self.buffer_index()].selection();
            let range = &selection.ranges[selection.primary_range_index];
            range.head.to_string()
        };

        let count = if self.count() > 0 {
            format!(" {}", self.count())
        } else {
            "".to_string()
        };

        Block::default()
            .title(format!("{} {}{}", mode, cursor, count))
            .style(Style::default().bg(Color::Rgb(0xEE, 0xEE, 0xEE)))
            .render(chunks[1], buffer);

        // Render command line
        if let Mode::Command = self.mode() {
            self.command_line().render(chunks[2], buffer);
        }

        // Render buffer
        self.buffers()[self.buffer_index()].render(chunks[0], buffer);
    }
}

impl Widget for &Buffer {
    fn render(self, area: tui::layout::Rect, buffer: &mut tui::buffer::Buffer) {
        use tui::buffer::Buffer;
        use tui::{
            layout::{Constraint, Direction, Layout, Rect},
            style::{Color, Style},
        };

        let height = std::cmp::min(
            area.height - 10,
            (self.rope().len_lines() - self.view_lines_offset()) as u16,
        );

        let blank = Buffer::empty(Rect { height, ..area });
        buffer.merge(&blank);

        let view_slice = {
            let view_start_line = self.view_lines_offset();
            let view_end_line = view_start_line + (area.height as usize - 1);
            let buffer_end_line = self.rope().len_lines();
            let start_char_index = self.rope().line_to_char(view_start_line);
            if buffer_end_line <= view_end_line {
                self.rope().slice(start_char_index..)
            } else {
                let end_char_index = self.rope().line_to_char(view_end_line);
                self.rope().slice(start_char_index..end_char_index)
            }
        };

        let chunks = Layout::default()
            .direction(Direction::Horizontal)
            .constraints([Constraint::Length(4), Constraint::Min(0)].as_ref())
            .split(area);

        for (i, line) in view_slice.lines().enumerate() {
            let line = if line.len_chars().saturating_sub(self.view_columns_offset()) > 0 {
                Cow::from(line.slice(self.view_columns_offset()..))
                    .trim_end()
                    .to_string()
            } else {
                "".to_string()
            };

            buffer.set_stringn(
                chunks[0].left(),
                chunks[0].top() + i as u16,
                format!("{:>3} ", self.view_lines_offset() + i + 1),
                chunks[0].width as usize,
                Style::default().bg(Color::Rgb(0xEE, 0xEE, 0xEE)),
            );

            buffer.set_stringn(
                chunks[1].left(),
                chunks[1].top() + i as u16,
                line,
                chunks[1].width as usize,
                Style::default(),
            );
        }

        for range in &self.selection().ranges {
            let anchor_cursor = (&range.anchor, Style::default().bg(Color::LightCyan));
            let head_cursor = (&range.head, Style::default().bg(Color::LightYellow));

            for (cursor, style) in [anchor_cursor, head_cursor] {
                let buffer_line = cursor.line;
                let buffer_column = cursor.column;
                let view_line = buffer_line.saturating_sub(self.view_lines_offset());
                let view_column = buffer_column.saturating_sub(self.view_columns_offset());

                let cursor_visible = [
                    buffer_line >= self.view_lines_offset(),
                    buffer_column >= self.view_columns_offset(),
                    view_line < chunks[1].bottom() as usize,
                    view_column < chunks[1].right() as usize,
                ]
                .iter()
                .all(|x| *x);

                if cursor_visible {
                    buffer
                        .get_mut(
                            chunks[1].left() + view_column as u16,
                            chunks[1].top() + view_line as u16,
                        )
                        .set_style(style);
                }
            }
        }
    }
}

impl Widget for &CommandLine {
    fn render(self, area: tui::layout::Rect, buffer: &mut tui::buffer::Buffer) {
        use tui::style::Color;
        use tui::widgets::Block;

        Block::default()
            .title(format!(":{}", self.command()))
            .render(area, buffer);

        buffer
            .get_mut(area.left() + self.command().len() as u16 + 1, area.top())
            .set_bg(Color::White);
    }
}
