#![warn(
    clippy::use_self,
    clippy::cast_possible_truncation,
    clippy::cast_possible_wrap,
    clippy::cast_sign_loss
)]

use crate::terminal::Terminal;
use crossterm::event::{Event, KeyCode, KeyModifiers, MouseButton, MouseEvent, MouseEventKind};
use indigo_core::*;
use std::{
    num::NonZeroUsize,
    time::{Duration, Instant},
};
use tui::{
    buffer::Buffer as Surface,
    layout::{Constraint, Direction, Layout, Rect},
    style::{Color, Style},
    widgets::Widget,
};

mod terminal;

pub fn run(editor: Editor) {
    let mut terminal = Terminal::new();
    let mut tui = Tui::new(editor);

    let frames_per_second = 120;
    let frame_time = Duration::from_secs(1) / frames_per_second;

    let mut last_render_time = Instant::now() - (frame_time * 2);
    let mut last_size = terminal.size().unwrap();

    while !tui.quit {
        if last_render_time.elapsed() >= frame_time {
            terminal
                .draw(|frame| frame.render_widget(&tui, last_size))
                .unwrap();
            last_render_time = Instant::now();
        }

        let event = crossterm::event::read().unwrap();

        if matches!(event, Event::Resize(_, _)) {
            last_size = terminal.size().unwrap();
        };

        let areas = areas(&tui.editor, last_size);
        handle_event(&mut tui, &areas, event);
    }
}

struct Tui {
    editor: Editor,
    quit: bool,
}

impl Tui {
    fn new(editor: Editor) -> Self {
        Self {
            editor,
            quit: false,
        }
    }
}

struct Areas {
    numbers_area: Rect,
    buffer_area: Rect,
    status_area: Rect,
    command_area: Rect,
}

fn areas(editor: &Editor, area: Rect) -> Areas {
    let vertical = Layout::default()
        .direction(Direction::Vertical)
        .constraints([
            // number + buffer
            Constraint::Min(0),
            // status
            Constraint::Length(1),
            // command
            Constraint::Length(1),
        ])
        .split(area);

    #[allow(clippy::cast_possible_truncation, clippy::cast_sign_loss)]
    // Change to work with integers instead of floats once this is stabilized:
    // https://github.com/rust-lang/rust/issues/70887
    let number_width = {
        let n = editor
            .current_buffer()
            .contents()
            .len_lines()
            .saturating_sub(1);
        assert!(n != 0);
        let n = n as f64;
        let digits = 1.0 + n.log10().floor();
        (digits.max(2.0) + 1.0) as u16
    };

    let horizontal = Layout::default()
        .direction(Direction::Horizontal)
        .constraints([
            // number
            Constraint::Length(number_width),
            // buffer
            Constraint::Min(0),
        ])
        .split(vertical[0]);

    Areas {
        numbers_area: horizontal[0],
        buffer_area: horizontal[1],
        status_area: vertical[1],
        command_area: vertical[2],
    }
}

fn handle_event(tui: &mut Tui, areas: &Areas, event: Event) {
    #[allow(clippy::single_match)]
    match &mut tui.editor.mode {
        Mode::Normal(_) => handle_event_normal(tui, areas, event),
        _ => {}
    }
}

fn handle_event_normal(tui: &mut Tui, areas: &Areas, event: Event) {
    let buffer = tui.editor.current_buffer_mut();

    match event {
        Event::Key(key_event) => match (key_event.modifiers, key_event.code) {
            (KeyModifiers::CONTROL, KeyCode::Char('c')) => {
                tui.quit = true;
            }
            (KeyModifiers::CONTROL, KeyCode::Char('p')) => {
                panic!();
            }
            (KeyModifiers::NONE, KeyCode::Up) => {
                *buffer = buffer.scroll_up(1);
            }
            (KeyModifiers::NONE, KeyCode::Down) => {
                *buffer = buffer.scroll_down(1);
            }
            (KeyModifiers::NONE, KeyCode::Left) => {
                *buffer = buffer.scroll_left(1);
            }
            (KeyModifiers::NONE, KeyCode::Right) => {
                *buffer = buffer.scroll_right(1);
            }
            (KeyModifiers::NONE, KeyCode::Char('h')) => {
                *buffer = buffer
                    .update_selection(|rope, selection| {
                        selection
                            .update_ranges(|_, range| *range::move_left(range, rope, 1))
                            .valid_for(rope)
                            .unwrap()
                    })
                    .scroll_to_selection(areas.buffer_area.height);
            }
            (KeyModifiers::NONE, KeyCode::Char('j')) => {
                *buffer = buffer
                    .update_selection(|rope, selection| {
                        selection
                            .update_ranges(|_, range| *range::move_down(range, rope, 1))
                            .valid_for(rope)
                            .unwrap()
                    })
                    .scroll_to_selection(areas.buffer_area.height);
            }
            (KeyModifiers::NONE, KeyCode::Char('k')) => {
                *buffer = buffer
                    .update_selection(|rope, selection| {
                        selection
                            .update_ranges(|_, range| *range::move_up(range, rope, 1))
                            .valid_for(rope)
                            .unwrap()
                    })
                    .scroll_to_selection(areas.buffer_area.height);
            }
            (KeyModifiers::NONE, KeyCode::Char('l')) => {
                *buffer = buffer
                    .update_selection(|rope, selection| {
                        selection
                            .update_ranges(|_, range| *range::move_right(range, rope, 1))
                            .valid_for(rope)
                            .unwrap()
                    })
                    .scroll_to_selection(areas.buffer_area.height);
            }
            (KeyModifiers::SHIFT, KeyCode::Char('H')) => {
                *buffer = buffer
                    .update_selection(|rope, selection| {
                        selection
                            .update_ranges(|_, range| *range::extend_left(range, rope, 1))
                            .valid_for(rope)
                            .unwrap()
                    })
                    .scroll_to_selection(areas.buffer_area.height);
            }
            (KeyModifiers::SHIFT, KeyCode::Char('J')) => {
                *buffer = buffer
                    .update_selection(|rope, selection| {
                        selection
                            .update_ranges(|_, range| *range::extend_down(range, rope, 1))
                            .valid_for(rope)
                            .unwrap()
                    })
                    .scroll_to_selection(areas.buffer_area.height);
            }
            (KeyModifiers::SHIFT, KeyCode::Char('K')) => {
                *buffer = buffer
                    .update_selection(|rope, selection| {
                        selection
                            .update_ranges(|_, range| *range::extend_up(range, rope, 1))
                            .valid_for(rope)
                            .unwrap()
                    })
                    .scroll_to_selection(areas.buffer_area.height);
            }
            (KeyModifiers::SHIFT, KeyCode::Char('L')) => {
                *buffer = buffer
                    .update_selection(|rope, selection| {
                        selection
                            .update_ranges(|_, range| *range::extend_right(range, rope, 1))
                            .valid_for(rope)
                            .unwrap()
                    })
                    .scroll_to_selection(areas.buffer_area.height);
            }
            _ => {}
        },
        Event::Mouse(mouse_event) => match mouse_event.kind {
            MouseEventKind::ScrollUp => {
                *buffer = buffer.scroll_up(3);
            }
            MouseEventKind::ScrollDown => {
                *buffer = buffer.scroll_down(3);
            }
            MouseEventKind::Up(_) => {}
            MouseEventKind::Down(MouseButton::Left) => {
                if let Some(head) = mouse_to_buffer_position(&mouse_event, areas, buffer) {
                    *buffer = buffer.update_selection(|rope, _selection| {
                        Selection::from(Range::from(head)).valid_for(rope).unwrap()
                    });
                }
            }
            MouseEventKind::Down(MouseButton::Right) => {
                if let Some(head) = mouse_to_buffer_position(&mouse_event, areas, buffer) {
                    *buffer = buffer.update_selection(|rope, selection| {
                        selection
                            .update_primary_range(|_, range| Range::from((range.anchor(), head)))
                            .valid_for(rope)
                            .unwrap()
                    });
                }
            }
            MouseEventKind::Down(_) => {}
            MouseEventKind::Moved => {}
            MouseEventKind::Drag(MouseButton::Left) => {
                if let Some(head) = mouse_to_buffer_position(&mouse_event, areas, buffer) {
                    *buffer = buffer.update_selection(|rope, selection| {
                        let anchor = selection.primary_range().1.anchor();
                        Selection::from(Range::from((anchor, head)))
                            .valid_for(rope)
                            .unwrap()
                    });
                }
            }
            MouseEventKind::Drag(_) => {}
        },
        Event::Resize(_, _) => {}
    }
}

fn mouse_to_buffer_position(
    mouse_event: &MouseEvent,
    areas: &Areas,
    buffer: &Buffer,
) -> Option<Position> {
    let line_range = areas.buffer_area.top()..areas.buffer_area.bottom();
    let line = if line_range.contains(&mouse_event.row) {
        mouse_event.row - areas.buffer_area.top()
    } else {
        return None;
    };
    let line = NonZeroUsize::new(usize::from(line) + 1 + buffer.vertical_scroll_offset()).unwrap();

    let column_range = areas.buffer_area.left()..areas.buffer_area.right();
    let column = if column_range.contains(&mouse_event.column) {
        mouse_event.column - areas.buffer_area.left()
    } else {
        return None;
    };
    let column =
        NonZeroUsize::new(usize::from(column) + 1 + buffer.horizontal_scroll_offset()).unwrap();

    Some(*Position::from((line, column)).corrected(buffer.contents()))
}

impl Widget for &Tui {
    fn render(self, area: Rect, surface: &mut Surface) {
        let areas = areas(&self.editor, area);

        render_numbers(self, areas.numbers_area, surface);
        render_buffer(self, areas.buffer_area, surface);
        render_selection(self, areas.buffer_area, surface);
        render_status(self, areas.status_area, surface);
        render_command(self, areas.command_area, surface);
    }
}

fn render_numbers(tui: &Tui, area: Rect, surface: &mut Surface) {
    let buffer = tui.editor.current_buffer();

    let total_lines = buffer.contents().len_lines().saturating_sub(1);

    for y in area.top()..area.bottom() {
        let line_number = usize::from(y) + buffer.vertical_scroll_offset() + 1;
        let number_width = usize::from(area.width) - 1;

        if line_number <= total_lines {
            surface.set_stringn(
                area.x,
                y,
                format!("{:>number_width$}│", line_number),
                number_width + 1,
                Style::default(),
            );
        } else {
            surface.get_mut(area.x, y).set_char('~');
        }
    }
}

fn render_buffer(tui: &Tui, area: Rect, surface: &mut Surface) {
    let buffer = tui.editor.current_buffer();

    for y in area.top()..area.bottom() {
        let line_index = usize::from(y) + buffer.vertical_scroll_offset();

        if let Some(line) = buffer.contents().get_line(line_index) {
            if let Some(line) = line.get_slice(buffer.horizontal_scroll_offset()..) {
                surface.set_stringn(
                    area.x,
                    y,
                    line.to_string(),
                    usize::from(area.width),
                    Style::default(),
                );
            }
        } else {
            break;
        }
    }
}

fn render_selection(tui: &Tui, area: Rect, surface: &mut Surface) {
    let buffer = tui.editor.current_buffer();
    let rope = buffer.contents();

    let yellow = Color::Rgb(0xFF, 0xD3, 0x3D);
    let light_yellow = Color::Rgb(0xFF, 0xF5, 0xB1);

    for range in buffer.selection().ranges() {
        let range_slice = range.to_rope_slice(rope).0;

        for (i, _) in range_slice.chars().enumerate() {
            let i = if range.is_forwards() {
                i + *range.anchor().to_rope_index(rope).0
            } else {
                i + *range.head().to_rope_index(rope).0
            };

            let buffer_position: Position = *Position::from_rope_index(rope, i).0;
            let buffer_line: usize = buffer_position.line.get() - 1;
            let buffer_column: usize = buffer_position.column.get() - 1;

            let view_line: usize = buffer_line.saturating_sub(buffer.vertical_scroll_offset());
            let view_column: usize =
                buffer_column.saturating_sub(buffer.horizontal_scroll_offset());

            let position_visible = [
                buffer_line >= buffer.vertical_scroll_offset(),
                buffer_column >= buffer.horizontal_scroll_offset(),
                view_line < area.height as usize,
                view_column < area.width as usize,
            ]
            .iter()
            .all(|x| *x);

            let style = if buffer_position == range.head() {
                Style::default().bg(yellow)
            } else {
                Style::default().bg(light_yellow)
            };

            if position_visible {
                surface
                    .get_mut(
                        area.left() + u16::try_from(view_column).unwrap(),
                        area.top() + u16::try_from(view_line).unwrap(),
                    )
                    .set_style(style);
            }
        }
    }
}

fn render_status(tui: &Tui, area: Rect, surface: &mut Surface) {
    let mode = match tui.editor.mode {
        Mode::Normal(_) => "normal",
        Mode::Command(_) => "command",
        Mode::Insert(_) => "insert",
    };

    let buffer = tui.editor.current_buffer();

    let path = match buffer.path.as_ref() {
        Some(path) => path.as_str(),
        None => "[no file]",
    };

    let modified = if buffer.is_modified() { " [+]" } else { "" };

    let position = {
        let position @ Position { line, column } = buffer.selection().primary_range().1.head();
        let index = *position.to_rope_index(buffer.contents()).0;
        format!("{line}:{column}#{index}")
    };

    surface.set_string(
        area.x,
        area.y,
        format!("{mode} {path}{modified} {position}"),
        Style::default(),
    );
}

fn render_command(tui: &Tui, area: Rect, surface: &mut Surface) {
    if let Mode::Command(CommandMode { command_line }) = &tui.editor.mode {
        surface.set_string(
            area.x,
            area.y,
            format!(":{}", command_line.contents()),
            Style::default(),
        );

        let cursor_index = u16::try_from(command_line.cursor_index()).unwrap();

        let yellow = Color::Rgb(0xFF, 0xD3, 0x3D);

        surface
            .get_mut(area.x + 1 + cursor_index, area.y)
            .set_style(Style::default().bg(yellow));
    }
}
