#![warn(
    clippy::use_self,
    clippy::cast_possible_truncation,
    clippy::cast_possible_wrap,
    clippy::cast_sign_loss
)]

use crate::terminal::Terminal;
use indigo_core::*;
use std::time::{Duration, Instant};
use tui::widgets::Widget;

mod terminal;

pub fn run(editor: Editor) {
    let mut terminal = Terminal::new();
    let mut tui = Tui::new(editor);

    let frames_per_second = 120;
    let frame_time = Duration::from_secs(1) / frames_per_second;

    let mut last_render_time = Instant::now() - (frame_time * 2);

    while !tui.quit {
        if last_render_time.elapsed() >= frame_time {
            terminal
                .draw(|frame| frame.render_widget(&tui, frame.size()))
                .unwrap();
            last_render_time = Instant::now();
        }

        let event = crossterm::event::read().unwrap();
        handle_event(&mut tui, event);
    }
}

struct Tui {
    editor: Editor,
    quit: bool,
}

impl Tui {
    pub fn new(editor: Editor) -> Self {
        Self {
            editor,
            quit: false,
        }
    }
}

impl Widget for &Tui {
    fn render(self, area: tui::layout::Rect, buffer: &mut tui::buffer::Buffer) {
        render(self, area, buffer);
    }
}

fn handle_event(tui: &mut Tui, event: crossterm::event::Event) {
    #[allow(clippy::single_match)]
    match tui.editor.mode() {
        Mode::Normal { .. } => handle_event_normal(tui, event),
        _ => {}
    }
}

fn handle_event_normal(tui: &mut Tui, event: crossterm::event::Event) {
    use crossterm::event::{Event, KeyCode, KeyModifiers, MouseEventKind};

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
                *buffer = buffer.update_selection(|rope, selection| {
                    selection
                        .update_ranges(|_, range| *range::move_left(range, rope, 1))
                        .valid_for(rope)
                        .unwrap()
                });
            }
            (KeyModifiers::NONE, KeyCode::Char('j')) => {
                *buffer = buffer.update_selection(|rope, selection| {
                    selection
                        .update_ranges(|_, range| *range::move_down(range, rope, 1))
                        .valid_for(rope)
                        .unwrap()
                });
            }
            (KeyModifiers::NONE, KeyCode::Char('k')) => {
                *buffer = buffer.update_selection(|rope, selection| {
                    selection
                        .update_ranges(|_, range| *range::move_up(range, rope, 1))
                        .valid_for(rope)
                        .unwrap()
                });
            }
            (KeyModifiers::NONE, KeyCode::Char('l')) => {
                *buffer = buffer.update_selection(|rope, selection| {
                    selection
                        .update_ranges(|_, range| *range::move_right(range, rope, 1))
                        .valid_for(rope)
                        .unwrap()
                });
            }
            (KeyModifiers::SHIFT, KeyCode::Char('H')) => {
                *buffer = buffer.update_selection(|rope, selection| {
                    selection
                        .update_ranges(|_, range| *range::extend_left(range, rope, 1))
                        .valid_for(rope)
                        .unwrap()
                });
            }
            (KeyModifiers::SHIFT, KeyCode::Char('J')) => {
                *buffer = buffer.update_selection(|rope, selection| {
                    selection
                        .update_ranges(|_, range| *range::extend_down(range, rope, 1))
                        .valid_for(rope)
                        .unwrap()
                });
            }
            (KeyModifiers::SHIFT, KeyCode::Char('K')) => {
                *buffer = buffer.update_selection(|rope, selection| {
                    selection
                        .update_ranges(|_, range| *range::extend_up(range, rope, 1))
                        .valid_for(rope)
                        .unwrap()
                });
            }
            (KeyModifiers::SHIFT, KeyCode::Char('L')) => {
                *buffer = buffer.update_selection(|rope, selection| {
                    selection
                        .update_ranges(|_, range| *range::extend_right(range, rope, 1))
                        .valid_for(rope)
                        .unwrap()
                });
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
            _ => {}
        },
        Event::Resize(_, _) => {}
    }
}

fn render(tui: &Tui, area: tui::layout::Rect, surface: &mut tui::buffer::Buffer) {
    use tui::layout::{Constraint, Direction, Layout};

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

    let horizontal = Layout::default()
        .direction(Direction::Horizontal)
        .constraints([
            // number
            Constraint::Length(4),
            // buffer
            Constraint::Min(0),
        ])
        .split(vertical[0]);

    render_numbers(tui, horizontal[0], surface);
    render_buffer(tui, horizontal[1], surface);
    render_selection(tui, horizontal[1], surface);
    render_status(tui, vertical[1], surface);
    render_command(tui, vertical[2], surface);
}

fn render_numbers(tui: &Tui, area: tui::layout::Rect, surface: &mut tui::buffer::Buffer) {
    use tui::style::Style;

    let buffer = tui.editor.current_buffer();

    let total_lines = buffer.contents().len_lines().saturating_sub(1);

    for y in area.top()..area.bottom() {
        let line_number = usize::from(y) + buffer.vertical_scroll_offset() + 1;

        if line_number <= total_lines {
            surface.set_stringn(
                area.x,
                y,
                format!("{:>3} ", line_number),
                usize::from(area.width),
                Style::default(),
            );
        } else {
            surface.get_mut(area.x, y).set_char('~');
        }
    }
}

fn render_buffer(tui: &Tui, area: tui::layout::Rect, surface: &mut tui::buffer::Buffer) {
    use tui::style::Style;

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

fn render_selection(tui: &Tui, area: tui::layout::Rect, surface: &mut tui::buffer::Buffer) {
    use tui::style::{Color, Style};

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

fn render_status(_tui: &Tui, area: tui::layout::Rect, surface: &mut tui::buffer::Buffer) {
    use tui::style::Style;

    surface.set_string(area.x, area.y, "TODO status", Style::default());
}

fn render_command(_tui: &Tui, area: tui::layout::Rect, surface: &mut tui::buffer::Buffer) {
    use tui::style::Style;

    surface.set_string(area.x, area.y, "TODO command", Style::default());
}
