mod areas;
mod event;
mod key;
mod terminal;

use crate::{areas::Areas, event::handle_event};
use camino::Utf8PathBuf;
use clap::Parser as _;
use indigo_core::prelude::*;
use ratatui::{
    prelude::{Buffer as Surface, Rect, Style, Widget as _},
    style::{Color, Modifier},
    text::{Line, Span},
};
use std::{fs, io, process::ExitCode};
use tracing_subscriber::EnvFilter;

#[derive(Debug, clap::Parser)]
struct Args {
    file: Option<Utf8PathBuf>,

    #[clap(long, env = "INDIGO_LOG", default_value_t)]
    log_filter: String,
}

fn main() -> anyhow::Result<ExitCode> {
    #[cfg(debug_assertions)]
    if std::env::var("RUST_BACKTRACE").is_err() {
        // SAFETY: At this point the program is single-threaded. There are no other threads that
        // could be reading from or writing to the environment.
        // TODO: Replace with `std::panic::set_backtrace_style` once it stabilizes.
        // https://github.com/rust-lang/rust/issues/93346
        unsafe {
            std::env::set_var("RUST_BACKTRACE", "1");
        }
    }

    let args = Args::parse();

    init_tracing_subscriber(&args)?;

    let rope = if let Some(path) = args.file {
        let file = fs::File::open(path)?;
        Rope::from_reader(io::BufReader::new(file))?
    } else {
        Rope::new()
    };

    let mut editor = Editor::from_rope(rope);

    let mut terminal = terminal::enter()?;

    let mut areas = Areas::default();

    loop {
        terminal.draw(|frame| {
            areas = Areas::new(&editor, frame.area());
            editor.height = usize::from(areas.text.height);
            let surface = frame.buffer_mut();
            render(&editor, areas, surface);
        })?;

        let event = crossterm::event::read()?;

        handle_event(&mut editor, &mut terminal, areas, &event)?;

        if let Some(exit_code) = editor.exit {
            return Ok(ExitCode::from(exit_code));
        }
    }
}

fn init_tracing_subscriber(args: &Args) -> anyhow::Result<()> {
    use etcetera::app_strategy::{AppStrategy as _, AppStrategyArgs, Xdg};

    let xdg = Xdg::new(AppStrategyArgs {
        top_level_domain: String::from("com"),
        author: String::from("Evan Relf"),
        app_name: String::from("Indigo"),
    })?;

    let log_path = xdg.in_state_dir("tui.log").unwrap();

    fs::create_dir_all(log_path.parent().unwrap())?;

    let log_file = fs::OpenOptions::new()
        .append(true)
        .create(true)
        .open(log_path)?;

    tracing_subscriber::fmt()
        .with_env_filter(EnvFilter::new(args.log_filter.clone()))
        .with_writer(log_file)
        .init();

    Ok(())
}

fn render(editor: &Editor, areas: Areas, surface: &mut Surface) {
    render_navigation_bar(editor, areas.navigation_bar, surface);
    render_line_numbers(editor, areas.line_numbers, surface);
    render_tildes(editor, areas.line_numbers, surface);
    render_text(editor, areas.text, surface);
    render_selection(editor, areas.text, surface);
    render_status_bar(editor, areas.status_bar, surface);
}

fn render_navigation_bar(_editor: &Editor, area: Rect, surface: &mut Surface) {
    Line::styled(" ", Modifier::UNDERLINED).render(area, surface);
}

fn render_line_numbers(editor: &Editor, area: Rect, surface: &mut Surface) {
    let total_lines = editor.rope().len_lines_indigo();

    for (i, row) in area.rows().enumerate() {
        let line_number = i + editor.vertical_scroll() + 1;

        if line_number > total_lines {
            break;
        }

        Line::raw(format!("{line_number}│"))
            .right_aligned()
            .render(row, surface);
    }
}

fn render_tildes(editor: &Editor, area: Rect, surface: &mut Surface) {
    let total_lines = editor.rope().len_lines_indigo();

    for (i, row) in area.rows().enumerate() {
        let line_number = i + editor.vertical_scroll() + 1;

        if line_number <= total_lines {
            continue;
        }

        let bottom = row.y + 1 == area.bottom();

        let style = if bottom {
            Modifier::UNDERLINED
        } else {
            Modifier::empty()
        };

        Line::styled("~", style).render(row, surface);
    }
}

fn render_text(editor: &Editor, area: Rect, surface: &mut Surface) {
    let lines = editor.rope().lines_at(editor.vertical_scroll());

    let rows = area.rows();

    'line: for (line, mut rect) in lines.zip(rows) {
        'grapheme: for grapheme in line.graphemes() {
            let span = match grapheme.get_char(0) {
                Some('\t') => Span::styled("→       ", Color::Rgb(0xee, 0xee, 0xee)),
                Some('\n') => Span::styled("¬", Color::Rgb(0xee, 0xee, 0xee)),
                _ => Span::raw(grapheme),
            };

            let width_usize = grapheme.display_width();

            let width_u16 = u16::try_from(width_usize).unwrap();

            if rect.x + width_u16 > rect.right() {
                continue 'line;
            }

            if width_usize == 0 {
                continue 'grapheme;
            }

            span.render(rect, surface);

            rect.x += width_u16;
        }
    }

    if let Some(bottom_row) = area.rows().last() {
        surface.set_style(bottom_row, Modifier::UNDERLINED);
    }
}

fn render_selection(editor: &Editor, area: Rect, surface: &mut Surface) {
    const LIGHT_YELLOW: Color = Color::Rgb(0xff, 0xf5, 0xb1);
    const DARK_YELLOW: Color = Color::Rgb(0xff, 0xd3, 0x3d);
    const RED: Color = Color::Rgb(0xd7, 0x3a, 0x4a);

    let rope = editor.rope();
    let range = editor.range();
    let vertical_scroll = editor.vertical_scroll();

    let start_line = rope.char_to_line(range.start());

    let end_line = rope.char_to_line(range.end().saturating_sub(1));

    let grapheme_area = |char_index| char_index_to_area(char_index, rope, vertical_scroll, area);

    let line_area = |line_index| line_index_to_area(line_index, rope, vertical_scroll, area);

    if range.is_empty() {
        if let Some(rect) = grapheme_area(range.head()) {
            surface.set_style(rect, Style::default().bg(RED));
        }
        return;
    }

    for (line_index, mut line_rect) in (start_line..=end_line)
        .filter_map(|line_index| line_area(line_index).map(|rect| (line_index, rect)))
    {
        if line_index == start_line {
            if let Some(start_rect) = grapheme_area(range.start()) {
                let delta = start_rect.x - line_rect.x;
                line_rect.x += delta;
                line_rect.width -= delta;
            }
        }
        if line_index == end_line {
            if let Some(end_rect) = grapheme_area(range.end().saturating_sub(1)) {
                let delta = line_rect.right() - end_rect.right();
                line_rect.width -= delta;
            }
        }
        surface.set_style(line_rect, Style::default().bg(LIGHT_YELLOW));
    }

    #[expect(clippy::collapsible_else_if)]
    if range.is_backward() {
        if let Some(rect) = grapheme_area(range.head()) {
            surface.set_style(rect, Style::default().bg(DARK_YELLOW));
        }
    } else {
        if let Some(rect) = grapheme_area(range.head().saturating_sub(1)) {
            surface.set_style(rect, Style::default().bg(DARK_YELLOW));
        }
    }
}

fn line_index_to_area(
    line_index: usize,
    rope: &Rope,
    vertical_scroll: usize,
    area: Rect,
) -> Option<Rect> {
    if vertical_scroll > line_index {
        return None;
    }

    let y = area.y + u16::try_from(line_index - vertical_scroll).unwrap();

    if !(area.top()..area.bottom()).contains(&y) {
        return None;
    }

    let x = area.x;

    let line = rope.get_line(line_index)?;

    // Assumes a minimum grapheme width of 1
    let width = if line.len_chars() >= usize::from(area.width) {
        // Avoid expensive display width calculation if we know it would exceed the viewport width
        area.width
    } else {
        u16::try_from(line.display_width()).unwrap()
    };

    Some(Rect {
        x,
        y,
        width,
        height: 1,
    })
}

// TODO: Should this be `gap_index` instead of `char_index`?
fn char_index_to_area(
    char_index: usize,
    rope: &Rope,
    vertical_scroll: usize,
    area: Rect,
) -> Option<Rect> {
    let line_index = rope.try_char_to_line(char_index).ok()?;

    if vertical_scroll > line_index {
        return None;
    }

    let y = area.y + u16::try_from(line_index - vertical_scroll).unwrap();

    if !(area.top()..area.bottom()).contains(&y) {
        return None;
    }

    let line_char_index = rope.line_to_char(line_index);

    let char_index = rope.snap_to_grapheme_boundary(char_index, SnapBias::Before);

    let prefix_width = rope.slice(line_char_index..char_index).display_width();

    // TODO: When horizontal scroll is introduced, still return portion of rect that is visible.
    // Even if it starts to the left of the area, it might be wide enough to peek into the viewport.
    let x = area.x + u16::try_from(prefix_width).unwrap();

    if !(area.left()..area.right()).contains(&x) {
        return None;
    }

    let width = if rope.len_chars() == char_index {
        // Cursor at EOF
        1
    } else if let Some(grapheme) = rope.get_grapheme(char_index) {
        u16::try_from(grapheme.display_width()).unwrap()
    } else {
        // We're at EOF, but we already checked for that
        unreachable!()
    };

    Some(Rect {
        x,
        y,
        width,
        height: 1,
    })
}

fn render_status_bar(editor: &Editor, area: Rect, surface: &mut Surface) {
    let anchor = editor.range().anchor();

    let head = editor.range().head();

    let char_length = editor.range().char_length();

    let grapheme_length = editor.range().grapheme_length(editor.rope());

    let display_width = editor.range().rope_slice(editor.rope()).display_width();

    let eof = editor.range().head() == editor.rope().len_chars();

    let mode = match editor.mode {
        Mode::Normal { .. } => "normal",
        Mode::Insert => "insert",
    };

    let count = editor.mode.count();

    let status_bar = [
        format!("anchor={anchor}"),
        format!("head={head}"),
        format!("char_length={char_length}"),
        format!("grapheme_length={grapheme_length}"),
        format!("display_width={display_width}"),
        format!("eof={eof}"),
        format!("mode={mode}"),
        format!("count={count}"),
    ]
    .join(" ");

    Line::raw(status_bar).render(area, surface);
}
