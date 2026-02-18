use camino::Utf8PathBuf;
use clap::Parser as _;
use etcetera::app_strategy::{AppStrategy as _, Xdg};
use indigo_core::{
    fs::RealFs,
    prelude::{Buffer, *},
    rope::LINE_TYPE,
};
use indigo_tui::{
    areas::{Areas, byte_index_to_area, line_index_to_area},
    event::{handle_event, should_skip_event},
    terminal,
    terminal::TerminalGuard,
};
use pathdiff::diff_utf8_paths;
use ratatui::{
    crossterm,
    prelude::{Buffer as Surface, *},
};
use std::{
    borrow::Cow,
    cmp::{max, min},
    env, fs,
    process::ExitCode,
    sync::Arc,
};

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
        //
        // NOTE: Replace with `std::panic::set_backtrace_style` once it stabilizes.
        // https://github.com/rust-lang/rust/issues/93346
        #[expect(unsafe_code)]
        unsafe {
            std::env::set_var("RUST_BACKTRACE", "1");
        }
    }

    let args = Args::parse();

    let xdg = init_xdg()?;

    init_tracing(&args, &xdg)?;

    let mut terminal = terminal::init()?;

    let result = run(&args, &mut terminal);

    drop(terminal);

    result
}

fn init_xdg() -> anyhow::Result<Xdg> {
    use etcetera::app_strategy::AppStrategyArgs;

    Ok(Xdg::new(AppStrategyArgs {
        top_level_domain: String::from("com"),
        author: String::from("Evan Relf"),
        app_name: String::from("Indigo"),
    })?)
}

fn init_tracing(args: &Args, xdg: &Xdg) -> anyhow::Result<()> {
    use tracing_subscriber::{EnvFilter, Registry, fmt, prelude::*};

    let log_path = xdg
        .in_state_dir("tui.log")
        .expect("Always returns a path for the XDG strategy");

    fs::create_dir_all(log_path.parent().expect("Log file exists in a directory"))?;

    let log_file = fs::OpenOptions::new()
        .append(true)
        .create(true)
        .open(log_path)?;

    let filter_layer = EnvFilter::new(args.log_filter.clone());

    let fmt_layer = fmt::Layer::default().with_writer(log_file);

    let subscriber = Registry::default().with(fmt_layer).with(filter_layer);

    subscriber.init();

    Ok(())
}

fn run(args: &Args, terminal: &mut TerminalGuard) -> anyhow::Result<ExitCode> {
    let buffer = if let Some(path) = &args.file {
        Buffer::open(&RealFs, path)?
    } else {
        Buffer::new()
    };

    let mut editor = Editor::from(buffer);

    editor.fs = Arc::new(RealFs);
    editor.pwd = Some(Utf8PathBuf::try_from(env::current_dir()?)?);

    let mut areas = Areas::default();

    let exit_code = 'frame: loop {
        terminal.draw(|frame| {
            let area = frame.area();
            let surface = frame.buffer_mut();
            areas = Areas::new(&editor, area);
            editor.window_mut().set_height(areas.text.height);
            render(&editor, area, surface);
        })?;

        let event = 'event_read: loop {
            let event = crossterm::event::read()?;
            if event.is_resize() {
                continue 'frame;
            }
            if !should_skip_event(&event) {
                break 'event_read event;
            }
        };

        handle_event(&mut editor, terminal, areas, event)?;

        if let Some(exit_code) = editor.exit_code() {
            break exit_code;
        }
    };

    Ok(exit_code)
}

pub const LIGHT_YELLOW: Color = Color::from_u32(0x00_fff5b1);
pub const DARK_YELLOW: Color = Color::from_u32(0x00_ffd33d);
pub const LIGHT_RED: Color = Color::from_u32(0x00_ffdce0);
pub const RED: Color = Color::from_u32(0x00_d73a4a);

pub fn render(editor: &Editor, area: Rect, surface: &mut Surface) {
    let areas = Areas::new(editor, area);
    render_status_bar(editor, areas.status_bar, surface);
    render_line_numbers(editor, areas.line_numbers, surface);
    render_dots(editor, areas.dots, surface);
    render_text(editor, areas.text, surface);
    render_selection(editor, areas.text, surface);
    render_scroll_bar(editor, areas.scroll_bar, surface);
}

fn render_status_bar(editor: &Editor, mut area: Rect, surface: &mut Surface) {
    if let Mode::Command(ref command_mode) = editor.mode {
        if let Some(cell) = surface.cell_mut(area.as_position()) {
            cell.set_char(':');
        } else {
            unreachable!();
        }

        area.x += 1;
        area.width -= 1;

        Line::raw(Cow::<str>::from(command_mode.rope())).render(area, surface);

        if let Some(rect) = byte_index_to_area(
            command_mode.cursor().byte_offset(),
            command_mode.rope(),
            0,
            area,
        ) {
            surface.set_style(rect, Style::default().bg(DARK_YELLOW));
        }

        return;
    }

    if let Mode::Prompt(ref prompt_mode) = editor.mode {
        format!("{}:", prompt_mode.prompt()).render(area, surface);

        area.x += u16::try_from(prompt_mode.prompt().display_width())
            .expect("Prompt width is less than u16::MAX")
            + 1;
        area.width -= 1;

        Line::raw(Cow::<str>::from(prompt_mode.rope())).render(area, surface);

        if let Some(rect) = byte_index_to_area(
            prompt_mode.cursor().byte_offset(),
            prompt_mode.rope(),
            0,
            area,
        ) {
            surface.set_style(rect, Style::default().bg(DARK_YELLOW));
        }

        return;
    }

    if let Some(message) = &editor.message {
        match message {
            Ok(message) => Line::raw(message).render(area, surface),
            Err(message) => Line::raw(message).bg(LIGHT_RED).render(area, surface),
        }
    } else {
        let mode = match &editor.mode {
            Mode::Normal(_) => "normal",
            Mode::Seek(seek_mode) => {
                use indigo_core::mode::seek::{
                    SeekDirection::{Next, Prev},
                    SeekInclude::{Onto, Until},
                    SeekSelect::{Extend, Move},
                };
                match (&seek_mode.select, &seek_mode.include, &seek_mode.direction) {
                    (Move, Until, Prev) => "move until prev",
                    (Extend, Until, Prev) => "extend until prev",
                    (Move, Until, Next) => "move until next",
                    (Extend, Until, Next) => "extend until next",
                    (Move, Onto, Prev) => "move onto prev",
                    (Extend, Onto, Prev) => "extend onto prev",
                    (Move, Onto, Next) => "move onto next",
                    (Extend, Onto, Next) => "extend onto next",
                }
            }
            Mode::Goto(_) => "goto",
            Mode::Insert(_) => "insert",
            Mode::Command(_) | Mode::Prompt(_) => unreachable!(),
        };

        let path = match (&editor.pwd, &editor.window().buffer().kind()) {
            (_, BufferKind::Scratch) => String::from("*scratch*"),
            (None, BufferKind::File { path, .. }) => path.to_string(),
            (Some(pwd), BufferKind::File { path, .. }) => match diff_utf8_paths(path, pwd) {
                None => path.to_string(),
                Some(path) => path.to_string(),
            },
        };

        let window = editor.window();
        let selection = window.buffer().selection();
        let state = selection.state();
        let ranges = state.ranges.len();
        let primary = &state.ranges[state.primary_range];
        let tail = primary.tail.byte_offset;
        let head = primary.head.byte_offset;
        let goal = primary.goal_column;

        let count = match editor.mode.count() {
            Some(count) if count.get() == usize::MAX => &String::from(" count=∞"),
            Some(count) => &format!(" count={count}"),
            None => "",
        };

        Line::raw(format!(
            "{path} · {mode} · ranges={ranges} tail={tail} head={head} goal={goal}{count}"
        ))
        .render(area, surface);
    }
}

fn render_line_numbers(editor: &Editor, area: Rect, surface: &mut Surface) {
    let window = editor.window();

    let buffer = window.buffer();

    let total_lines = buffer.rope().len_lines_indigo();

    for (i, row) in area.rows().enumerate() {
        let line_number = i + window.vertical_scroll() + 1;

        if line_number > total_lines {
            break;
        }

        Line::raw(format!("{line_number} "))
            .fg(Color::from_u32(0x00_cccccc))
            .right_aligned()
            .render(row, surface);
    }
}

fn render_scroll_bar(editor: &Editor, area: Rect, surface: &mut Surface) {
    const LOWER_BLOCKS: [&str; 9] = [" ", "▁", "▂", "▃", "▄", "▅", "▆", "▇", "█"];
    const UNITS_PER_CELL: usize = 8;
    const MIN_UNITS: usize = 8;

    if area.height == 0 {
        return;
    }

    let gutter_color = Color::from_u32(0x00_cccccc);
    let bar_color = Color::Black;

    let window = editor.window();
    let buffer = window.buffer();

    let text_lines = buffer.rope().len_lines_indigo();
    let window_lines = usize::from(window.height());
    let scroll_lines = text_lines.saturating_add(window_lines.saturating_sub(1));
    let gutter_cells = usize::from(area.height);
    let gutter_units = gutter_cells * UNITS_PER_CELL;

    if gutter_units == 0 || scroll_lines == 0 {
        return;
    }

    let bar_units = ((gutter_units * window_lines) / scroll_lines).clamp(MIN_UNITS, gutter_units);

    let current_scroll = window.vertical_scroll();
    let max_scroll = text_lines.saturating_sub(1);

    let bar_start_units = if max_scroll == 0 || gutter_units == bar_units {
        0
    } else {
        ((gutter_units - bar_units) * current_scroll) / max_scroll
    };
    let bar_end_units = bar_start_units + bar_units;

    for (i, row) in area.rows().enumerate() {
        let cell_top = i * UNITS_PER_CELL;
        let cell_bottom = cell_top + UNITS_PER_CELL;

        let overlap_start = max(cell_top, bar_start_units);
        let overlap_end = min(cell_bottom, bar_end_units);
        let overlap_units = overlap_end.saturating_sub(overlap_start);

        if overlap_units == 0 {
            " ".bg(gutter_color).render(row, surface);
            continue;
        }

        if overlap_units == UNITS_PER_CELL {
            " ".bg(bar_color).render(row, surface);
            continue;
        }

        if overlap_end == cell_bottom && overlap_start > cell_top {
            let h = overlap_units;
            LOWER_BLOCKS[h]
                .fg(bar_color)
                .bg(gutter_color)
                .render(row, surface);
            continue;
        }

        if overlap_start == cell_top && overlap_end < cell_bottom {
            let empty = UNITS_PER_CELL - overlap_units;
            LOWER_BLOCKS[empty]
                .fg(gutter_color)
                .bg(bar_color)
                .render(row, surface);
            continue;
        }

        unreachable!();
    }
}

fn render_dots(editor: &Editor, area: Rect, surface: &mut Surface) {
    let window = editor.window();

    let buffer = window.buffer();

    let total_lines = buffer.rope().len_lines_indigo();

    let grid_scale = 1;

    for (i, row) in area.rows().enumerate() {
        let line_number = i + window.vertical_scroll() + 1;

        if !line_number.is_multiple_of(grid_scale) {
            continue;
        }

        if line_number <= total_lines {
            continue;
        }

        for (j, cell) in row.columns().enumerate() {
            if !j.is_multiple_of(grid_scale * 2) {
                continue;
            }

            "⢀".fg(Color::from_u32(0x00_dddddd)).render(cell, surface);
        }
    }
}

fn render_text(editor: &Editor, area: Rect, surface: &mut Surface) {
    let window = editor.window();

    let buffer = window.buffer();

    let lines = buffer.rope().lines_at(window.vertical_scroll(), LINE_TYPE);

    let rows = area.rows();

    // TODO(horizontal_scroll): Once horizontal scroll is added, this needs to truncate lines.
    'line: for (line, mut rect) in lines.zip(rows) {
        'grapheme: for grapheme in line.graphemes() {
            let span = match grapheme.get_char(0) {
                Ok('\t') => Span::styled("→       ", Color::from_u32(0x00_eeeeee)),
                Ok('\n') => Span::styled("¬", Color::from_u32(0x0_eeeeee)),
                _ => Span::raw(grapheme),
            };

            let width_usize = grapheme.display_width();

            let width_u16 = u16::try_from(width_usize)
                .expect("No grapheme exists with a display width > u16::MAX");

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
}

fn render_selection(editor: &Editor, area: Rect, surface: &mut Surface) {
    let window = editor.window();

    let buffer = window.buffer();

    let rope = buffer.rope();

    let vertical_scroll = window.vertical_scroll();

    buffer.selection().for_each(|range| {
        let start_line = rope.byte_to_line_idx(range.start().byte_offset(), LINE_TYPE);

        let end_byte = range.end().byte_offset().saturating_sub(1);
        let end_line = rope.byte_to_line_idx(end_byte, LINE_TYPE);

        let grapheme_area =
            |byte_index| byte_index_to_area(byte_index, rope, vertical_scroll, area);

        let line_area = |line_index| line_index_to_area(line_index, rope, vertical_scroll, area);

        if range.is_empty() {
            if let Some(rect) = grapheme_area(range.head().byte_offset()) {
                surface.set_style(rect, Style::default().bg(RED));
            }
            return;
        }

        for (line_index, mut line_rect) in (start_line..=end_line)
            .filter_map(|line_index| line_area(line_index).map(|rect| (line_index, rect)))
        {
            if line_index == start_line {
                if let Some(start_rect) = grapheme_area(range.start().byte_offset()) {
                    let delta = start_rect.x - line_rect.x;
                    line_rect.x += delta;
                    line_rect.width -= delta;
                } else {
                    // TODO(horizontal_scroll): We continue here because we know the range start is
                    // off the screen to the right. Once horizontal scrolling is added, we'll need
                    // to handle when the range is off the screen to the left. `grapheme_area`
                    // doesn't say which direction the index is off screen.
                    continue;
                }
            }
            #[expect(clippy::collapsible_if)]
            if line_index == end_line {
                if let Some(end_rect) = grapheme_area(range.end().byte_offset().saturating_sub(1)) {
                    let delta = line_rect.right() - end_rect.right();
                    line_rect.width -= delta;
                }
            }
            surface.set_style(line_rect, Style::default().bg(LIGHT_YELLOW));
        }

        #[expect(clippy::collapsible_else_if)]
        if range.is_backward() {
            if let Some(rect) = grapheme_area(range.head().byte_offset()) {
                surface.set_style(rect, Style::default().bg(DARK_YELLOW));
            }
        } else {
            if let Some(rect) = grapheme_area(range.head().byte_offset().saturating_sub(1)) {
                surface.set_style(rect, Style::default().bg(DARK_YELLOW));
            }
        }
    });
}
