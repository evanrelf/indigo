use camino::Utf8PathBuf;
use clap::Parser as _;
use etcetera::app_strategy::{AppStrategy as _, Xdg};
use hdrhistogram::Histogram;
use indigo_core::{
    fs::RealFs,
    prelude::{Buffer, *},
};
use indigo_tui::{
    areas::{Areas, char_index_to_area, line_index_to_area},
    event::{handle_event, should_skip_event},
    terminal,
    terminal::TerminalGuard,
};
use ratatui::{
    crossterm,
    prelude::{Buffer as Surface, *},
};
use std::{borrow::Cow, cmp::max, env, fs, process::ExitCode, sync::Arc, time::Instant};

#[derive(Debug, clap::Parser)]
struct Args {
    file: Option<Utf8PathBuf>,

    #[clap(long, env = "INDIGO_LOG", default_value_t)]
    log_filter: Arc<str>,

    #[clap(long)]
    stats: bool,
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

    let xdg = init_etcetera()?;

    init_tracing_subscriber(&args, &xdg)?;

    let terminal = terminal::init();

    run(&args, terminal)
}

fn init_etcetera() -> anyhow::Result<Xdg> {
    use etcetera::app_strategy::AppStrategyArgs;

    Ok(Xdg::new(AppStrategyArgs {
        top_level_domain: String::from("com"),
        author: String::from("Evan Relf"),
        app_name: String::from("Indigo"),
    })?)
}

fn init_tracing_subscriber(args: &Args, xdg: &Xdg) -> anyhow::Result<()> {
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

struct Times {
    name: &'static str,
    enabled: bool,
    histogram: Histogram<u16>,
    instant: Option<Instant>,
}

impl Times {
    fn new(name: &'static str, enabled: bool) -> Self {
        Self {
            name,
            enabled,
            histogram: Histogram::<u16>::new(3).expect("Using supported number significant digits"),
            instant: None,
        }
    }

    fn start(&mut self) {
        if !self.enabled {
            return;
        }
        self.instant = Some(Instant::now());
    }

    fn stop(&mut self) {
        if !self.enabled {
            return;
        }
        let Some(instant) = self.instant else {
            return;
        };
        if let Ok(time) = u64::try_from(instant.elapsed().as_micros()) {
            let _ = self.histogram.record(time);
        }
        self.instant = None;
    }

    fn print_stats(&self) {
        if !self.enabled {
            return;
        }

        println!("=== {} ===", self.name);

        println!("{} frames", self.histogram.len());

        let p50_micros = self.histogram.value_at_quantile(0.50);
        let p50_fps = 1_000_000 / max(1, p50_micros);
        println!("P50:   {p50_micros:>8}μs {p50_fps:>6}fps");

        let p90_micros = self.histogram.value_at_quantile(0.90);
        let p90_fps = 1_000_000 / max(1, p90_micros);
        println!("P90:   {p90_micros:>8}μs {p90_fps:>6}fps");

        let p999_micros = self.histogram.value_at_quantile(0.999);
        let p999_fps = 1_000_000 / max(1, p999_micros);
        println!("P99.9: {p999_micros:>8}μs {p999_fps:>6}fps");

        let max_micros = self.histogram.max();
        let max_fps = 1_000_000 / max(1, max_micros);
        println!("Worst: {max_micros:>8}μs {max_fps:>6}fps");

        println!();
    }
}

fn run(args: &Args, mut terminal: TerminalGuard) -> anyhow::Result<ExitCode> {
    let buffer = if let Some(path) = &args.file {
        Buffer::open(&mut RealFs, path)?
    } else {
        Buffer::new()
    };

    let mut editor = Editor::from(buffer);

    editor.fs = Box::new(RealFs);
    editor.pwd = Some(Utf8PathBuf::try_from(env::current_dir()?)?);

    let mut areas = Areas::default();

    let mut frame_times = Times::new("full frame", args.stats);
    let mut render_times = Times::new("just render", args.stats);
    let mut event_read_times = Times::new("just event read", args.stats);

    let exit_code = loop {
        render_times.start();
        terminal.draw(|frame| {
            let _span = tracing::trace_span!("terminal draw").entered();
            let area = frame.area();
            let surface = frame.buffer_mut();
            areas = Areas::new(&editor, area);
            editor.window_mut().set_height(areas.text.height);
            render(&editor, area, surface);
        })?;
        render_times.stop();
        frame_times.stop();

        let event = loop {
            frame_times.start();
            event_read_times.start();
            let event = crossterm::event::read()?;
            if !should_skip_event(&event) {
                event_read_times.stop();
                break event;
            }
        };

        handle_event(&mut editor, &mut terminal, areas, event)?;

        if let Some(exit_code) = editor.exit {
            break exit_code;
        }
    };

    drop(terminal);

    frame_times.print_stats();
    render_times.print_stats();
    event_read_times.print_stats();

    Ok(exit_code)
}

pub const LIGHT_YELLOW: Color = Color::Rgb(0xff, 0xf5, 0xb1);
pub const DARK_YELLOW: Color = Color::Rgb(0xff, 0xd3, 0x3d);
pub const LIGHT_RED: Color = Color::Rgb(0xff, 0xdc, 0xe0);
pub const RED: Color = Color::Rgb(0xd7, 0x3a, 0x4a);

pub fn render(editor: &Editor, area: Rect, surface: &mut Surface) {
    let areas = Areas::new(editor, area);
    render_status_bar(editor, areas.status_bar, surface);
    render_line_numbers(editor, areas.line_numbers, surface);
    render_tildes(editor, areas.line_numbers, surface);
    render_text(editor, areas.text, surface);
    render_selection(editor, areas.text, surface);
}

fn render_status_bar(editor: &Editor, mut area: Rect, surface: &mut Surface) {
    if let Mode::Command(ref normal_mode) = editor.mode {
        if let Some(cell) = surface.cell_mut(area.as_position()) {
            cell.set_char(':');
        } else {
            unreachable!();
        }

        area.x += 1;
        area.width -= 1;

        Line::raw(Cow::<str>::from(normal_mode.rope())).render(area, surface);

        if let Some(rect) = char_index_to_area(
            normal_mode.cursor().char_offset(),
            normal_mode.rope(),
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
                use indigo_core::mode::{
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
            Mode::Command(_) => "command",
        };

        let path = match &editor.buffer.kind() {
            BufferKind::Scratch => "*scratch*",
            BufferKind::File { path, .. } => path.as_str(),
        };

        let anchor = editor.buffer.range().anchor().char_offset();

        let head = editor.buffer.range().head().char_offset();

        let count = match editor.mode.count() {
            Some(count) if count.get() == usize::MAX => &String::from(" count=∞"),
            Some(count) => &format!(" count={count}"),
            None => "",
        };

        Line::raw(format!("{path} · {mode} {anchor}-{head}{count}")).render(area, surface);
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

        Line::raw(format!("{line_number}│"))
            .right_aligned()
            .render(row, surface);
    }
}

fn render_tildes(editor: &Editor, area: Rect, surface: &mut Surface) {
    let window = editor.window();

    let buffer = window.buffer();

    let total_lines = buffer.rope().len_lines_indigo();

    for (i, row) in area.rows().enumerate() {
        let line_number = i + window.vertical_scroll() + 1;

        if line_number <= total_lines {
            continue;
        }

        Line::raw("~").render(row, surface);
    }
}

fn render_text(editor: &Editor, area: Rect, surface: &mut Surface) {
    let window = editor.window();

    let buffer = window.buffer();

    let lines = buffer.rope().lines_at(window.vertical_scroll());

    let rows = area.rows();

    'line: for (line, mut rect) in lines.zip(rows) {
        'grapheme: for grapheme in line.graphemes() {
            let span = match grapheme.get_char(0) {
                Some('\t') => Span::styled("→       ", Color::Rgb(0xee, 0xee, 0xee)),
                Some('\n') => Span::styled("¬", Color::Rgb(0xee, 0xee, 0xee)),
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

    let range = buffer.range();

    let vertical_scroll = window.vertical_scroll();

    let start_line = rope.char_to_line(range.start().char_offset());

    let end_line = rope.char_to_line(range.end().char_offset().saturating_sub(1));

    let grapheme_area = |char_index| char_index_to_area(char_index, rope, vertical_scroll, area);

    let line_area = |line_index| line_index_to_area(line_index, rope, vertical_scroll, area);

    if range.is_empty() {
        if let Some(rect) = grapheme_area(range.head().char_offset()) {
            surface.set_style(rect, Style::default().bg(RED));
        }
        return;
    }

    for (line_index, mut line_rect) in (start_line..=end_line)
        .filter_map(|line_index| line_area(line_index).map(|rect| (line_index, rect)))
    {
        if line_index == start_line {
            if let Some(start_rect) = grapheme_area(range.start().char_offset()) {
                let delta = start_rect.x - line_rect.x;
                line_rect.x += delta;
                line_rect.width -= delta;
            } else {
                // TODO: We continue here because we know the range start is off the screen to the
                // right. Once horizontal scrolling is added, we'll need to handle when the range is
                // off the screen to the left. `grapheme_area` doesn't say which direction the index
                // is off screen.
                continue;
            }
        }
        #[expect(clippy::collapsible_if)]
        if line_index == end_line {
            if let Some(end_rect) = grapheme_area(range.end().char_offset().saturating_sub(1)) {
                let delta = line_rect.right() - end_rect.right();
                line_rect.width -= delta;
            }
        }
        surface.set_style(line_rect, Style::default().bg(LIGHT_YELLOW));
    }

    #[expect(clippy::collapsible_else_if)]
    if range.is_backward() {
        if let Some(rect) = grapheme_area(range.head().char_offset()) {
            surface.set_style(rect, Style::default().bg(DARK_YELLOW));
        }
    } else {
        if let Some(rect) = grapheme_area(range.head().char_offset().saturating_sub(1)) {
            surface.set_style(rect, Style::default().bg(DARK_YELLOW));
        }
    }
}
