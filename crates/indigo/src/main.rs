use arrayvec::ArrayString;
use camino::Utf8PathBuf;
use clap::Parser as _;
use indigo::{
    areas::{Areas, byte_index_to_area, line_index_to_area},
    event::{IndigoEvent, handle_event, should_skip_event},
    terminal,
    terminal::TerminalGuard,
    theme::THEME,
};
use indigo_core::{
    fs::{RealFs, home_dir},
    mode::{insert, normal, prompt},
    prelude::{Buffer, Client, DisplayWidth as _, Keys, Mode, RopeExt as _, Server},
    syntax::Language,
};
use indigo_key::Keymap;
use indigo_text::rope::LINE_TYPE;
use ratatui::{
    crossterm,
    prelude::{Buffer as Surface, Line, Rect, Span, Style, Stylize as _, Widget as _},
};
use std::{
    borrow::Cow,
    cmp::{max, min},
    env,
    fmt::Write as _,
    fs, mem,
    process::ExitCode,
    sync::Arc,
};

// TODO: Figure out how to represent the mutual exclusion here more elegantly
#[derive(Debug, clap::Parser)]
struct Args {
    #[arg(conflicts_with = "keymap")]
    file: Option<Utf8PathBuf>,

    /// Print key mappings
    #[arg(long)]
    keymap: bool,

    #[arg(long, env = "INDIGO_LOG", default_value_t)]
    log_filter: String,

    #[command(flatten)]
    experimental: ExperimentalArgs,
}

#[expect(clippy::struct_field_names)]
#[derive(Debug, clap::Args)]
#[command(next_help_heading = "Experimental options")]
struct ExperimentalArgs {
    /// Execute keys at startup
    #[arg(long, name = "KEYS", conflicts_with = "keymap")]
    x_exec: Option<Keys>,

    /// Show debug bar
    #[arg(long)]
    x_ui_debug_bar: bool,

    /// Hide superfluous UI chrome
    #[arg(long)]
    x_ui_barebones: bool,

    /// Limit viewport width
    #[arg(
        long,
        value_name = "COLUMNS",
        default_value_t,
        overrides_with = "x_ui_max_width"
    )]
    x_ui_max_width: u16,

    /// Limit viewport height
    #[arg(
        long,
        value_name = "ROWS",
        default_value_t,
        overrides_with = "x_ui_max_height"
    )]
    x_ui_max_height: u16,
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

    if args.keymap {
        sigpipe::reset();
        print_keymap();
        return Ok(ExitCode::SUCCESS);
    }

    init_tracing(&args)?;

    let mut terminal = terminal::init()?;

    let result = run(&args, &mut terminal);

    drop(terminal);

    result
}

fn print_keymap() {
    fn print_mode<V>(name: &str, keymap: &Keymap<V>) {
        println!("{name}");
        for (keys, description) in keymap.docs() {
            if let Some(description) = description {
                println!("    {:10} {description}", format!("{keys}"));
            }
        }
    }
    print_mode("normal", &normal::KEYMAP);
    println!();
    print_mode("insert", &insert::KEYMAP);
    println!();
    print_mode("prompt", &prompt::KEYMAP);
}

fn init_tracing(args: &Args) -> anyhow::Result<()> {
    use tracing_subscriber::{EnvFilter, Registry, fmt, prelude::*};

    let home = home_dir()?;

    let log_path = home.join(".local/state/indigo/log");

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

fn parse_file_line(raw: &Utf8PathBuf) -> (Utf8PathBuf, Option<usize>) {
    let s = raw.as_str();
    if let Some(colon) = s.rfind(':')
        && let Ok(line) = s[colon + 1..].parse::<usize>()
        && line > 0
    {
        let path = Utf8PathBuf::from(&s[..colon]);
        // If the split path exists, or the original doesn't exist as-is, use the split
        if path.exists() || !raw.exists() {
            return (path, Some(line));
        }
    }
    (raw.clone(), None)
}

fn pwd() -> anyhow::Result<Utf8PathBuf> {
    // Try to use the shell's `$PWD` (doesn't resolve symlinks) if possible
    let unix_cwd = Utf8PathBuf::try_from(env::current_dir()?)?;
    let Ok(shell_pwd) = env::var("PWD").map(Utf8PathBuf::from) else {
        // Missing or not UTF-8
        return Ok(unix_cwd);
    };
    if !shell_pwd.is_absolute() {
        // Not absolute
        return Ok(unix_cwd);
    }
    if !fs::canonicalize(&shell_pwd).is_ok_and(|pwd| pwd == unix_cwd) {
        // Stale
        return Ok(unix_cwd);
    }
    Ok(shell_pwd)
}

fn run(args: &Args, terminal: &mut TerminalGuard) -> anyhow::Result<ExitCode> {
    let (file, goto_line) = if let Some(raw) = &args.file {
        let (path, line) = parse_file_line(raw);
        (Some(path), line)
    } else {
        (None, None)
    };

    let pwd = pwd()?;

    let buffer = if let Some(path) = &file {
        Buffer::open(&RealFs, pwd.join(path))?
    } else {
        Buffer::new()
    };

    let mut server = Server::new(home_dir()?, buffer);

    server.fs = Arc::new(RealFs);
    server.client_mut().session_mut().pwd = Some(pwd);

    // Centering scroll deferred until height is known
    let mut pending_center = false;
    if let Some(line_number) = goto_line {
        let mut client = server.client_mut();
        let mut window = client.focused_window_mut();
        let rope = window.buffer().text.rope();
        let line_index = min(line_number - 1, rope.len_lines_unix() - 1);
        let byte_index = rope.line_to_byte_idx(line_index, LINE_TYPE);
        window
            .selection_move()
            .for_each_move(|mut range| range.move_to(byte_index));
        window.selection_move().merge_overlapping();
        pending_center = true;
    }

    if let Some(keys) = &args.experimental.x_exec {
        for key in &keys.0 {
            let event = IndigoEvent::Key(indigo_core::client::KeyEvent {
                key: *key,
                kind: indigo_core::client::KeyEventKind::Press,
            });
            server.handle_event(event)?;
        }
        if let Some(exit_code) = server.client().exit_code {
            return Ok(exit_code);
        }
    }

    let mut areas = Areas::default();

    let exit_code = 'frame: loop {
        if mem::take(&mut server.client_mut().suspend) {
            terminal::suspend(terminal)?;
        }

        terminal.draw(|frame| {
            let area = frame.area();
            let limit = |max, actual| if max == 0 { actual } else { min(max, actual) };
            let area = Rect {
                width: limit(args.experimental.x_ui_max_width, area.width),
                height: limit(args.experimental.x_ui_max_height, area.height),
                ..area
            };
            let surface = frame.buffer_mut();
            areas = Areas::new(&server.client(), area, args.experimental.x_ui_debug_bar);
            server
                .client_mut()
                .focused_window_mut()
                .set_height(areas.text.height);
            if pending_center {
                server
                    .client_mut()
                    .focused_window_mut()
                    .scroll_center_selection();
                pending_center = false;
            }
            server.client_mut().focused_buffer_mut().text.reparse();
            render(&server.client(), &args.experimental, area, surface);
        })?;

        let event = 'event_read: loop {
            let event = crossterm::event::read()?;
            if event.is_resize() {
                continue 'frame;
            }
            if !should_skip_event(&event, areas) {
                break 'event_read event;
            }
        };

        handle_event(&mut server, areas, event)?;

        if let Some(exit_code) = server.client().exit_code {
            break exit_code;
        }
    };

    Ok(exit_code)
}

fn render(client: &Client, experimental: &ExperimentalArgs, area: Rect, surface: &mut Surface) {
    if area.is_empty() {
        return;
    }
    let areas = Areas::new(client, area, experimental.x_ui_debug_bar);
    render_status_bar(client, areas.status_bar, surface);
    render_prompt(client, areas.status_bar, surface);
    render_line_numbers(client, areas.line_numbers, surface);
    if !experimental.x_ui_barebones {
        render_dots(client, areas.dots, surface);
    }
    if experimental.x_ui_debug_bar {
        render_debug_bar(client, areas.debug_bar, surface);
    }
    render_text(client, areas.text, surface);
    render_selection(client, areas.text, surface);
    if !experimental.x_ui_barebones {
        render_scroll_bar(client, areas.scroll_bar, surface);
    }
}

fn render_status_bar(client: &Client, area: Rect, surface: &mut Surface) {
    let mode = match &client.mode {
        Mode::Normal => "normal",
        Mode::Insert => "insert",
        Mode::Prompt(_) => "prompt",
    };

    let path = client
        .focused_buffer()
        .path()
        .map_or(Cow::Borrowed("*scratch*"), |path| client.display_path(path));

    let language = client
        .focused_buffer()
        .text
        .language()
        .map_or("", |language| match language {
            Language::Rust => " rust",
        });

    let window = client.focused_window();
    let selection = window.selection();

    let (line, column) = {
        let range = selection.get_primary();
        let cursor = range.head();
        let line = cursor.line_number();
        let column = cursor.column_number();
        (line, column)
    };

    let count = match client.count {
        Some(count) if count.get() == usize::MAX => Cow::Borrowed(" count=∞"),
        Some(count) => Cow::Owned(format!(" count={count}")),
        None => Cow::Borrowed(""),
    };

    Line::raw(format!("{path}{language} {line}:{column} {mode}{count}"))
        .right_aligned()
        .bg(THEME.status_bar_bg)
        .render(area, surface);
}

fn render_prompt(client: &Client, mut area: Rect, surface: &mut Surface) {
    if let Mode::Prompt(prompt_mode) = &client.mode {
        surface.set_style(area, Style::new().bg(THEME.status_bar_bg));

        format!("{}:", prompt_mode.prompt).render(area, surface);

        let offset = u16::try_from(prompt_mode.prompt.display_width())
            .expect("Prompt width is less than u16::MAX")
            + 1;
        area.x += offset;
        area.width = area.width.saturating_sub(offset);

        Line::raw(format!("{}   ", prompt_mode.content())).render(area, surface);

        if let Some(rect) =
            byte_index_to_area(prompt_mode.cursor().byte_index, prompt_mode.rope(), 0, area)
        {
            surface.set_style(
                rect,
                Style::default()
                    .fg(THEME.primary_cursor_fg)
                    .bg(THEME.primary_cursor_bg),
            );
        }
    } else if let Some(message) = client.message() {
        match message {
            Ok(message) => Line::raw(message)
                .bg(THEME.status_bar_bg)
                .render(area, surface),
            Err(message) => Line::raw(message).bg(THEME.error_bg).render(area, surface),
        }
    }
}

fn render_line_numbers(client: &Client, area: Rect, surface: &mut Surface) {
    let window = client.focused_window();

    let buffer = window.buffer();

    let total_lines = buffer.text.rope().len_lines_unix();

    for (i, row) in area.rows().enumerate() {
        let line_number = i + window.vertical_scroll() + 1;

        if line_number > total_lines {
            break;
        }

        let mut label = ArrayString::<24>::new();
        write!(label, "{line_number} ").expect("usize and a space fit in 24 bytes");

        let width = u16::try_from(label.len()).expect("Label width is less than u16::MAX");
        let x = max(row.x, row.right().saturating_sub(width));

        surface.set_stringn(
            x,
            row.y,
            label,
            usize::from(row.width),
            Style::new().fg(THEME.line_numbers_fg),
        );
    }
}

fn render_scroll_bar(client: &Client, area: Rect, surface: &mut Surface) {
    const LOWER_BLOCKS: [&str; 9] = [" ", "▁", "▂", "▃", "▄", "▅", "▆", "▇", "█"];
    const UNITS_PER_CELL: usize = 8;
    const MIN_UNITS: usize = 8;

    if area.height == 0 {
        return;
    }

    let track_color = THEME.scroll_bar_track;
    let thumb_color = THEME.scroll_bar_thumb;

    let window = client.focused_window();
    let buffer = window.buffer();

    let text_lines = buffer.text.rope().len_lines_unix();
    let window_lines = usize::from(window.height());
    let scroll_lines = text_lines.saturating_add(window_lines.saturating_sub(1));
    let track_cells = usize::from(area.height);
    let track_units = track_cells * UNITS_PER_CELL;

    if track_units == 0 || scroll_lines == 0 {
        return;
    }

    let thumb_units = ((track_units * window_lines) / scroll_lines).clamp(MIN_UNITS, track_units);

    let current_scroll = window.vertical_scroll();
    let max_scroll = text_lines.saturating_sub(1);

    let thumb_start_units = if max_scroll == 0 || track_units == thumb_units {
        0
    } else {
        ((track_units - thumb_units) * current_scroll) / max_scroll
    };
    let thumb_end_units = thumb_start_units + thumb_units;

    for (i, row) in area.rows().enumerate() {
        let cell_top = i * UNITS_PER_CELL;
        let cell_bottom = cell_top + UNITS_PER_CELL;

        let overlap_start = max(cell_top, thumb_start_units);
        let overlap_end = min(cell_bottom, thumb_end_units);
        let overlap_units = overlap_end.saturating_sub(overlap_start);

        if overlap_units == 0 {
            " ".bg(track_color).render(row, surface);
            continue;
        }

        if overlap_units == UNITS_PER_CELL {
            " ".bg(thumb_color).render(row, surface);
            continue;
        }

        if overlap_end == cell_bottom && overlap_start > cell_top {
            let h = overlap_units;
            LOWER_BLOCKS[h]
                .fg(thumb_color)
                .bg(track_color)
                .render(row, surface);
            continue;
        }

        if overlap_start == cell_top && overlap_end < cell_bottom {
            let empty = UNITS_PER_CELL - overlap_units;
            LOWER_BLOCKS[empty]
                .fg(track_color)
                .bg(thumb_color)
                .render(row, surface);
            continue;
        }

        unreachable!();
    }
}

fn render_dots(client: &Client, area: Rect, surface: &mut Surface) {
    let window = client.focused_window();

    let buffer = window.buffer();

    let total_lines = buffer.text.rope().len_lines_unix();

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

            "⢀".fg(THEME.dots).render(cell, surface);
        }
    }
}

fn render_debug_bar(client: &Client, area: Rect, surface: &mut Surface) {
    let window = client.focused_window();
    let selection = window.selection();
    let ranges = selection.ranges.len();
    let primary = selection.get_primary();
    let tail = primary.tail().byte_index;
    let head = primary.head().byte_index;
    let goal = primary.goal_column;

    let pending = if client.pending_keys.is_empty() {
        String::new()
    } else {
        let keys = Keys(&client.pending_keys);
        format!(" pending={keys}")
    };

    let info = format!("{pending} ranges={ranges} tail={tail} head={head} goal={goal:?}");
    let syntax = render_syntax_breadcrumb(
        client,
        usize::from(area.width).saturating_sub(info.as_str().display_width()),
    );

    surface.set_style(area, Style::new().bg(THEME.status_bar_bg));
    Line::raw(syntax).render(area, surface);
    Line::raw(info).right_aligned().render(area, surface);
}

fn render_syntax_breadcrumb(client: &Client, max_width: usize) -> String {
    const SEPARATOR: &str = "›";
    const ELLIPSIS: &str = "… ";

    let window = client.focused_window();

    let Some(syntax) = window.buffer().text.syntax_stale() else {
        return String::new();
    };

    let (start, end) = window.selection().get_primary().byte_offsets();

    let kinds = syntax
        .node_path(start..end)
        .into_iter()
        .map(|node| {
            if node.is_error() || node.is_missing() {
                format!("!{}!", node.kind())
            } else {
                node.kind().to_string()
            }
        })
        .collect::<Vec<_>>();

    if kinds.is_empty() {
        return String::new();
    }

    let mut skipped = 0;
    loop {
        let mut breadcrumb = if skipped > 0 {
            format!("{ELLIPSIS}{SEPARATOR}")
        } else {
            String::new()
        };
        breadcrumb.push_str(&kinds[skipped..].join(SEPARATOR));
        if breadcrumb.as_str().display_width() <= max_width || skipped + 1 >= kinds.len() {
            return breadcrumb;
        }
        skipped += 1;
    }
}

fn render_text(client: &Client, area: Rect, surface: &mut Surface) {
    let window = client.focused_window();

    let buffer = window.buffer();

    let lines = buffer
        .text
        .rope()
        .lines_at(window.vertical_scroll(), LINE_TYPE);

    let rows = area.rows();

    // TODO(horizontal_scroll): Once horizontal scroll is added, this needs to truncate lines.
    'line: for (line, mut rect) in lines.zip(rows) {
        'grapheme: for grapheme in line.graphemes() {
            let span = match grapheme.get_char(0) {
                // Ok(' ') => Span::styled("·", THEME.whitespace_fg),
                Ok('\t') => Span::styled("→       ", THEME.whitespace_fg),
                Ok('\n') => Span::styled("¬", THEME.whitespace_fg),
                _ => Span::styled(grapheme, Style::reset()),
            };

            let width_usize = grapheme.display_width();

            let width_u16 = u16::try_from(width_usize)
                .expect("No grapheme exists with a display width > u16::MAX");

            if width_u16 > rect.width {
                continue 'line;
            }

            if width_usize == 0 {
                continue 'grapheme;
            }

            span.render(rect, surface);

            rect.x += width_u16;
            rect.width -= width_u16;
        }
    }
}

#[expect(clippy::similar_names)]
fn render_selection(client: &Client, area: Rect, surface: &mut Surface) {
    let window = client.focused_window();

    let buffer = window.buffer();

    let rope = buffer.text.rope();

    let vertical_scroll = window.vertical_scroll();

    let selection = window.selection();

    let primary_index = selection.primary_range;

    let visible_lines = vertical_scroll..vertical_scroll + usize::from(area.height);

    let visible_ranges = {
        // Assumes ranges don't nest, so ends are sorted like starts.
        let start = selection.ranges.partition_point(|range| {
            rope.byte_to_line_idx(range.end().byte_index, LINE_TYPE) < visible_lines.start
        });
        let end = selection.ranges.partition_point(|range| {
            rope.byte_to_line_idx(range.start().byte_index, LINE_TYPE) < visible_lines.end
        });
        start..end
    };

    for i in visible_ranges {
        let range = selection.get(i).unwrap();

        let (range_bg, cursor_fg, cursor_bg) = if i == primary_index {
            (
                THEME.primary_range_bg,
                THEME.primary_cursor_fg,
                THEME.primary_cursor_bg,
            )
        } else {
            (
                THEME.secondary_range_bg,
                THEME.secondary_cursor_fg,
                THEME.secondary_cursor_bg,
            )
        };

        let head = range.head().byte_index;
        let start = range.start().byte_index;
        let end = range.end().byte_index;

        let start_line = rope.byte_to_line_idx(start, LINE_TYPE);
        let end_line = rope.byte_to_line_idx(end, LINE_TYPE);

        let grapheme_area =
            |byte_index| byte_index_to_area(byte_index, rope, vertical_scroll, area);

        let line_area = |line_index| line_index_to_area(line_index, rope, vertical_scroll, area);

        let visible_start_line = max(start_line, visible_lines.start);
        let visible_end_line = min(end_line + 1, visible_lines.end);

        for (line_index, mut line_rect) in (visible_start_line..visible_end_line)
            .filter_map(|line_index| line_area(line_index).map(|rect| (line_index, rect)))
        {
            if line_index == start_line {
                if let Some(start_rect) = grapheme_area(start) {
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
                if let Some(end_rect) = grapheme_area(end) {
                    let delta = line_rect.right() - end_rect.right();
                    line_rect.width -= delta;
                }
            }
            surface.set_style(line_rect, Style::default().bg(range_bg));
        }

        if let Some(rect) = grapheme_area(head) {
            surface.set_style(rect, Style::default().fg(cursor_fg).bg(cursor_bg));
        }
    }
}
