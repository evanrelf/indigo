use camino::Utf8PathBuf;
use clap::Parser as _;
use etcetera::app_strategy::{AppStrategy as _, Xdg};
use hdrhistogram::Histogram;
use indigo_core::prelude::*;
use indigo_tui::{
    areas::Areas,
    event::{handle_event, should_skip_event},
    terminal,
    terminal::TerminalGuard,
    widgets,
};
use ratatui::widgets::Widget;
use std::time::Instant;
use std::{cmp::max, fs, io, process::ExitCode, sync::Arc};

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
        // TODO: Replace with `std::panic::set_backtrace_style` once it stabilizes.
        // https://github.com/rust-lang/rust/issues/93346
        unsafe {
            std::env::set_var("RUST_BACKTRACE", "1");
        }
    }

    let args = Args::parse();

    let xdg = init_etcetera()?;

    init_tracing_subscriber(&args, &xdg)?;

    let terminal = terminal::init();

    let rope = if let Some(ref path) = args.file {
        let file = fs::File::open(path)?;
        Rope::from_reader(io::BufReader::new(file))?
    } else {
        Rope::new()
    };

    let editor = Editor::from(Buffer::from(rope));

    run(&args, terminal, editor)
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

    let log_path = xdg.in_state_dir("tui.log").unwrap();

    fs::create_dir_all(log_path.parent().unwrap())?;

    let log_file = fs::OpenOptions::new()
        .append(true)
        .create(true)
        .open(log_path)?;

    let filter_layer = EnvFilter::new(args.log_filter.clone());

    let fmt_layer = fmt::Layer::default().with_writer(log_file);

    let subscriber = Registry::default()
        .with(fmt_layer)
        .with(filter_layer);

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
            histogram: Histogram::<u16>::new(3).unwrap(),
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

fn run(args: &Args, mut terminal: TerminalGuard, mut editor: Editor) -> anyhow::Result<ExitCode> {
    let mut areas = Areas::default();

    let mut frame = Times::new("full frame", args.stats);
    let mut render = Times::new("just render", args.stats);
    let mut event_read = Times::new("just event read", args.stats);

    let exit_code = loop {
        render.start();
        terminal.draw(|frame| {
            let _span = tracing::trace_span!("terminal draw").entered();
            let area = frame.area();
            let surface = frame.buffer_mut();
            areas = Areas::new(&editor, area);
            editor.height = usize::from(areas.text.height);
            widgets::Editor::new(&editor).render(area, surface);
        })?;
        render.stop();
        frame.stop();

        let event = loop {
            frame.start();
            event_read.start();
            let event = crossterm::event::read()?;
            if !should_skip_event(&event) {
                event_read.stop();
                break event;
            }
        };

        handle_event(&mut editor, &mut terminal, areas, event)?;

        if let Some(exit_code) = editor.exit {
            break ExitCode::from(exit_code);
        }
    };

    drop(terminal);

    frame.print_stats();
    render.print_stats();
    event_read.print_stats();

    Ok(exit_code)
}
