use camino::Utf8PathBuf;
use clap::Parser as _;
use etcetera::app_strategy::{AppStrategy as _, Xdg};
use indigo_core::prelude::*;
use indigo_tui::{
    areas::Areas,
    event::{handle_event, should_skip_event},
    terminal,
    terminal::Terminal,
    widgets,
};
use ratatui::{crossterm, widgets::Widget};
use std::{fs, io, process::ExitCode};

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

    let xdg = init_etcetera()?;

    init_tracing_subscriber(&args, &xdg)?;

    let mut terminal = terminal::enter()?;

    let rope = if let Some(ref path) = args.file {
        let file = fs::File::open(path)?;
        Rope::from_reader(io::BufReader::new(file))?
    } else {
        Rope::new()
    };

    let editor = Editor::from_rope(rope);

    run(&args, &mut terminal, editor)
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

    // NOTE: When profiling with Tracy, make sure to set `--log-filter=trace` / `INDIGO_LOG=trace`
    // (or an equivalent) to send as much data as possible.
    let filter_layer = EnvFilter::new(args.log_filter.clone());

    let fmt_layer = fmt::Layer::default().with_writer(log_file);

    #[cfg(feature = "tracy")]
    let tracy_layer = tracing_tracy::TracyLayer::default();
    #[cfg(not(feature = "tracy"))]
    let tracy_layer = tracing_subscriber::layer::Identity::default();

    let subscriber = Registry::default()
        .with(fmt_layer)
        .with(filter_layer)
        .with(tracy_layer);

    subscriber.init();

    Ok(())
}

fn run(_args: &Args, terminal: &mut Terminal, mut editor: Editor) -> anyhow::Result<ExitCode> {
    let mut areas = Areas::default();

    // Using a non-continuous frame to omit time spent blocking on reading the next event. It makes
    // up the vast majority of time in the loop.
    #[cfg(feature = "tracy")]
    let mut tracy_frame = Some(tracing_tracy::client::non_continuous_frame!("frame"));

    loop {
        terminal.draw(|frame| {
            let _span = tracing::trace_span!("terminal draw").entered();
            let area = frame.area();
            let surface = frame.buffer_mut();
            areas = Areas::new(&editor, area);
            editor.height = usize::from(areas.text.height);
            widgets::Editor::new(&editor).render(area, surface);
        })?;

        #[cfg(feature = "tracy")]
        let _ = tracy_frame.take();

        let event = loop {
            let event = crossterm::event::read()?;
            if !should_skip_event(&event) {
                break event;
            }
        };

        #[cfg(feature = "tracy")]
        let _ = tracy_frame.insert(tracing_tracy::client::non_continuous_frame!("frame"));

        handle_event(&mut editor, terminal, areas, &event)?;

        if let Some(exit_code) = editor.exit {
            return Ok(ExitCode::from(exit_code));
        }
    }
}
