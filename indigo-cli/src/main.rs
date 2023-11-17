use camino::Utf8PathBuf;
use clap::Parser as _;
use clap_verbosity_flag::Verbosity;
use indigo_core::Keys;
use tracing_log::AsTrace as _;

#[derive(Debug, clap::Parser)]
struct Args {
    file: Option<Utf8PathBuf>,

    #[arg(long, short)]
    keys: Keys,

    #[command(flatten)]
    verbosity: Verbosity,
}

fn main() {
    #[cfg(debug_assertions)]
    if std::env::var("RUST_BACKTRACE").is_err() {
        std::env::set_var("RUST_BACKTRACE", "1");
    }

    let args = Args::parse();

    tracing_subscriber::fmt()
        .with_max_level(args.verbosity.log_level_filter().as_trace())
        .init();

    // TODO: Once `indigo-cli` has an `Editor`, check that it's valid in debug mode.
    // #[cfg(debug_assertions)]
    // editor.assert_valid();

    println!("file: {:?}", args.file);
    println!("keys: {}", args.keys);
}
