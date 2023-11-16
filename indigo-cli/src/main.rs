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
    let args = Args::parse();

    tracing_subscriber::fmt()
        .with_max_level(args.verbosity.log_level_filter().as_trace())
        .init();

    println!("file: {:?}", args.file);
    println!("keys: {}", args.keys);
}
