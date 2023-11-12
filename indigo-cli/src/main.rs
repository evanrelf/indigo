#![warn(clippy::pedantic, clippy::use_self)]
#![allow(
    clippy::bool_to_int_with_if,
    clippy::missing_errors_doc,
    clippy::missing_panics_doc,
    clippy::module_name_repetitions
)]
// TODO: Remove
#![allow(dead_code)]

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
