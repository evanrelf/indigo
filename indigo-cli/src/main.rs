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
use indigo_core::Keys;

#[derive(Debug, clap::Parser)]
struct Args {
    file: Option<Utf8PathBuf>,

    #[arg(long, short)]
    keys: Keys,
}

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    tracing_subscriber::fmt::init();

    let args = Args::parse();

    println!("{args:?}");

    Ok(())
}
