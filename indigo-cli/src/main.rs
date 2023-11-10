#![warn(clippy::pedantic, clippy::use_self)]
#![allow(
    clippy::bool_to_int_with_if,
    clippy::missing_errors_doc,
    clippy::missing_panics_doc,
    clippy::module_name_repetitions
)]
// TODO: Remove
#![allow(dead_code)]

use clap::Parser as _;

#[derive(Debug, clap::Parser)]
struct Args {}

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    let args = Args::parse();
    println!("{args:#?}");
    Ok(())
}
