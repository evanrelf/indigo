use clap::Parser as _;
use indigo_core::key2::Keys;
use std::{io, process::ExitCode};

// $ watchexec --quiet --clear -- cat keys \| cargo run --quiet --bin indigo-parse-keys

#[derive(clap::Parser)]
struct Args {
    #[arg(long)]
    debug: bool,
}

fn main() -> anyhow::Result<ExitCode> {
    let args = Args::parse();

    let input = io::read_to_string(io::stdin())?;

    let result = input.parse::<Keys>();

    match &result {
        Ok(keys) if args.debug => println!("{keys:?}"),
        Ok(keys) => println!("{keys}"),
        Err(error) => println!("{error}"),
    }
    match &result {
        Ok(_) => Ok(ExitCode::SUCCESS),
        Err(_) => Ok(ExitCode::FAILURE),
    }
}
