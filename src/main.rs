use camino::Utf8PathBuf;
use clap::Parser as _;
use ropey::Rope;
use std::{fs::File, io::BufReader};

#[derive(Debug, clap::Parser)]
struct Args {
    file: Utf8PathBuf,
}

fn main() -> anyhow::Result<()> {
    let args = Args::parse();

    let file = File::open(args.file)?;

    let rope = Rope::from_reader(BufReader::new(file))?;

    println!("Lines: {}", rope.len_lines());

    Ok(())
}
