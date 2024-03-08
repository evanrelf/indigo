use camino::Utf8PathBuf;
use clap::Parser as _;
use ropey::Rope;
use std::{fs::File, io::BufReader};

#[derive(clap::Parser, Debug)]
struct Args {
    file: Option<Utf8PathBuf>,
}

fn main() -> anyhow::Result<()> {
    let args = Args::parse();

    let mut editor = Editor::default();

    if let Some(ref path) = args.file {
        let file = File::open(path)?;
        editor.text = Rope::from_reader(BufReader::new(file))?;
    }

    Ok(())
}

#[derive(Debug, Default)]
struct Editor {
    text: Rope,
    cursor: (usize, usize),
    scroll: usize,
}
