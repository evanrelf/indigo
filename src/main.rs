use camino::Utf8PathBuf;
use clap::Parser as _;
use ropey::Rope;
use std::{fs::File, io::BufReader};

#[derive(Debug, clap::Parser)]
struct Args {
    file: Utf8PathBuf,
}

struct Editor {
    text: Rope,
    // Byte offset
    cursor: usize,
}

impl Editor {
    fn new(path: Utf8PathBuf) -> anyhow::Result<Self> {
        let file = File::open(path)?;
        let rope = Rope::from_reader(BufReader::new(file))?;
        Ok(Self {
            text: rope,
            cursor: 0,
        })
    }
}

fn main() -> anyhow::Result<()> {
    let args = Args::parse();

    let editor = Editor::new(args.file)?;

    println!("Lines: {}", editor.text.len_lines());

    Ok(())
}
