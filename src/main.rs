use camino::Utf8PathBuf;
use clap::Parser as _;
use ropey::Rope;

#[derive(clap::Parser, Debug)]
struct Args {
    file: Option<Utf8PathBuf>,
}

fn main() {
    let args = Args::parse();

    println!("{args:?}");
}

#[derive(Default)]
struct Editor {
    text: Rope,
    cursor: (usize, usize),
    scroll: usize,
}
