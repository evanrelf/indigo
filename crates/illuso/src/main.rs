use clap::Parser as _;

#[derive(clap::Parser)]
struct Args {}

fn main() {
    let _args = Args::parse();
}
