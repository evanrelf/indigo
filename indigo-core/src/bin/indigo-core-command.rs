use clap::Parser as _;
use indigo_core::Command;

fn main() {
    let command = Command::parse();
    println!("{command:?}");
}
