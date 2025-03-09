use clap::Parser as _;
use indigo_core::{
    event::{Event, handle_event},
    prelude::*,
};
use std::panic;

#[derive(clap::Parser)]
struct Args {
    #[clap(long)]
    seed: Option<u64>,
}

fn main() {
    let args = Args::parse();
    let seed = args.seed.unwrap_or_else(|| fastrand::u64(..));
    let mut raw_data = [0u8; 1024];
    fastrand::Rng::with_seed(seed).fill(&mut raw_data);
    let Err(error) = panic::catch_unwind(|| {
        let mut unstructured = arbitrary::Unstructured::new(&raw_data);
        let rope = Rope::from_str(unstructured.arbitrary::<&str>().unwrap());
        let mut editor = Editor::from_rope(rope);
        loop {
            let event = unstructured.arbitrary::<Event>().unwrap();
            println!("{event:?}");
            handle_event(&mut editor, &event);
        }
    });
    println!("seed: {seed}");
    panic::resume_unwind(error);
}
