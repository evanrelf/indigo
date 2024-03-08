use ropey::Rope;

struct Editor {
    text: Rope,
    cursor: (usize, usize),
    scroll: usize,
}

fn main() {
    println!("Hello, world!");
}
