// Utterly basic, effectively irreducible

use ropey::Rope;

struct Editor {
    text: Rope,
    cursor: (usize, usize),
    scroll: usize,
}
