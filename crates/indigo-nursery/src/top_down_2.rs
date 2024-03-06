// MVP, cutting scope aggressively

use ropey::Rope;

struct Editor {
    text: Rope,
    range: Range,
    mode: Mode,
    scroll: usize,
}

struct Range {
    anchor: Position,
    head: Position,
    target_column: Option<usize>,
}

struct Position {
    line: usize,
    column: usize,
}

enum Mode {
    Normal,
    Insert,
}
