use ropey::Rope;

#[derive(Debug, Default)]
pub struct Editor {
    pub text: Rope,
    pub cursor: (usize, usize),
    pub scroll: usize,
}

impl Editor {
    pub fn scroll_up(&mut self, distance: usize) {
        self.scroll = self.scroll.saturating_sub(distance);
    }

    pub fn scroll_down(&mut self, distance: usize) {
        self.scroll += distance;
    }
}
