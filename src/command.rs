use tui::widgets::Widget;

pub struct CommandLine {}

impl CommandLine {
    pub fn new() -> Self {
        Self {}
    }
}

impl Default for CommandLine {
    fn default() -> Self {
        Self::new()
    }
}

impl Widget for &CommandLine {
    fn render(self, area: tui::layout::Rect, buffer: &mut tui::buffer::Buffer) {
        use tui::widgets::Block;

        Block::default()
            .title(":{}".to_string())
            .render(area, buffer);
}
