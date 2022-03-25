use tui::widgets::Widget;

pub struct CommandLine {
    command: String,
}

impl CommandLine {
    pub fn new() -> Self {
        Self {
            command: String::new(),
        }
    }
}

impl Default for CommandLine {
    fn default() -> Self {
        Self::new()
    }
}

impl Widget for &CommandLine {
    fn render(self, area: tui::layout::Rect, buffer: &mut tui::buffer::Buffer) {
        use tui::style::Color;
        use tui::widgets::Block;

        Block::default()
            .title(format!(":{}", self.command))
            .render(area, buffer);

        buffer
            .get_mut(area.left() + self.command.len() as u16 + 1, area.top())
            .set_bg(Color::White);
    }
}
