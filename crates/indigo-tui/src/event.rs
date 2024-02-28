pub enum Event {
    Core(indigo_core::Event),
    Terminal(crossterm::event::Event),
}
