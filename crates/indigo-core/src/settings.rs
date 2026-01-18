pub struct Settings {
    pub meaning_of_life: usize,
}

impl Settings {
    pub fn set(&mut self, _name: &str, _value: &str) -> anyhow::Result<()> {
        anyhow::bail!("todo")
    }
}

impl Default for Settings {
    fn default() -> Self {
        Self {
            meaning_of_life: 42,
        }
    }
}

#[derive(clap::ValueEnum, Clone)]
pub enum Scope {
    Global,
    Buffer,
    Window,
}
