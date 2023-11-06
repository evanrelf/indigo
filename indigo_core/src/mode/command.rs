#[derive(Clone, Debug, Default)]
pub struct CommandMode {
    command: Rope,
    cursor: usize,
}
