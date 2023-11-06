use ropey::Rope;
use std::{borrow::Cow, cmp::min};

#[derive(Clone, Debug, Default)]
pub struct CommandMode {
    command: Rope,
    cursor: usize,
}

impl CommandMode {
    #[must_use]
    pub fn command(&self) -> &Rope {
        &self.command
    }

    #[must_use]
    pub fn cursor(&self) -> usize {
        self.cursor
    }

    #[must_use]
    pub fn move_left(&self, distance: usize) -> Self {
        Self {
            cursor: self.cursor.saturating_sub(distance),
            ..self.clone()
        }
    }

    #[must_use]
    pub fn move_right(&self, distance: usize) -> Self {
        Self {
            cursor: min(self.command.len_chars(), self.cursor + distance),
            ..self.clone()
        }
    }

    #[must_use]
    pub fn move_line_begin(&self) -> Self {
        Self {
            cursor: 0,
            ..self.clone()
        }
    }

    #[must_use]
    pub fn move_line_end(&self) -> Self {
        Self {
            cursor: self.command.len_chars(),
            ..self.clone()
        }
    }

    #[must_use]
    pub fn insert_char(&self, c: char) -> Self {
        let mut command = self.command.clone();
        command.insert_char(self.cursor, c);
        Self {
            command,
            cursor: self.cursor + 1,
        }
    }

    // TODO: In theory `Cow` is pointless here because `Rope`s are copy-on-write anyways, but it
    // might start to matter if `CommandMode` gets more fields that are expensive to clone?

    #[must_use]
    pub fn backspace(&self) -> Cow<Self> {
        if self.cursor > 0 {
            let mut command = self.command.clone();
            let cursor = self.cursor - 1;
            command.remove(cursor..=cursor);
            Cow::Owned(Self { command, cursor })
        } else {
            Cow::Borrowed(self)
        }
    }

    #[must_use]
    pub fn clear(&self) -> Self {
        Self {
            command: Rope::new(),
            cursor: 0,
        }
    }
}
