use crate::{History, Position, RopeExt as _, Selection};
use ropey::Rope;
use std::cmp::{max, min};

#[derive(Debug)]
pub struct Buffer {
    text: Rope,
    // TODO: Make private
    pub selection: Selection,
    history: History<(Rope, Selection)>,
}

impl Buffer {
    pub fn new(text: Rope) -> anyhow::Result<Self> {
        anyhow::ensure!(!text.len_chars() > 0);

        let selection = Selection::default();

        let mut history = History::default();
        history.push((text.clone(), selection));

        Ok(Self {
            text,
            selection,
            history,
        })
    }

    #[must_use]
    pub fn text(&self) -> &Rope {
        &self.text
    }

    #[must_use]
    pub fn selection(&self) -> &Selection {
        &self.selection
    }

    pub fn extend_to(&mut self, line: usize, column: usize) -> anyhow::Result<()> {
        self.selection.cursor.line = line;
        self.selection.cursor.column = column;
        self.selection.target_column = None;
        self.selection.cursor.correct(&self.text)
    }

    pub fn extend_up(&mut self, distance: usize) -> anyhow::Result<()> {
        self.selection.target_column = match self.selection.target_column {
            None => Some(self.selection.cursor.column),
            Some(target_column) => Some(max(self.selection.cursor.column, target_column)),
        };

        self.selection.cursor.line = self.selection.cursor.line.saturating_sub(distance);
        self.selection.cursor.column = self
            .selection
            .target_column
            .unwrap_or(self.selection.cursor.column);
        self.selection.cursor.correct(&self.text)?;

        if self.selection.target_column.unwrap_or(0) <= self.selection.cursor.column {
            self.selection.target_column = None;
        }

        Ok(())
    }

    pub fn extend_down(&mut self, distance: usize) -> anyhow::Result<()> {
        self.selection.target_column = match self.selection.target_column {
            None => Some(self.selection.cursor.column),
            Some(target_column) => Some(max(self.selection.cursor.column, target_column)),
        };

        let last_line = self.text.len_lines_indigo().saturating_sub(1);

        self.selection.cursor.line = min(last_line, self.selection.cursor.line + distance);
        self.selection.cursor.column = self
            .selection
            .target_column
            .unwrap_or(self.selection.cursor.column);
        self.selection.cursor.correct(&self.text)?;

        if self.selection.target_column.unwrap_or(0) <= self.selection.cursor.column {
            self.selection.target_column = None;
        }

        Ok(())
    }

    pub fn extend_left(&mut self, distance: usize) -> anyhow::Result<()> {
        self.selection.cursor = self
            .selection
            .cursor
            .via_char_index(&self.text, |index| index.saturating_sub(distance))?;
        self.selection.target_column = None;
        Ok(())
    }

    pub fn extend_right(&mut self, distance: usize) -> anyhow::Result<()> {
        self.selection.cursor = self
            .selection
            .cursor
            .via_char_index(&self.text, |index| index + distance)?;
        self.selection.target_column = None;
        Ok(())
    }

    pub fn move_to(&mut self, line: usize, column: usize) -> anyhow::Result<()> {
        self.extend_to(line, column)?;
        self.selection.reduce();
        Ok(())
    }

    pub fn move_up(&mut self, distance: usize) -> anyhow::Result<()> {
        self.extend_up(distance)?;
        self.selection.reduce();
        Ok(())
    }

    pub fn move_down(&mut self, distance: usize) -> anyhow::Result<()> {
        self.extend_down(distance)?;
        self.selection.reduce();
        Ok(())
    }

    pub fn move_left(&mut self, distance: usize) -> anyhow::Result<()> {
        self.extend_left(distance)?;
        self.selection.reduce();
        Ok(())
    }

    pub fn move_right(&mut self, distance: usize) -> anyhow::Result<()> {
        self.extend_right(distance)?;
        self.selection.reduce();
        Ok(())
    }

    pub fn insert_char(&mut self, char: char) -> anyhow::Result<()> {
        let mut text = self.text.clone();

        let anchor_index = self.selection.anchor.to_char_index(&text)?;
        let cursor_index = self.selection.cursor.to_char_index(&text)?;

        let move_anchor = self.selection.is_backward() || self.selection.is_reduced();

        text.insert_char(cursor_index, char);

        self.selection.cursor = Position::from_char_index(cursor_index + 1, &text)?;

        if move_anchor {
            self.selection.anchor = Position::from_char_index(anchor_index + 1, &text)?;
        }

        self.text = text;

        Ok(())
    }

    pub fn insert(&mut self, str: &str) -> anyhow::Result<()> {
        let mut text = self.text.clone();

        let anchor_index = self.selection.anchor.to_char_index(&text)?;
        let cursor_index = self.selection.cursor.to_char_index(&text)?;

        let move_anchor = self.selection.is_backward() || self.selection.is_reduced();

        text.insert(cursor_index, str);

        self.selection.cursor = Position::from_char_index(cursor_index + str.len(), &text)?;

        if move_anchor {
            self.selection.anchor = Position::from_char_index(anchor_index + str.len(), &text)?;
        }

        self.text = text;

        Ok(())
    }

    pub fn backspace(&mut self) -> anyhow::Result<()> {
        let mut text = self.text.clone();

        let anchor_index = self.selection.anchor.to_char_index(&text)?;
        let cursor_index = self.selection.cursor.to_char_index(&text)?;

        let move_anchor = self.selection.is_backward() || self.selection.is_reduced();

        if cursor_index == 0 {
            return Ok(());
        }

        text.remove(cursor_index - 1..cursor_index);

        self.selection.cursor = Position::from_char_index(cursor_index - 1, &text)?;

        if move_anchor {
            self.selection.anchor = Position::from_char_index(anchor_index - 1, &text)?;
        }

        self.text = text;

        Ok(())
    }

    pub fn record(&mut self) {
        self.history.push((self.text.clone(), self.selection));
    }

    pub fn undo(&mut self) {
        while let Some((text, selection)) = self.history.undo() {
            if self.text != *text {
                self.text = text.clone();
                self.selection = *selection;
                return;
            }
        }
    }

    pub fn redo(&mut self) {
        while let Some((text, selection)) = self.history.redo() {
            if self.text != *text {
                self.text = text.clone();
                self.selection = *selection;
                return;
            }
        }
    }
}

impl Default for Buffer {
    fn default() -> Self {
        Self {
            text: Rope::from("\n"),
            selection: Selection::default(),
            history: History::default(),
        }
    }
}
