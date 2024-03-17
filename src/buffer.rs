use crate::{Direction, History, Position, RopeExt as _, Selection};
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

    fn extend_horizontally(&mut self, direction: Direction, distance: usize) -> anyhow::Result<()> {
        let mut index = self.selection.cursor.to_char_index(&self.text)?;

        index = match direction {
            Direction::Backward => index.saturating_sub(distance),
            Direction::Forward => index + distance,
        };

        self.selection.cursor = Position::from_char_index(index, &self.text)?;

        self.selection.target_column = None;

        Ok(())
    }

    fn extend_horizontally_while(
        &mut self,
        direction: Direction,
        mut predicate: impl FnMut(char, Option<char>) -> bool,
    ) -> anyhow::Result<()> {
        let mut index = self.selection.cursor.to_char_index(&self.text)?;

        let mut chars = match direction {
            Direction::Backward => {
                let mut iter = self.text.chars_at(index).reversed();
                let _ = iter.prev();
                iter.peekable()
            }
            Direction::Forward => self.text.chars_at(index).peekable(),
        };

        while let Some(this) = chars.next() {
            let peek = chars.peek().copied();

            if !predicate(this, peek) {
                break;
            }

            index = match direction {
                Direction::Backward => index.saturating_sub(1),
                Direction::Forward => index + 1,
            }
        }

        self.selection.cursor = Position::from_char_index(index, &self.text)?;

        self.selection.target_column = None;

        Ok(())
    }

    fn extend_vertically(&mut self, direction: Direction, distance: usize) -> anyhow::Result<()> {
        self.selection.target_column = match self.selection.target_column {
            None => Some(self.selection.cursor.column),
            Some(target_column) => Some(max(self.selection.cursor.column, target_column)),
        };

        self.selection.cursor.line = match direction {
            Direction::Backward => self.selection.cursor.line.saturating_sub(distance),
            Direction::Forward => {
                let last_line = self.text.len_lines_indigo().saturating_sub(1);
                min(last_line, self.selection.cursor.line + distance)
            }
        };
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

    pub fn extend_up(&mut self, distance: usize) -> anyhow::Result<()> {
        self.extend_vertically(Direction::Backward, distance)
    }

    pub fn extend_down(&mut self, distance: usize) -> anyhow::Result<()> {
        self.extend_vertically(Direction::Forward, distance)
    }

    pub fn extend_left(&mut self, distance: usize) -> anyhow::Result<()> {
        self.extend_horizontally(Direction::Backward, distance)
    }

    pub fn extend_right(&mut self, distance: usize) -> anyhow::Result<()> {
        self.extend_horizontally(Direction::Forward, distance)
    }

    pub fn extend_line_start(&mut self) -> anyhow::Result<()> {
        self.extend_horizontally_while(Direction::Backward, |_, peek| peek != Some('\n'))
    }

    pub fn extend_line_non_blank_start(&mut self) -> anyhow::Result<()> {
        self.extend_line_start()?;
        self.extend_horizontally_while(Direction::Forward, |this, _| HSPACES.contains(&this))?;
        Ok(())
    }

    pub fn extend_line_end(&mut self) -> anyhow::Result<()> {
        self.extend_horizontally_while(Direction::Forward, |this, peek| {
            this != '\n' && peek != Some('\n')
        })
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

    pub fn move_line_start(&mut self) -> anyhow::Result<()> {
        self.extend_line_start()?;
        self.selection.reduce();
        Ok(())
    }

    pub fn move_line_non_blank_start(&mut self) -> anyhow::Result<()> {
        self.extend_line_non_blank_start()?;
        self.selection.reduce();
        Ok(())
    }

    pub fn move_line_end(&mut self) -> anyhow::Result<()> {
        self.extend_line_end()?;
        self.selection.reduce();
        Ok(())
    }

    pub fn insert_char(&mut self, char: char) -> anyhow::Result<()> {
        let mut text = self.text.clone();

        let anchor_index = self.selection.anchor.to_char_index(&text)?;
        let cursor_index = self.selection.cursor.to_char_index(&text)?;

        let move_anchor = self.selection.is_backward() || self.selection.is_reduced();

        text.insert_char(cursor_index, char);

        if move_anchor {
            self.selection.anchor = Position::from_char_index(anchor_index + 1, &text)?;
        }

        self.selection.cursor = Position::from_char_index(cursor_index + 1, &text)?;

        self.text = text;

        Ok(())
    }

    pub fn insert(&mut self, str: &str) -> anyhow::Result<()> {
        let mut text = self.text.clone();

        let anchor_index = self.selection.anchor.to_char_index(&text)?;
        let cursor_index = self.selection.cursor.to_char_index(&text)?;

        let move_anchor = self.selection.is_backward() || self.selection.is_reduced();

        text.insert(cursor_index, str);

        if move_anchor {
            self.selection.anchor = Position::from_char_index(anchor_index + str.len(), &text)?;
        }

        self.selection.cursor = Position::from_char_index(cursor_index + str.len(), &text)?;

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

        if move_anchor {
            self.selection.anchor = Position::from_char_index(anchor_index - 1, &text)?;
        }

        self.selection.cursor = Position::from_char_index(cursor_index - 1, &text)?;

        self.text = text;

        Ok(())
    }

    pub fn delete(&mut self) -> anyhow::Result<()> {
        self.record();

        let mut text = self.text.clone();

        let start_index = self.selection.start().to_char_index(&text)?;
        let end_index = self.selection.end().to_char_index(&text)?;

        text.remove(start_index..=end_index);

        self.selection.anchor = Position::from_char_index(start_index, &text)?;
        self.selection.cursor = Position::from_char_index(start_index, &text)?;

        self.text = text;

        self.record();

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

const SPACES: [char; 3] = [' ', '\t', '\n'];

const HSPACES: [char; 2] = [' ', '\t'];
