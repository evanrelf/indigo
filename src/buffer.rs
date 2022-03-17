use crate::{cursor::Cursor, selection::Selection};
use ropey::{Rope, RopeSlice};
use std::{fs::File, io::BufReader, path::Path, sync::Mutex};

pub(crate) enum Mode {
    Normal,
    Insert,
}

pub(crate) struct Buffer {
    pub(crate) rope: Rope,

    pub(crate) selections: Vec<Mutex<Selection>>,
    pub(crate) primary_selection: usize,

    pub(crate) mode: Mode,

    pub(crate) viewport_lines_offset: usize,
    pub(crate) viewport_columns_offset: usize,
}

impl Buffer {
    pub(crate) fn new() -> Buffer {
        let rope = Rope::new();

        let selections = vec![Mutex::new(Selection::default())];

        Buffer {
            rope,

            selections,
            primary_selection: 0,

            mode: Mode::Normal,

            viewport_lines_offset: 0,
            viewport_columns_offset: 0,
        }
    }

    pub(crate) fn from_file<P>(path: P) -> Buffer
    where
        P: AsRef<Path>,
    {
        let file = File::open(path).unwrap();
        let reader = BufReader::new(file);

        let rope = Rope::from_reader(reader).unwrap();

        let selections = vec![Mutex::new(Selection::default())];

        Buffer {
            rope,

            selections,
            primary_selection: 0,

            mode: Mode::Normal,

            viewport_lines_offset: 0,
            viewport_columns_offset: 0,
        }
    }

    fn from_str(s: &str) -> Buffer {
        let rope = Rope::from_str(s);

        let selections = vec![Mutex::new(Selection::default())];

        Buffer {
            rope,

            selections,
            primary_selection: 0,

            mode: Mode::Normal,

            viewport_lines_offset: 0,
            viewport_columns_offset: 0,
        }
    }

    pub(crate) fn cursor_to_index(&self, cursor: Cursor) -> Option<usize> {
        let line_index = self.rope.try_line_to_char(cursor.line).ok()?;
        let line_length = self.rope.get_line(cursor.line)?.len_chars();
        if line_length > cursor.column {
            Some(line_index + cursor.column)
        } else {
            None
        }
    }

    pub(crate) fn index_to_cursor(&self, index: usize) -> Option<Cursor> {
        let line = self.rope.try_char_to_line(index).ok()?;
        let column = index - self.rope.try_line_to_char(line).ok()?;
        Some(Cursor {
            line,
            column,
            target_column: None,
        })
    }

    pub(crate) fn selection_to_slice(&self, selection: &Selection) -> Option<RopeSlice> {
        let anchor_index = self.cursor_to_index(selection.anchor)?;
        let cursor_index = self.cursor_to_index(selection.cursor)?;
        if selection.is_forwards() {
            self.rope.get_slice(anchor_index..=cursor_index)
        } else {
            self.rope.get_slice(cursor_index..=anchor_index)
        }
    }

    pub(crate) fn corrected_cursor(&self, cursor: &Cursor) -> Option<Cursor> {
        let line_length = self.rope.get_line(cursor.line)?.len_chars();

        if line_length == 0 {
            return None;
        }

        match cursor.target_column {
            Some(target_column) if line_length > target_column => Some(Cursor {
                column: target_column,
                target_column: None,
                ..*cursor
            }),
            Some(_) => Some(Cursor {
                column: line_length - 1,
                ..*cursor
            }),
            None if line_length > cursor.column => Some(Cursor { ..*cursor }),
            None => Some(Cursor {
                column: line_length - 1,
                target_column: Some(cursor.column),
                ..*cursor
            }),
        }
    }

    pub(crate) fn corrected_selection(&self, selection: &Selection) -> Option<Selection> {
        let anchor = self.corrected_cursor(&selection.anchor)?;
        let cursor = self.corrected_cursor(&selection.cursor)?;
        Some(Selection { anchor, cursor })
    }

    pub(crate) fn scroll_up(&mut self, distance: usize) -> &mut Buffer {
        self.viewport_lines_offset = self.viewport_lines_offset.saturating_sub(distance);
        self
    }

    pub(crate) fn scroll_down(&mut self, distance: usize) -> &mut Buffer {
        let new_viewport_lines_offset = self.viewport_lines_offset + distance;
        if new_viewport_lines_offset <= self.rope.len_lines() {
            self.viewport_lines_offset = new_viewport_lines_offset;
        }
        self
    }

    pub(crate) fn scroll_left(&mut self, distance: usize) -> &mut Buffer {
        self.viewport_columns_offset = self.viewport_columns_offset.saturating_sub(distance);
        self
    }

    pub(crate) fn scroll_right(&mut self, distance: usize) -> &mut Buffer {
        self.viewport_columns_offset += distance;
        self
    }

    pub(crate) fn move_up(&mut self, distance: usize) -> &mut Buffer {
        for selection_mutex in &self.selections {
            let mut selection = selection_mutex.lock().unwrap();
            todo!();
        }
        self
    }

    pub(crate) fn move_down(&mut self, distance: usize) -> &mut Buffer {
        for selection_mutex in &self.selections {
            let mut selection = selection_mutex.lock().unwrap();
            todo!();
        }
        self
    }

    pub(crate) fn move_left(&mut self, distance: usize) -> &mut Buffer {
        for selection_mutex in &self.selections {
            let mut selection = selection_mutex.lock().unwrap();
            let old_index = self.cursor_to_index(selection.cursor).unwrap();
            let new_index = old_index.saturating_sub(distance);
            selection.cursor = self.index_to_cursor(new_index).unwrap();
        }
        self
    }

    pub(crate) fn move_right(&mut self, distance: usize) -> &mut Buffer {
        for selection_mutex in &self.selections {
            let mut selection = selection_mutex.lock().unwrap();
            let old_index = self.cursor_to_index(selection.cursor).unwrap();
            let new_index = old_index + distance;
            if new_index < self.rope.len_chars() {
                selection.cursor = self.index_to_cursor(new_index).unwrap();
            }
        }
        self
    }

    pub(crate) fn reduce(&mut self) -> &mut Buffer {
        for selection_mutex in &self.selections {
            let mut selection = selection_mutex.lock().unwrap();
            selection.reduce();
        }
        self
    }

    pub(crate) fn backspace(&mut self) -> &mut Buffer {
        for selection_mutex in &self.selections {
            let mut selection = selection_mutex.lock().unwrap();
            let anchor_index = self.cursor_to_index(selection.cursor).unwrap();
            let cursor_index = self.cursor_to_index(selection.cursor).unwrap();
            if cursor_index > 0 {
                // Delete character
                self.rope.remove(cursor_index - 1..cursor_index);

                // Shift selection
                selection.anchor = self.index_to_cursor(anchor_index - 1).unwrap();
                selection.cursor = self.index_to_cursor(cursor_index - 1).unwrap();
            }
        }

        self
    }

    pub(crate) fn delete(&mut self) -> &mut Buffer {
        for selection_mutex in &self.selections {
            let mut selection = selection_mutex.lock().unwrap();
            selection.flip_backwards();
            let anchor_index = self.cursor_to_index(selection.anchor).unwrap();
            let cursor_index = self.cursor_to_index(selection.cursor).unwrap();
            self.rope.remove(cursor_index..=anchor_index);
            selection.reduce();
            if let Some(s) = self.corrected_selection(&selection) {
                *selection = s;
            };
        }
        self
    }

    pub(crate) fn insert(&mut self, c: char) -> &mut Buffer {
        for selection_mutex in &self.selections {
            // Insert character
            let mut selection = selection_mutex.lock().unwrap();
            let anchor_index = self.cursor_to_index(selection.anchor).unwrap();
            let cursor_index = self.cursor_to_index(selection.cursor).unwrap();

            if selection.is_forwards() {
                self.rope.insert_char(anchor_index, c);
            } else {
                self.rope.insert_char(cursor_index, c);
            }

            // Shift selection
            selection.anchor = self.index_to_cursor(anchor_index + 1).unwrap();
            selection.cursor = self.index_to_cursor(cursor_index + 1).unwrap();
        }
        self
    }
}

#[test]
fn test_index_cursor() {
    fn case(s: &str, tuple: (usize, usize), expected: char) {
        let buffer = Buffer::from_str(s);
        let cursor = Cursor::from(tuple);
        let index = buffer.cursor_to_index(cursor).unwrap();
        let actual = buffer.rope.char(index);
        assert!(
            expected == actual,
            "\nexpected = {:?}\nactual = {:?}\n",
            expected,
            actual
        );
    }

    case("abc\nxyz\n", (0, 0), 'a');
    case("abc\nxyz\n", (0, 3), '\n');
    case("abc\nxyz\n", (1, 0), 'x');
    case("abc\nxyz\n", (1, 1), 'y');
}

#[test]
fn test_index_cursor_roundtrip() {
    enum CaseResult {
        Pass,
        Fail,
    }
    use CaseResult::*;

    fn case(result: CaseResult, s: &str, tuple: (usize, usize)) {
        let buffer = Buffer::from_str(s);
        let cursor = Cursor::from(tuple);
        let expected = Some(cursor);
        let actual = buffer
            .cursor_to_index(cursor)
            .and_then(|i| buffer.index_to_cursor(i));
        assert!(
            match result {
                Pass => expected == actual,
                Fail => None == actual,
            },
            "\nexpected = {:?}\nactual = {:?}\n",
            expected,
            actual
        );
    }

    case(Pass, "abc\nxyz\n", (0, 0));
    case(Pass, "abc\nxyz\n", (1, 0));
    case(Pass, "abc\nxyz\n", (1, 1));
    case(Fail, "abc\nxyz\n", (0, 4));
    case(Fail, "abc\nxyz\n", (0, 20));
    case(Fail, "abc\nxyz\n", (2, 2));
}

#[test]
fn test_corrected_cursor() {
    fn case(s: &str, original: (usize, usize), corrected: (usize, usize)) {
        let buffer = Buffer::from_str(s);
        let input = Cursor::from(original);
        let expected = Some(Cursor::from(corrected));
        let actual = buffer.corrected_cursor(&input);
        assert!(
            expected == actual,
            "\nexpected = {:?}\nactual = {:?}\n",
            expected,
            actual
        );
    }

    case("abc\nx\n", (0, 0), (0, 0));
    case("abc\nx\n", (0, 99), (0, 3));
    case("abc\nx\n", (1, 1), (1, 1));
    case("abc\nx\n", (1, 99), (1, 1));
}

#[test]
fn test_selection_to_slice() {
    fn case(s: &str, selection: ((usize, usize), (usize, usize)), expected: &str) {
        let buffer = Buffer::from_str(s);
        let selection = Selection::new(selection.0, selection.1);
        let expected = Some(expected);
        let actual = buffer
            .selection_to_slice(&selection)
            .and_then(|slice| slice.as_str());
        assert!(
            expected == actual,
            "\nexpected = {:?}\nactual = {:?}\n",
            expected,
            actual
        );
    }

    case("Hello, world!", ((0, 0), (0, 4)), "Hello");
    case("Hello, world!", ((0, 7), (0, 11)), "world");
    case("Fizz\nBuzz", ((1, 0), (1, 3)), "Buzz");
    case("Fizz\nBuzz", ((0, 0), (1, 3)), "Fizz\nBuzz");
}
