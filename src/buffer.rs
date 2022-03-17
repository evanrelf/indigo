use crate::{cursor::Cursor, selection::Selection};
use ropey::Rope;
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
            // TODO: Doesn't remember target column
            if let Some(new_cursor) = selection.cursor.move_up(distance).corrected(&self.rope) {
                selection.cursor = new_cursor;
            }
        }
        self
    }

    pub(crate) fn move_down(&mut self, distance: usize) -> &mut Buffer {
        for selection_mutex in &self.selections {
            let mut selection = selection_mutex.lock().unwrap();
            // TODO: Doesn't remember target column
            if let Some(new_cursor) = selection.cursor.move_down(distance).corrected(&self.rope) {
                selection.cursor = new_cursor;
            }
        }
        self
    }

    pub(crate) fn move_left(&mut self, distance: usize) -> &mut Buffer {
        for selection_mutex in &self.selections {
            let mut selection = selection_mutex.lock().unwrap();
            let old_index = selection.cursor.to_index(&self.rope).unwrap();
            let new_index = old_index.saturating_sub(distance);
            selection.cursor = Cursor::from_index(&self.rope, new_index).unwrap();
        }
        self
    }

    pub(crate) fn move_right(&mut self, distance: usize) -> &mut Buffer {
        for selection_mutex in &self.selections {
            let mut selection = selection_mutex.lock().unwrap();
            let old_index = selection.cursor.to_index(&self.rope).unwrap();
            let new_index = old_index + distance;
            if new_index < self.rope.len_chars() {
                selection.cursor = Cursor::from_index(&self.rope, new_index).unwrap();
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
            let anchor_index = selection.cursor.to_index(&self.rope).unwrap();
            let cursor_index = selection.cursor.to_index(&self.rope).unwrap();
            if cursor_index > 0 {
                // Delete character
                self.rope.remove(cursor_index - 1..cursor_index);

                // Shift selection
                selection.anchor = Cursor::from_index(&self.rope, anchor_index - 1).unwrap();
                selection.cursor = Cursor::from_index(&self.rope, cursor_index - 1).unwrap();
            }
        }

        self
    }

    pub(crate) fn delete(&mut self) -> &mut Buffer {
        for selection_mutex in &self.selections {
            let mut selection = selection_mutex.lock().unwrap();
            selection.flip_backwards();
            let anchor_index = selection.anchor.to_index(&self.rope).unwrap();
            let cursor_index = selection.cursor.to_index(&self.rope).unwrap();
            self.rope.remove(cursor_index..=anchor_index);
            selection.reduce();
            if let Some(s) = selection.corrected(&self.rope) {
                *selection = s;
            };
        }
        self
    }

    pub(crate) fn insert(&mut self, c: char) -> &mut Buffer {
        for selection_mutex in &self.selections {
            // Insert character
            let mut selection = selection_mutex.lock().unwrap();
            let anchor_index = selection.anchor.to_index(&self.rope).unwrap();
            let cursor_index = selection.cursor.to_index(&self.rope).unwrap();

            if selection.is_forwards() {
                self.rope.insert_char(anchor_index, c);
            } else {
                self.rope.insert_char(cursor_index, c);
            }

            // Shift selection
            selection.anchor = Cursor::from_index(&self.rope, anchor_index + 1).unwrap();
            selection.cursor = Cursor::from_index(&self.rope, cursor_index + 1).unwrap();
        }
        self
    }
}

#[test]
fn test_index_cursor() {
    fn case(s: &str, tuple: (usize, usize), expected: char) {
        let rope = Rope::from_str(s);
        let cursor = Cursor::from(tuple);
        let index = cursor.to_index(&rope).unwrap();
        let actual = rope.char(index);
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
        let rope = Rope::from_str(s);
        let cursor = Cursor::from(tuple);
        let expected = Some(cursor);
        let actual = cursor
            .to_index(&rope)
            .and_then(|i| Cursor::from_index(&rope, i));
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
        let rope = Rope::from_str(s);
        let input = Cursor::from(original);
        let expected = Some(Cursor::from(corrected));
        let actual = input.corrected(&rope);
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
