use crate::{
    cursor::Cursor,
    selection::{self, Selection},
};
use ropey::{Rope, RopeSlice};
use std::{fs::File, io::BufReader, path::Path, sync::Mutex};

pub(crate) struct Buffer {
    rope: Rope,
    selections: Vec<Mutex<Selection>>,
    primary_selection_index: usize,
    viewport_lines_offset: usize,
    viewport_columns_offset: usize,
}

pub(crate) enum Operation {
    ScrollUp(usize),
    ScrollDown(usize),
    ScrollLeft(usize),
    ScrollRight(usize),
    MoveUp(usize),
    MoveDown(usize),
    MoveLeft(usize),
    MoveRight(usize),
    NextSelection,
    PreviousSelection,
    PrimarySelection(selection::Operation),
    AllSelections(selection::Operation),
    Insert(char),
    Delete,
    Backspace,
}

impl Buffer {
    pub(crate) fn new() -> Self {
        let rope = Rope::new();
        let selections = vec![Mutex::new(Selection::default())];

        Buffer {
            rope,
            selections,
            primary_selection_index: 0,
            viewport_lines_offset: 0,
            viewport_columns_offset: 0,
        }
    }

    pub(crate) fn from_file<P>(path: P) -> Self
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
            primary_selection_index: 0,
            viewport_lines_offset: 0,
            viewport_columns_offset: 0,
        }
    }

    fn from_str(s: &str) -> Self {
        let rope = Rope::from_str(s);
        let selections = vec![Mutex::new(Selection::default())];

        Buffer {
            rope,
            selections,
            primary_selection_index: 0,
            viewport_lines_offset: 0,
            viewport_columns_offset: 0,
        }
    }

    pub(crate) fn apply_operation(&mut self, operation: Operation) {
        use Operation::*;

        match operation {
            ScrollUp(distance) => {
                self.scroll_up(distance);
            }
            ScrollDown(distance) => {
                self.scroll_down(distance);
            }
            ScrollLeft(distance) => {
                self.scroll_left(distance);
            }
            ScrollRight(distance) => {
                self.scroll_right(distance);
            }
            MoveUp(distance) => {
                self.move_up(distance);
            }
            MoveDown(distance) => {
                self.move_down(distance);
            }
            MoveLeft(distance) => {
                self.move_left(distance);
            }
            MoveRight(distance) => {
                self.move_right(distance);
            }
            NextSelection => {
                if self.primary_selection_index >= self.selections.len() {
                    self.primary_selection_index = 0;
                } else {
                    self.primary_selection_index += 1;
                }
            }
            PreviousSelection => {
                if self.primary_selection_index == 0 {
                    self.primary_selection_index = self.selections.len() - 1;
                } else {
                    self.primary_selection_index -= 1;
                }
            }
            PrimarySelection(operation) => {
                self.selections[self.primary_selection_index]
                    .lock()
                    .unwrap()
                    .apply_operation(operation);
            }
            AllSelections(operation) => {
                for selection in &self.selections {
                    selection.lock().unwrap().apply_operation(operation.clone());
                }
            }
            Insert(c) => {
                self.insert(c);
            }
            Backspace => {
                self.backspace();
            }
            Delete => {
                self.delete();
            }
        }
    }

    pub(crate) fn rope(&self) -> &Rope {
        &self.rope
    }

    pub(crate) fn selections(&self) -> &Vec<Mutex<Selection>> {
        &self.selections
    }

    pub(crate) fn primary_selection_index(&self) -> usize {
        self.primary_selection_index
    }

    pub(crate) fn viewport_lines_offset(&self) -> usize {
        self.viewport_lines_offset
    }

    pub(crate) fn viewport_columns_offset(&self) -> usize {
        self.viewport_columns_offset
    }

    pub(crate) fn cursor_to_index(&self, cursor: &Cursor) -> Option<usize> {
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

    pub(crate) fn is_valid_cursor(&self, cursor: &Cursor) -> bool {
        self.cursor_to_index(cursor).is_some()
    }

    pub(crate) fn selection_to_slice(&self, selection: &Selection) -> Option<RopeSlice> {
        let anchor_index = self.cursor_to_index(&selection.anchor)?;
        let cursor_index = self.cursor_to_index(&selection.cursor)?;
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

        #[allow(clippy::collapsible_else_if)]
        if let Some(target_column) = cursor.target_column {
            if line_length > target_column {
                Some(Cursor {
                    column: target_column,
                    target_column: None,
                    ..cursor.clone()
                })
            } else {
                Some(Cursor {
                    column: line_length - 1,
                    ..cursor.clone()
                })
            }
        } else {
            if line_length > cursor.column {
                Some(cursor.clone())
            } else {
                Some(Cursor {
                    column: line_length - 1,
                    target_column: Some(cursor.column),
                    ..cursor.clone()
                })
            }
        }
    }

    pub(crate) fn corrected_selection(&self, selection: &Selection) -> Option<Selection> {
        let anchor = self.corrected_cursor(&selection.anchor)?;
        let cursor = self.corrected_cursor(&selection.cursor)?;
        Some(Selection { anchor, cursor })
    }

    fn scroll_up(&mut self, distance: usize) {
        self.viewport_lines_offset = self.viewport_lines_offset.saturating_sub(distance);
    }

    fn scroll_down(&mut self, distance: usize) {
        let new_viewport_lines_offset = self.viewport_lines_offset + distance;
        if new_viewport_lines_offset <= self.rope.len_lines() {
            self.viewport_lines_offset = new_viewport_lines_offset;
        }
    }

    fn scroll_left(&mut self, distance: usize) {
        self.viewport_columns_offset = self.viewport_columns_offset.saturating_sub(distance);
    }

    fn scroll_right(&mut self, distance: usize) {
        self.viewport_columns_offset += distance;
    }

    fn move_up(&mut self, distance: usize) {
        for selection_mutex in &self.selections {
            let mut selection = selection_mutex.lock().unwrap();
            let cursor = self.corrected_cursor(&Cursor {
                line: selection.cursor.line.saturating_sub(distance),
                ..selection.cursor
            });
            match cursor {
                Some(cursor) if self.is_valid_cursor(&cursor) => selection.cursor = cursor,
                _ => {}
            }
        }
    }

    fn move_down(&mut self, distance: usize) {
        for selection_mutex in &self.selections {
            let mut selection = selection_mutex.lock().unwrap();
            let cursor = self.corrected_cursor(&Cursor {
                line: selection.cursor.line + distance,
                ..selection.cursor
            });
            match cursor {
                Some(cursor) if self.is_valid_cursor(&cursor) => selection.cursor = cursor,
                _ => {}
            }
        }
    }

    fn move_left(&mut self, distance: usize) {
        for selection_mutex in &self.selections {
            let mut selection = selection_mutex.lock().unwrap();
            let old_index = self.cursor_to_index(&selection.cursor).unwrap();
            let new_index = old_index.saturating_sub(distance);
            selection.cursor = self.index_to_cursor(new_index).unwrap();
        }
    }

    fn move_right(&mut self, distance: usize) {
        for selection_mutex in &self.selections {
            let mut selection = selection_mutex.lock().unwrap();
            let old_index = self.cursor_to_index(&selection.cursor).unwrap();
            let new_index = old_index + distance;
            if new_index < self.rope.len_chars() {
                selection.cursor = self.index_to_cursor(new_index).unwrap();
            } else {
                selection.cursor = self.index_to_cursor(self.rope.len_chars() - 1).unwrap();
            }
        }
    }

    fn reduce(&mut self) {
        for selection_mutex in &self.selections {
            let mut selection = selection_mutex.lock().unwrap();
            selection.reduce();
        }
    }

    fn insert(&mut self, c: char) {
        for selection_mutex in &self.selections {
            // Insert character
            let mut selection = selection_mutex.lock().unwrap();
            let anchor_index = self.cursor_to_index(&selection.anchor).unwrap();
            let cursor_index = self.cursor_to_index(&selection.cursor).unwrap();

            if selection.is_forwards() {
                self.rope.insert_char(anchor_index, c);
            } else {
                self.rope.insert_char(cursor_index, c);
            }

            // Shift selection
            selection.anchor = self.index_to_cursor(anchor_index + 1).unwrap();
            selection.cursor = self.index_to_cursor(cursor_index + 1).unwrap();
        }
    }

    fn backspace(&mut self) {
        for selection_mutex in &self.selections {
            let mut selection = selection_mutex.lock().unwrap();
            let anchor_index = self.cursor_to_index(&selection.cursor).unwrap();
            let cursor_index = self.cursor_to_index(&selection.cursor).unwrap();
            if cursor_index > 0 {
                // Delete character
                self.rope.remove(cursor_index - 1..cursor_index);

                // Shift selection
                selection.anchor = self.index_to_cursor(anchor_index - 1).unwrap();
                selection.cursor = self.index_to_cursor(cursor_index - 1).unwrap();
            }
        }
    }

    fn delete(&mut self) {
        for selection_mutex in &self.selections {
            let mut selection = selection_mutex.lock().unwrap();
            selection.flip_backwards();
            let anchor_index = self.cursor_to_index(&selection.anchor).unwrap();
            let cursor_index = self.cursor_to_index(&selection.cursor).unwrap();
            self.rope.remove(cursor_index..=anchor_index);
            selection.reduce();
            if let Some(s) = self.corrected_selection(&selection) {
                *selection = s;
            };
        }
    }
}

#[test]
fn test_index_cursor() {
    fn case(s: &str, tuple: (usize, usize), expected: char) {
        let buffer = Buffer::from_str(s);
        let cursor = Cursor::from(tuple);
        let index = buffer.cursor_to_index(&cursor).unwrap();
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
        let actual = buffer
            .cursor_to_index(&cursor)
            .and_then(|i| buffer.index_to_cursor(i));
        let expected = Some(cursor);
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
    fn case(
        s: &str,
        original: (usize, usize, Option<usize>),
        corrected: (usize, usize, Option<usize>),
    ) {
        let buffer = Buffer::from_str(s);
        let expected = Some(Cursor {
            line: corrected.0,
            column: corrected.1,
            target_column: corrected.2,
        });
        let actual = buffer.corrected_cursor(&Cursor {
            line: original.0,
            column: original.1,
            target_column: original.2,
        });
        assert!(
            expected == actual,
            "\nexpected = {:?}\nactual = {:?}\n",
            expected,
            actual
        );
    }

    case("abc\nx\n", (0, 0, None), (0, 0, None));
    case("abc\nx\n", (0, 99, None), (0, 3, Some(99)));
    case("abc\nx\n", (1, 0, Some(1)), (1, 1, None));
    case("abc\nx\n", (1, 99, None), (1, 1, Some(99)));
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
