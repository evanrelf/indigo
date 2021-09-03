use crate::position::Position;
use crate::selection::Selection;
use ropey::Rope;
use std::fs::File;
use std::io::BufReader;
use std::path::Path;
use std::sync::{Arc, Mutex};

pub struct Buffer {
    pub rope: Arc<Mutex<Rope>>,

    pub selections: Vec<Mutex<Selection>>,
    pub primary_selection: usize,

    pub viewport_lines_offset: usize,
    pub viewport_columns_offset: usize,
}

impl Buffer {
    pub fn new() -> Buffer {
        Buffer {
            rope: Arc::new(Mutex::new(Rope::new())),

            selections: vec![Mutex::new(Selection::default())],
            primary_selection: 0,

            viewport_lines_offset: 0,
            viewport_columns_offset: 0,
        }
    }

    pub fn from_file<P>(path: P) -> Buffer
    where
        P: AsRef<Path>,
    {
        let file = File::open(path).unwrap();
        let reader = BufReader::new(file);

        Buffer {
            rope: Arc::new(Mutex::new(Rope::from_reader(reader).unwrap())),

            selections: vec![Mutex::new(Selection::default())],
            primary_selection: 0,

            viewport_lines_offset: 0,
            viewport_columns_offset: 0,
        }
    }

    fn from_str(s: &str) -> Buffer {
        Buffer {
            rope: Arc::new(Mutex::new(Rope::from_str(s))),

            selections: vec![Mutex::new(Selection::default())],
            primary_selection: 0,

            viewport_lines_offset: 0,
            viewport_columns_offset: 0,
        }
    }

    pub fn position_to_index(&self, position: Position) -> Option<usize> {
        let Position { line, column } = position;
        let rope = self.rope.lock().unwrap();
        let line_index = rope.try_line_to_char(line).ok()?;
        let line_length = rope.get_line(line)?.len_chars();
        if line_length > column {
            Some(line_index + column)
        } else {
            None
        }
    }

    pub fn index_to_position(&self, index: usize) -> Option<Position> {
        let rope = self.rope.lock().unwrap();
        let line = rope.try_char_to_line(index).ok()?;
        let column = index - rope.try_line_to_char(line).ok()?;
        Some(Position { line, column })
    }

    pub fn effective_position(&self, position: Position) -> Option<Position> {
        let Position { line, column } = position;
        let line_length = self.rope.lock().unwrap().get_line(line)?.len_chars();
        if line_length > column {
            Some(position)
        } else {
            Some(Position {
                line,
                column: line_length - 1,
            })
        }
    }

    pub fn scroll_up(&mut self, distance: usize) -> &mut Buffer {
        self.viewport_lines_offset = self.viewport_lines_offset.saturating_sub(distance);
        self
    }

    pub fn scroll_down(&mut self, distance: usize) -> &mut Buffer {
        let new_viewport_lines_offset = self.viewport_lines_offset.saturating_add(distance);
        if new_viewport_lines_offset <= self.rope.lock().unwrap().len_lines() {
            self.viewport_lines_offset = new_viewport_lines_offset;
        }
        self
    }

    pub fn scroll_left(&mut self, distance: usize) -> &mut Buffer {
        self.viewport_columns_offset = self.viewport_columns_offset.saturating_sub(distance);
        self
    }

    pub fn scroll_right(&mut self, distance: usize) -> &mut Buffer {
        self.viewport_columns_offset = self.viewport_columns_offset.saturating_add(distance);
        self
    }

    pub fn for_each_selection<F>(&self, f: F) -> &Buffer
    where
        F: Fn(&mut Selection),
    {
        for selection_mutex in &self.selections {
            let mut selection = selection_mutex.lock().unwrap();
            f(&mut selection);
        }
        self
    }

    pub fn move_left(&mut self, distance: usize) -> &mut Buffer {
        self.for_each_selection(|selection| {
            let old_index = self.position_to_index(selection.cursor).unwrap();
            let new_index = old_index.saturating_sub(distance);
            selection.cursor = self.index_to_position(new_index).unwrap();
        });
        self
    }

    pub fn move_right(&mut self, distance: usize) -> &mut Buffer {
        self.for_each_selection(|selection| {
            let old_index = self.position_to_index(selection.cursor).unwrap();
            let new_index = old_index.saturating_add(distance);
            if new_index < self.rope.lock().unwrap().len_chars() {
                selection.cursor = self.index_to_position(new_index).unwrap();
            }
        });
        self
    }

    pub fn reduce(&mut self) -> &mut Buffer {
        self.for_each_selection(|selection| {
            selection.reduce();
        });
        self
    }
}

#[test]
fn test_index_position() {
    fn case(s: &str, tuple: (usize, usize), expected: char) {
        let buffer = Buffer::from_str(s);
        let position = Position::from(tuple);
        let index = buffer.position_to_index(position).unwrap();
        let actual = buffer.rope.lock().unwrap().char(index);
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
fn test_index_position_roundtrip() {
    enum CaseResult {
        Pass,
        Fail,
    }
    use CaseResult::*;

    fn case(result: CaseResult, s: &str, tuple: (usize, usize)) {
        let buffer = Buffer::from_str(s);
        let position = Position::from(tuple);
        let expected = Some(position);
        let actual = buffer
            .position_to_index(position)
            .and_then(|i| buffer.index_to_position(i));
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
fn test_effective_position() {
    fn case(s: &str, original: (usize, usize), effective: (usize, usize)) {
        let buffer = Buffer::from_str(s);
        let input = Position::from(original);
        let expected = Some(Position::from(effective));
        let actual = buffer.effective_position(input);
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
