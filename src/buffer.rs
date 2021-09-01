use crate::position::Position;
use crate::selection::Selection;
use ropey::Rope;
use std::fs::File;
use std::io::BufReader;
use std::path::Path;

pub struct Buffer {
    pub contents: Rope,

    pub selections: Vec<Selection>,
    pub primary_selection: usize,

    pub viewport_lines_offset: usize,
    pub viewport_columns_offset: usize,
}

impl Buffer {
    pub fn new() -> Buffer {
        Buffer {
            contents: Rope::new(),

            selections: vec![Selection::default()],
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
            contents: Rope::from_reader(reader).unwrap(),

            selections: vec![Selection::default()],
            primary_selection: 0,

            viewport_lines_offset: 0,
            viewport_columns_offset: 0,
        }
    }

    fn from_str(s: &str) -> Buffer {
        Buffer {
            contents: Rope::from_str(s),

            selections: vec![Selection::default()],
            primary_selection: 0,

            viewport_lines_offset: 0,
            viewport_columns_offset: 0,
        }
    }

    pub fn position_to_index(&self, position: &Position) -> Option<usize> {
        let Position { line, column } = position;
        let line_index = self.contents.try_line_to_char(*line).ok()?;
        if self.contents.get_line(*line)?.len_chars() > *column {
            Some(line_index + column)
        } else {
            None
        }
    }

    pub fn index_to_position(&self, index: usize) -> Option<Position> {
        let line = self.contents.try_char_to_line(index).ok()?;
        let column = index - self.contents.try_line_to_char(line).ok()?;
        Some(Position { line, column })
    }

    pub fn scroll_up(&mut self, distance: usize) {
        self.viewport_lines_offset = self.viewport_lines_offset.saturating_sub(distance);
    }

    pub fn scroll_down(&mut self, distance: usize) {
        let new_viewport_lines_offset = self.viewport_lines_offset.saturating_add(distance);
        if new_viewport_lines_offset <= self.contents.len_lines() {
            self.viewport_lines_offset = new_viewport_lines_offset;
        }
    }

    pub fn scroll_left(&mut self, distance: usize) {
        self.viewport_columns_offset = self.viewport_columns_offset.saturating_sub(distance);
    }

    pub fn scroll_right(&mut self, distance: usize) {
        self.viewport_columns_offset = self.viewport_columns_offset.saturating_add(distance);
    }
}

#[test]
fn test_index_position() {
    fn case(s: &str, tuple: (usize, usize), expected: char) {
        let buffer = Buffer::from_str(s);
        let position = Position::from(tuple);
        let index = buffer.position_to_index(&position).unwrap();
        let actual = buffer.contents.char(index);
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
            .position_to_index(&position)
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
