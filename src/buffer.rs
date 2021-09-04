use crate::position::Position;
use crate::selection::Selection;
use ropey::Rope;
use std::fs::File;
use std::io::BufReader;
use std::path::Path;
use std::sync::Mutex;

pub struct Buffer {
    pub rope: Rope,

    pub selections: Vec<Mutex<Selection>>,
    pub primary_selection: usize,

    pub viewport_lines_offset: usize,
    pub viewport_columns_offset: usize,
}

impl Buffer {
    pub fn new() -> Buffer {
        let rope = Rope::new();

        let selections = vec![Mutex::new(Selection::default())];

        Buffer {
            rope,

            selections,
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

        let rope = Rope::from_reader(reader).unwrap();

        let selections = vec![Mutex::new(Selection::default())];

        Buffer {
            rope,

            selections,
            primary_selection: 0,

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

            viewport_lines_offset: 0,
            viewport_columns_offset: 0,
        }
    }

    pub fn scroll_up(&mut self, distance: usize) -> &mut Buffer {
        self.viewport_lines_offset = self.viewport_lines_offset.saturating_sub(distance);
        self
    }

    pub fn scroll_down(&mut self, distance: usize) -> &mut Buffer {
        let new_viewport_lines_offset = self.viewport_lines_offset.saturating_add(distance);
        if new_viewport_lines_offset <= self.rope.len_lines() {
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

    pub fn move_up(&mut self, distance: usize) -> &mut Buffer {
        for selection_mutex in &self.selections {
            let mut selection = selection_mutex.lock().unwrap();
            // TODO: Panics when moving to invalid position
            selection.cursor = *selection.cursor.move_up(distance);
        }
        self
    }

    pub fn move_down(&mut self, distance: usize) -> &mut Buffer {
        for selection_mutex in &self.selections {
            let mut selection = selection_mutex.lock().unwrap();
            // TODO: Panics when moving to invalid position
            selection.cursor = *selection.cursor.move_down(distance);
        }
        self
    }

    pub fn move_left(&mut self, distance: usize) -> &mut Buffer {
        for selection_mutex in &self.selections {
            let mut selection = selection_mutex.lock().unwrap();
            let old_index = selection.cursor.to_index(&self.rope).unwrap();
            let new_index = old_index.saturating_sub(distance);
            selection.cursor = Position::from_index(&self.rope, new_index).unwrap();
        }
        self
    }

    pub fn move_right(&mut self, distance: usize) -> &mut Buffer {
        for selection_mutex in &self.selections {
            let mut selection = selection_mutex.lock().unwrap();
            let old_index = selection.cursor.to_index(&self.rope).unwrap();
            let new_index = old_index.saturating_add(distance);
            if new_index < self.rope.len_chars() {
                selection.cursor = Position::from_index(&self.rope, new_index).unwrap();
            }
        }
        self
    }

    pub fn reduce(&mut self) -> &mut Buffer {
        for selection_mutex in &self.selections {
            let mut selection = selection_mutex.lock().unwrap();
            selection.reduce();
        }
        self
    }

    pub fn delete(&mut self) -> &mut Buffer {
        for selection_mutex in &self.selections {
            let selection = selection_mutex.lock().unwrap();
            let anchor_index = selection.anchor.to_index(&self.rope).unwrap();
            let cursor_index = selection.cursor.to_index(&self.rope).unwrap();
            if selection.is_forwards() {
                self.rope.remove(anchor_index..=cursor_index);
            } else {
                self.rope.remove(cursor_index..=anchor_index);
            }
        }
        self
    }
}

#[test]
fn test_index_position() {
    fn case(s: &str, tuple: (usize, usize), expected: char) {
        let rope = Rope::from_str(s);
        let position = Position::from(tuple);
        let index = position.to_index(&rope).unwrap();
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
fn test_index_position_roundtrip() {
    enum CaseResult {
        Pass,
        Fail,
    }
    use CaseResult::*;

    fn case(result: CaseResult, s: &str, tuple: (usize, usize)) {
        let rope = Rope::from_str(s);
        let position = Position::from(tuple);
        let expected = Some(position);
        let actual = position
            .to_index(&rope)
            .and_then(|i| Position::from_index(&rope, i));
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
        let rope = Rope::from_str(s);
        let input = Position::from(original);
        let expected = Some(Position::from(effective));
        let actual = input.effective(&rope);
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
