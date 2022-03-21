use crate::{
    cursor::Cursor,
    operand::Operand,
    selection::{self, Selection},
};
use parking_lot::Mutex;
use ropey::{Rope, RopeSlice};
use std::{borrow::Cow, fs::File, io::BufReader, path::Path};
use tui::widgets::Widget;

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
    NextSelection(usize),
    PreviousSelection(usize),
    PrimarySelection(selection::Operation),
    AllSelections(selection::Operation),
    Insert(char),
    Delete,
    Backspace,
}

impl Operand for Buffer {
    type Operation = Operation;

    fn apply(&mut self, operation: Self::Operation) {
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
            NextSelection(count) => {
                self.primary_selection_index = wrap_around(
                    self.selections.len(),
                    self.primary_selection_index,
                    count as isize,
                );
            }
            PreviousSelection(count) => {
                self.primary_selection_index = wrap_around(
                    self.selections.len(),
                    self.primary_selection_index,
                    -(count as isize),
                );
            }
            PrimarySelection(operation) => {
                self.selections[self.primary_selection_index]
                    .get_mut()
                    .apply(operation);
            }
            AllSelections(operation) => {
                for selection in &mut self.selections {
                    selection.get_mut().apply(operation.clone());
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
        let head_index = self.cursor_to_index(&selection.head)?;
        if selection.is_forwards() {
            self.rope.get_slice(anchor_index..=head_index)
        } else {
            self.rope.get_slice(head_index..=anchor_index)
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
        let head = self.corrected_cursor(&selection.head)?;
        Some(Selection { anchor, head })
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
            let mut selection = selection_mutex.lock();
            let head = self.corrected_cursor(&Cursor {
                line: selection.head.line.saturating_sub(distance),
                ..selection.head
            });
            match head {
                Some(head) if self.is_valid_cursor(&head) => selection.head = head,
                _ => {}
            }
        }
    }

    fn move_down(&mut self, distance: usize) {
        for selection_mutex in &self.selections {
            let mut selection = selection_mutex.lock();
            let head = self.corrected_cursor(&Cursor {
                line: selection.head.line + distance,
                ..selection.head
            });
            match head {
                Some(head) if self.is_valid_cursor(&head) => selection.head = head,
                _ => {}
            }
        }
    }

    fn move_left(&mut self, distance: usize) {
        for selection_mutex in &self.selections {
            let mut selection = selection_mutex.lock();
            let old_index = self.cursor_to_index(&selection.head).unwrap();
            let new_index = old_index.saturating_sub(distance);
            selection.head = self.index_to_cursor(new_index).unwrap();
        }
    }

    fn move_right(&mut self, distance: usize) {
        for selection_mutex in &self.selections {
            let mut selection = selection_mutex.lock();
            let old_index = self.cursor_to_index(&selection.head).unwrap();
            let new_index = old_index + distance;
            if new_index < self.rope.len_chars() {
                selection.head = self.index_to_cursor(new_index).unwrap();
            } else {
                selection.head = self.index_to_cursor(self.rope.len_chars() - 1).unwrap();
            }
        }
    }

    fn reduce(&mut self) {
        for selection_mutex in &mut self.selections {
            let selection = selection_mutex.get_mut();
            selection.reduce();
        }
    }

    fn insert(&mut self, c: char) {
        for selection_mutex in &self.selections {
            // Insert character
            let mut selection = selection_mutex.lock();
            let anchor_index = self.cursor_to_index(&selection.anchor).unwrap();
            let head_index = self.cursor_to_index(&selection.head).unwrap();

            if selection.is_forwards() {
                self.rope.insert_char(anchor_index, c);
            } else {
                self.rope.insert_char(head_index, c);
            }

            // Shift selection
            selection.anchor = self.index_to_cursor(anchor_index + 1).unwrap();
            selection.head = self.index_to_cursor(head_index + 1).unwrap();
        }
    }

    fn backspace(&mut self) {
        for selection_mutex in &self.selections {
            let mut selection = selection_mutex.lock();
            let anchor_index = self.cursor_to_index(&selection.head).unwrap();
            let head_index = self.cursor_to_index(&selection.head).unwrap();
            if head_index > 0 {
                // Delete character
                self.rope.remove(head_index - 1..head_index);

                // Shift selection
                selection.anchor = self.index_to_cursor(anchor_index - 1).unwrap();
                selection.head = self.index_to_cursor(head_index - 1).unwrap();
            }
        }
    }

    fn delete(&mut self) {
        for selection_mutex in &self.selections {
            let mut selection = selection_mutex.lock();
            selection.flip_backwards();
            let anchor_index = self.cursor_to_index(&selection.anchor).unwrap();
            let head_index = self.cursor_to_index(&selection.head).unwrap();
            self.rope.remove(head_index..=anchor_index);
            selection.reduce();
            if let Some(s) = self.corrected_selection(&selection) {
                *selection = s;
            };
        }
    }
}

impl Widget for &Buffer {
    fn render(self, area: tui::layout::Rect, buffer: &mut tui::buffer::Buffer) {
        use tui::buffer::Buffer;
        use tui::{
            layout::{Constraint, Direction, Layout, Rect},
            style::{Color, Style},
        };

        let height = std::cmp::min(
            area.height - 10,
            (self.rope.len_lines() - self.viewport_lines_offset) as u16,
        );

        let blank = Buffer::empty(Rect { height, ..area });
        buffer.merge(&blank);

        let viewport_slice = {
            let viewport_start_line = self.viewport_lines_offset;
            let viewport_end_line = viewport_start_line + (area.height as usize - 1);
            let buffer_end_line = self.rope().len_lines();
            let start_char_index = self.rope().line_to_char(viewport_start_line);
            if buffer_end_line <= viewport_end_line {
                self.rope().slice(start_char_index..)
            } else {
                let end_char_index = self.rope().line_to_char(viewport_end_line);
                self.rope().slice(start_char_index..end_char_index)
            }
        };

        let chunks = Layout::default()
            .direction(Direction::Horizontal)
            .constraints([Constraint::Length(4), Constraint::Min(0)].as_ref())
            .split(area);

        for (i, line) in viewport_slice.lines().enumerate() {
            let line = if line
                .len_chars()
                .saturating_sub(self.viewport_columns_offset)
                > 0
            {
                Cow::from(line.slice(self.viewport_columns_offset..))
                    .trim_end()
                    .to_string()
            } else {
                "".to_string()
            };

            buffer.set_stringn(
                chunks[0].left(),
                chunks[0].top() + i as u16,
                format!("{:>3} ", self.viewport_lines_offset + i + 1),
                chunks[0].width as usize,
                Style::default().bg(Color::Rgb(0xEE, 0xEE, 0xEE)),
            );

            buffer.set_stringn(
                chunks[1].left(),
                chunks[1].top() + i as u16,
                line,
                chunks[1].width as usize,
                Style::default(),
            );
        }
    }
}

// Modified version of https://stackoverflow.com/a/39740009
fn wrap_around(length: usize, value: usize, delta: isize) -> usize {
    let length = length as isize;
    if length == 0 {
        0
    } else {
        let value = value as isize;
        let result = if delta >= 0 {
            (value + delta) % length
        } else {
            ((value + delta) - (delta * length)) % length
        };
        result as usize
    }
}

#[test]
fn test_wrap_around() {
    assert_eq!(wrap_around(0, 0, 0), 0);
    assert_eq!(wrap_around(0, 0, 4), 0);
    assert_eq!(wrap_around(2, 0, 0), 0);
    assert_eq!(wrap_around(2, 0, 3), 1);
    assert_eq!(wrap_around(2, 0, 4), 0);
    assert_eq!(wrap_around(2, 0, -3), 1);
    assert_eq!(wrap_around(2, 0, -4), 0);
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
