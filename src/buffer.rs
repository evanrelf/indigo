use crate::{
    cursor::Cursor,
    direction::Direction,
    operand::Operand,
    range::Range,
    rope::Rope,
    selection::{self, Selection},
};
use std::{borrow::Cow, fs::File, io::BufReader, path::Path, str::FromStr};
use tui::widgets::Widget;

pub struct Buffer {
    rope: Rope,
    selection: Selection,
    view_lines_offset: usize,
    view_columns_offset: usize,
}

impl Buffer {
    pub fn new() -> Self {
        let rope = ropey::Rope::new().into();
        let selection = Selection::new(&[Range::default()]);

        Self {
            rope,
            selection,
            view_lines_offset: 0,
            view_columns_offset: 0,
        }
    }

    pub fn from_file<P>(path: P) -> Self
    where
        P: AsRef<Path>,
    {
        let file = File::open(path).unwrap();
        let reader = BufReader::new(file);

        let rope = ropey::Rope::from_reader(reader).unwrap().into();
        let selection = Selection::new(&[Range::default()]);

        Self {
            rope,
            selection,
            view_lines_offset: 0,
            view_columns_offset: 0,
        }
    }

    pub fn rope(&self) -> &Rope {
        &self.rope
    }

    pub fn selection(&self) -> &Selection {
        &self.selection
    }

    pub fn view_lines_offset(&self) -> usize {
        self.view_lines_offset
    }

    pub fn view_columns_offset(&self) -> usize {
        self.view_columns_offset
    }

    fn scroll_up(&mut self, distance: usize) {
        self.view_lines_offset = self.view_lines_offset.saturating_sub(distance);
    }

    fn scroll_down(&mut self, distance: usize) {
        let new_view_lines_offset = self.view_lines_offset + distance;
        if new_view_lines_offset <= self.rope.len_lines() {
            self.view_lines_offset = new_view_lines_offset;
        }
    }

    fn scroll_left(&mut self, distance: usize) {
        self.view_columns_offset = self.view_columns_offset.saturating_sub(distance);
    }

    fn scroll_right(&mut self, distance: usize) {
        self.view_columns_offset += distance;
    }

    fn move_up(&mut self, distance: usize) {
        for range in &mut self.selection.ranges {
            let head = self.rope.corrected_cursor(&Cursor {
                line: range.head.line.saturating_sub(distance),
                ..range.head
            });
            match head {
                Some(head) if self.rope.is_valid_cursor(&head) => range.head = head,
                _ => {}
            }
        }
    }

    fn move_down(&mut self, distance: usize) {
        for range in &mut self.selection.ranges {
            let head = self.rope.corrected_cursor(&Cursor {
                line: range.head.line + distance,
                ..range.head
            });
            match head {
                Some(head) if self.rope.is_valid_cursor(&head) => range.head = head,
                _ => {}
            }
        }
    }

    fn move_left(&mut self, distance: usize) {
        for range in &mut self.selection.ranges {
            let old_index = self.rope.cursor_to_index(&range.head).unwrap();
            let new_index = old_index.saturating_sub(distance);
            range.head = self.rope.index_to_cursor(new_index).unwrap();
        }
    }

    fn move_right(&mut self, distance: usize) {
        for range in &mut self.selection.ranges {
            let old_index = self.rope.cursor_to_index(&range.head).unwrap();
            let new_index = old_index + distance;
            if new_index < self.rope.len_chars() {
                range.head = self.rope.index_to_cursor(new_index).unwrap();
            } else {
                range.head = self
                    .rope
                    .index_to_cursor(self.rope.len_chars() - 1)
                    .unwrap();
            }
        }
    }

    fn insert(&mut self, c: char) {
        for range in &mut self.selection.ranges {
            // Insert character
            let anchor_index = self.rope.cursor_to_index(&range.anchor).unwrap();
            let head_index = self.rope.cursor_to_index(&range.head).unwrap();

            if range.is_forwards() {
                self.rope.insert_char(anchor_index, c);
            } else {
                self.rope.insert_char(head_index, c);
            }

            // Shift selection
            range.anchor = self.rope.index_to_cursor(anchor_index + 1).unwrap();
            range.head = self.rope.index_to_cursor(head_index + 1).unwrap();
        }
    }

    fn backspace(&mut self) {
        for range in &mut self.selection.ranges {
            let anchor_index = self.rope.cursor_to_index(&range.head).unwrap();
            let head_index = self.rope.cursor_to_index(&range.head).unwrap();
            if head_index > 0 {
                // Delete character
                self.rope.remove(head_index - 1..head_index);

                // Shift selection
                range.anchor = self.rope.index_to_cursor(anchor_index - 1).unwrap();
                range.head = self.rope.index_to_cursor(head_index - 1).unwrap();
            }
        }
    }

    fn delete(&mut self) {
        for range in &mut self.selection.ranges {
            range.flip_backwards();
            let anchor_index = self.rope.cursor_to_index(&range.anchor).unwrap();
            let head_index = self.rope.cursor_to_index(&range.head).unwrap();
            self.rope.remove(head_index..=anchor_index);
            range.reduce();
            if let Some(s) = self.rope.corrected_range(range) {
                *range = s;
            };
        }
    }
}

impl Default for Buffer {
    fn default() -> Self {
        Self::new()
    }
}

impl FromStr for Buffer {
    type Err = std::convert::Infallible;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let rope = ropey::Rope::from_str(s).into();
        let selection = Selection::new(&[Range::default()]);

        Ok(Self {
            rope,
            selection,
            view_lines_offset: 0,
            view_columns_offset: 0,
        })
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
            (self.rope.len_lines() - self.view_lines_offset) as u16,
        );

        let blank = Buffer::empty(Rect { height, ..area });
        buffer.merge(&blank);

        let view_slice = {
            let view_start_line = self.view_lines_offset;
            let view_end_line = view_start_line + (area.height as usize - 1);
            let buffer_end_line = self.rope().len_lines();
            let start_char_index = self.rope().line_to_char(view_start_line);
            if buffer_end_line <= view_end_line {
                self.rope().slice(start_char_index..)
            } else {
                let end_char_index = self.rope().line_to_char(view_end_line);
                self.rope().slice(start_char_index..end_char_index)
            }
        };

        let chunks = Layout::default()
            .direction(Direction::Horizontal)
            .constraints([Constraint::Length(4), Constraint::Min(0)].as_ref())
            .split(area);

        for (i, line) in view_slice.lines().enumerate() {
            let line = if line.len_chars().saturating_sub(self.view_columns_offset) > 0 {
                Cow::from(line.slice(self.view_columns_offset..))
                    .trim_end()
                    .to_string()
            } else {
                "".to_string()
            };

            buffer.set_stringn(
                chunks[0].left(),
                chunks[0].top() + i as u16,
                format!("{:>3} ", self.view_lines_offset + i + 1),
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

        for range in &self.selection.ranges {
            let anchor_cursor = (&range.anchor, Style::default().bg(Color::LightCyan));
            let head_cursor = (&range.head, Style::default().bg(Color::LightYellow));

            for (cursor, style) in [anchor_cursor, head_cursor] {
                let buffer_line = cursor.line;
                let buffer_column = cursor.column;
                let view_line = buffer_line.saturating_sub(self.view_lines_offset);
                let view_column = buffer_column.saturating_sub(self.view_columns_offset);

                let cursor_visible = [
                    buffer_line >= self.view_lines_offset,
                    buffer_column >= self.view_columns_offset,
                    view_line < chunks[1].bottom() as usize,
                    view_column < chunks[1].right() as usize,
                ]
                .iter()
                .all(|x| *x);

                if cursor_visible {
                    buffer
                        .get_mut(
                            chunks[1].left() + view_column as u16,
                            chunks[1].top() + view_line as u16,
                        )
                        .set_style(style);
                }
            }
        }
    }
}

pub enum Operation {
    Scroll(Direction, usize),
    Move(Direction, usize),
    Insert(char),
    Delete,
    Backspace,
    InSelection(selection::Operation),
}

impl Operand for Buffer {
    type Operation = Operation;

    fn apply(&mut self, operation: Self::Operation) {
        use Operation::*;

        match operation {
            Scroll(direction, distance) => match direction {
                Direction::Up => self.scroll_up(distance),
                Direction::Down => self.scroll_down(distance),
                Direction::Left => self.scroll_left(distance),
                Direction::Right => self.scroll_right(distance),
            },
            Move(direction, distance) => match direction {
                Direction::Up => self.move_up(distance),
                Direction::Down => self.move_down(distance),
                Direction::Left => self.move_left(distance),
                Direction::Right => self.move_right(distance),
            },
            Insert(c) => {
                self.insert(c);
            }
            Backspace => {
                self.backspace();
            }
            Delete => {
                self.delete();
            }
            InSelection(operation) => {
                self.selection.apply(operation);
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_index_cursor() {
        fn case(s: &str, tuple: (usize, usize), expected: char) {
            let buffer = Buffer::from_str(s).unwrap();
            let cursor = Cursor::from(tuple);
            let index = buffer.rope.cursor_to_index(&cursor).unwrap();
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
            let buffer = Buffer::from_str(s).unwrap();
            let cursor = Cursor::from(tuple);
            let actual = buffer
                .rope
                .cursor_to_index(&cursor)
                .and_then(|i| buffer.rope.index_to_cursor(i));
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
            let buffer = Buffer::from_str(s).unwrap();
            let expected = Some(Cursor {
                line: corrected.0,
                column: corrected.1,
                target_column: corrected.2,
            });
            let actual = buffer.rope.corrected_cursor(&Cursor {
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
    fn test_range_to_slice() {
        fn case(s: &str, range: ((usize, usize), (usize, usize)), expected: &str) {
            let buffer = Buffer::from_str(s).unwrap();
            let range = Range::new(range.0, range.1);
            let expected = Some(expected);
            let actual = buffer
                .rope
                .range_to_slice(&range)
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
}
