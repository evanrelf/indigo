# Todo

Roughly prioritized list of features I want.

## 1. Mouse click and drag to manipulate selection

- Mouse click and drag events should only be handled in normal mode.

- Requires a conversion from terminal coordinates to byte offsets in the rope,
  accounting for scrolling.

  Here's what that looks like in Blue:

  ```rust
  // src/main.rs
  fn position_to_byte_offset(
      rope: &Rope,
      vertical_scroll: usize,
      area: Rect,
      position: Position,
  ) -> Option<usize> {
      if !area.contains(position) {
          return None;
      }

      let target_column = usize::from(position.x - area.x);
      let row = usize::from(position.y - area.y) + vertical_scroll;

      if row >= rope.line_len() {
          return Some(rope.byte_len());
      }

      let mut current_column = 0;
      let mut byte_offset = rope.byte_of_line(row);

      for grapheme in rope.line(row).graphemes() {
          let grapheme_width = grapheme.as_ref().display_width();
          if current_column + grapheme_width > target_column {
              break;
          }
          current_column += grapheme_width;
          byte_offset += grapheme.len();
      }

      Some(byte_offset)
  }
  ```

- Requires `Range::{move, extend}_to` methods.

  Here's what those look like in Blue:

  ```rust
  // src/editor.rs
  pub fn move_to(&mut self, byte_offset: usize) {
      self.extend_to(byte_offset);
      self.reduce();
      self.extend_left(1);
      self.flip_forward();
  }
  ```

  ```rust
  // src/editor.rs
  pub fn extend_to(&mut self, byte_offset: usize) {
      debug_assert!(self.text.is_grapheme_boundary(byte_offset));
      if self.is_backward() {
          self.head = byte_offset;
      } else {
          self.head = ceil_grapheme_boundary(&self.text.byte_slice(..), byte_offset + 1);
      }
      self.update_desired_column();
  }
  ```

All together, here's what this looks like in Blue, matching on `crossterm` mouse
events:

```rust
// src/main.rs
match mouse.kind {
  // Move
  MouseEventKind::Down(MouseButton::Left) => {
      if let Some(byte_offset) = position_to_byte_offset(
          &editor.text,
          editor.vertical_scroll,
          areas.text,
          Position::new(mouse.column, mouse.row),
      ) {
          editor.move_to(byte_offset);
      }
  }
  // Extend
  MouseEventKind::Down(MouseButton::Right)
  | MouseEventKind::Drag(MouseButton::Left | MouseButton::Right) => {
      if let Some(byte_offset) = position_to_byte_offset(
          &editor.text,
          editor.vertical_scroll,
          areas.text,
          Position::new(mouse.column, mouse.row),
      ) {
          editor.extend_to(byte_offset);
      }
  }
  _ => {}
}
```
