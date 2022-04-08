use crossterm::event::{KeyCode, KeyEvent, KeyModifiers};
use indigo_macros::*;

/*
#[test]
fn test() {
    assert_eq!(key_expr!("h"), KeyEvent::new(KeyCode::Char('h'), NONE));
    assert_eq!(
        key_expr!("H"),
        KeyEvent::new(KeyCode::Char('H'), KeyModifiers::SHIFT)
    );
    assert_eq!(
        key_expr!("<c-h>"),
        KeyEvent::new(KeyCode::Char('h'), KeyModifiers::CONTROL)
    );
    assert_eq!(
        key_expr!("<c-H>"),
        KeyEvent::new(
            KeyCode::Char('H'),
            KeyModifiers::CONTROL | KeyModifiers::SHIFT
        )
    );
    assert_eq!(
        key_expr!("<a-h>"),
        KeyEvent::new(KeyCode::Char('h'), KeyModifiers::ALT)
    );
    assert_eq!(
        key_expr!("<a-H>"),
        KeyEvent::new(KeyCode::Char('H'), KeyModifiers::ALT | KeyModifiers::SHIFT)
    );
    assert_eq!(
        key_expr!("<c-a-h>"),
        KeyEvent::new(
            KeyCode::Char('h'),
            KeyModifiers::CONTROL | KeyModifiers::ALT
        )
    );
    assert_eq!(
        key_expr!("<c-a-H>"),
        KeyEvent::new(
            KeyCode::Char('H'),
            KeyModifiers::CONTROL | KeyModifiers::ALT | KeyModifiers::SHIFT
        )
    );
    assert_eq!(
        key_expr!("<a-c-h>"),
        KeyEvent::new(
            KeyCode::Char('h'),
            KeyModifiers::ALT | KeyModifiers::CONTROL
        )
    );
    assert_eq!(
        key_expr!("<a-c-H>"),
        KeyEvent::new(
            KeyCode::Char('H'),
            KeyModifiers::ALT | KeyModifiers::CONTROL | KeyModifiers::SHIFT
        )
    );
}
*/
