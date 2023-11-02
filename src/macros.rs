#![allow(unused_macros)]

#[macro_export]
macro_rules! key_modifiers {
    () => {
        ::crossterm::event::KeyModifiers::NONE
    };
    ($m:ident) => {
        ::crossterm::event::KeyModifiers::$m
    };
    ($m:ident | $($ms:ident)|+) => {
        ::crossterm::event::KeyModifiers::$m | ::indigo::key_modifiers!($($ms)|+)
    };
}

#[macro_export]
macro_rules! key_code {
    ($c:literal) => {
        ::crossterm::event::KeyCode::Char($c)
    };
    ($c:ident) => {
        ::crossterm::event::KeyCode::$c
    };
}

#[macro_export]
macro_rules! key_kind {
    ($k:ident) => {
        ::crossterm::event::KeyEventKind::$k
    };
}

#[macro_export]
macro_rules! key {
    ($c:literal) => {
        ::crossterm::event::KeyEvent::new(::indigo::key_code!($c), ::indigo::key_modifiers!())
    };
    ($c:ident) => {
        ::crossterm::event::KeyEvent::new(::indigo::key_code!($c), ::indigo::key_modifiers!())
    };
    ($($m:ident)+ $c:literal) => {
        ::crossterm::event::KeyEvent::new(::indigo::key_code!($c), ::indigo::key_modifiers!($($m)|+))
    };
    ($($m:ident)+ $c:ident) => {
        ::crossterm::event::KeyEvent::new(::indigo::key_code!($c), ::indigo::key_modifiers!($($m)|+))
    };
}

#[macro_export]
macro_rules! key_matches {
    ($key:expr, $c:literal) => {
        $key.code == ::indigo::key_code!($c) && $key.modifiers == ::indigo::key_modifiers!()
    };
    ($key:expr, $c:ident) => {
        $key.code == ::indigo::key_code!($c) && $key.modifiers == ::indigo::key_modifiers!()
    };
    ($key:expr, $($m:ident)+ $c:literal) => {
        $key.code == ::indigo::key_code!($c) && $key.modifiers == ::indigo::key_modifiers!($($m)|+)
    };
    ($key:expr, $($m:ident)+ $c:ident) => {
        $key.code == ::indigo::key_code!($c) && $key.modifiers == ::indigo::key_modifiers!($($m)|+)
    };
}
