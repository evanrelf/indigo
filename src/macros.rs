#![allow(unused_macros)]

macro_rules! key_modifiers {
    () => {
        ::crossterm::event::KeyModifiers::NONE
    };
    ($m:ident) => {
        ::crossterm::event::KeyModifiers::$m
    };
    ($m:ident | $($ms:ident)|+) => {
        ::crossterm::event::KeyModifiers::$m | crate::macros::key_modifiers!($($ms)|+)
    };
}

#[allow(unused_imports)]
pub(crate) use key_modifiers;

macro_rules! key_code {
    ($c:literal) => {
        ::crossterm::event::KeyCode::Char($c)
    };
    ($c:ident) => {
        ::crossterm::event::KeyCode::$c
    };
}

#[allow(unused_imports)]
pub(crate) use key_code;

macro_rules! key_kind {
    ($k:ident) => {
        ::crossterm::event::KeyEventKind::$k
    };
}

#[allow(unused_imports)]
pub(crate) use key_kind;

macro_rules! key {
    ($c:literal) => {
        ::crossterm::event::KeyEvent::new(crate::macros::key_code!($c), crate::macros::key_modifiers!())
    };
    ($c:ident) => {
        ::crossterm::event::KeyEvent::new(crate::macros::key_code!($c), crate::macros::key_modifiers!())
    };
    ($($m:ident)+ $c:literal) => {
        ::crossterm::event::KeyEvent::new(crate::macros::key_code!($c), crate::macros::key_modifiers!($($m)|+))
    };
    ($($m:ident)+ $c:ident) => {
        ::crossterm::event::KeyEvent::new(crate::macros::key_code!($c), crate::macros::key_modifiers!($($m)|+))
    };
}

#[allow(unused_imports)]
pub(crate) use key;

macro_rules! key_matches {
    ($key:expr, $c:literal) => {
        $key.code == crate::macros::key_code!($c) && $key.modifiers == crate::macros::key_modifiers!()
    };
    ($key:expr, $c:ident) => {
        $key.code == crate::macros::key_code!($c) && $key.modifiers == crate::macros::key_modifiers!()
    };
    ($key:expr, $($m:ident)+ $c:literal) => {
        $key.code == crate::macros::key_code!($c) && $key.modifiers == crate::macros::key_modifiers!($($m)|+)
    };
    ($key:expr, $($m:ident)+ $c:ident) => {
        $key.code == crate::macros::key_code!($c) && $key.modifiers == crate::macros::key_modifiers!($($m)|+)
    };
}

#[allow(unused_imports)]
pub(crate) use key_matches;
