#![allow(unused_macros, unused_imports)]

macro_rules! key_modifiers {
    () => {
        ::crossterm::event::KeyModifiers::NONE
    };
    ($m:ident) => {
        ::crossterm::event::KeyModifiers::$m
    };
    ($m:ident | $($ms:ident)|+) => {
        ::crossterm::event::KeyModifiers::$m | $crate::key::macros::key_modifiers!($($ms)|+)
    };
}

pub(crate) use key_modifiers;

macro_rules! key_code {
    ($c:literal) => {
        ::crossterm::event::KeyCode::Char($c)
    };
    ($c:ident) => {
        ::crossterm::event::KeyCode::$c
    };
}

pub(crate) use key_code;

macro_rules! key_kind {
    ($k:ident) => {
        ::crossterm::event::KeyEventKind::$k
    };
}

pub(crate) use key_kind;

macro_rules! key {
    ($c:literal) => {
        ::crossterm::event::KeyEvent::new($crate::key::macros::key_code!($c), $crate::key::macros::key_modifiers!())
    };
    ($c:ident) => {
        ::crossterm::event::KeyEvent::new($crate::key::macros::key_code!($c), $crate::key::macros::key_modifiers!())
    };
    ($($m:ident)+ $c:literal) => {
        ::crossterm::event::KeyEvent::new($crate::key::macros::key_code!($c), $crate::key::macros::key_modifiers!($($m)|+))
    };
    ($($m:ident)+ $c:ident) => {
        ::crossterm::event::KeyEvent::new($crate::key::macros::key_code!($c), $crate::key::macros::key_modifiers!($($m)|+))
    };
}

pub(crate) use key;

macro_rules! key_matches {
    // _+
    ($key:expr, $(_)+) => {
        true
    };
    // c
    ($key:expr, $c:literal) => {
        $key.code == $crate::key::macros::key_code!($c) && $key.modifiers == $crate::key::macros::key_modifiers!()
    };
    ($key:expr, $c:ident) => {
        $key.code == $crate::key::macros::key_code!($c) && $key.modifiers == $crate::key::macros::key_modifiers!()
    };
    // _ c
    ($key:expr, _ $c:literal) => {
        $key.code == $crate::key::macros::key_code!($c)
    };
    ($key:expr, _ $c:ident) => {
        $key.code == $crate::key::macros::key_code!($c)
    };
    // m+ _
    ($key:expr, $($m:ident)+ _) => {
        $key.modifiers == $crate::key::macros::key_modifiers!($($m)|+)
    };
    // m+ c
    ($key:expr, $($m:ident)+ $c:literal) => {
        $key.code == $crate::key::macros::key_code!($c) && $key.modifiers == $crate::key::macros::key_modifiers!($($m)|+)
    };
    ($key:expr, $($m:ident)+ $c:ident) => {
        $key.code == $crate::key::macros::key_code!($c) && $key.modifiers == $crate::key::macros::key_modifiers!($($m)|+)
    };
    // m+ _ c
    ($key:expr, $($m:ident)+ _ $c:literal) => {
        $key.code == $crate::key::macros::key_code!($c) && $key.modifiers.contains($crate::key::macros::key_modifiers!($($m)|+))
    };
    ($key:expr, $($m:ident)+ _ $c:ident) => {
        $key.code == $crate::key::macros::key_code!($c) && $key.modifiers.contains($crate::key::macros::key_modifiers!($($m)|+))
    };
}

pub(crate) use key_matches;
