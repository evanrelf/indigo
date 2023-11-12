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
        ::crossterm::event::KeyEvent::new($crate::key::macros::key_code!($c), ::crossterm::event::KeyModifiers::NONE)
    };
    ($c:ident) => {
        ::crossterm::event::KeyEvent::new($crate::key::macros::key_code!($c), ::crossterm::event::KeyModifiers::NONE)
    };
    ($($m:ident)+ $c:literal) => {
        ::crossterm::event::KeyEvent::new($crate::key::macros::key_code!($c), $crate::key::macros::key_modifiers!($($m)|+))
    };
    ($($m:ident)+ $c:ident) => {
        ::crossterm::event::KeyEvent::new($crate::key::macros::key_code!($c), $crate::key::macros::key_modifiers!($($m)|+))
    };
}

pub(crate) use key;

macro_rules! k {
    // c
    ($key:expr, $code:literal) => {
        $key.modifiers == ::crossterm::event::KeyModifiers::NONE && $key.code == $crate::key::macros::key_code!($code)
    };
    ($key:expr, $code:ident) => {
        $key.modifiers == ::crossterm::event::KeyModifiers::NONE && $key.code == $crate::key::macros::key_code!($code)
    };
    // m+ c
    ($key:expr, $($modifier:ident)+ , $code:literal) => {
        $key.modifiers == $crate::key::macros::key_modifiers!($($modifier)|+) && $key.code == $crate::key::macros::key_code!($code)
    };
    ($key:expr, $($modifier:ident)+ , $code:ident) => {
        $key.modifiers == $crate::key::macros::key_modifiers!($($modifier)|+) && $key.code == $crate::key::macros::key_code!($code)
    };
}

pub(crate) use k;
