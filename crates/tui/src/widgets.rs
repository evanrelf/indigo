mod command_bar;
mod editor;
mod line_numbers;
mod navigation_bar;
mod selection;
mod status_bar;
mod text;
mod tildes;

pub use crate::widgets::{
    command_bar::CommandBar, editor::Editor, line_numbers::LineNumbers,
    navigation_bar::NavigationBar, selection::Selection, status_bar::StatusBar, text::Text,
    tildes::Tildes,
};
