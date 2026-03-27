pub mod flexoki;

use ratatui::prelude::Color;

pub struct Theme {
    pub status_bar_bg: Color,
    pub line_numbers_fg: Color,
    pub dots: Color,
    pub scroll_bar_track: Color,
    pub scroll_bar_thumb: Color,
    pub cursor_fg: Color,
    pub cursor_bg: Color,
    pub range_bg: Color,
    pub empty_range_fg: Color,
    pub empty_range_bg: Color,
    pub error_bg: Color,
    pub whitespace_fg: Color,
}

pub const THEME: Theme = flexoki::light::THEME;
