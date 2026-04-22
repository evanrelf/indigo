pub mod flexoki;

use ratatui::prelude::Color;

pub struct Theme {
    pub status_bar_bg: Color,
    pub line_numbers_fg: Color,
    pub dots: Color,
    pub scroll_bar_track: Color,
    pub scroll_bar_thumb: Color,
    pub primary_cursor_fg: Color,
    pub primary_cursor_bg: Color,
    pub primary_range_bg: Color,
    pub secondary_cursor_fg: Color,
    pub secondary_cursor_bg: Color,
    pub secondary_range_bg: Color,
    pub empty_range_fg: Color,
    pub empty_range_bg: Color,
    pub error_bg: Color,
    pub whitespace_fg: Color,
}

pub const THEME: Theme = flexoki::light::THEME;
