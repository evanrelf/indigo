use ratatui::prelude::Color;

// https://github.com/evanrelf/primer.kak

// All colors

pub const BLACK: Color = Color::Rgb(0x1b, 0x1f, 0x23);

pub const WHITE: Color = Color::Rgb(0xff, 0xff, 0xff);

pub const GRAY_000: Color = Color::Rgb(0xfa, 0xfb, 0xfc);
pub const GRAY_100: Color = Color::Rgb(0xf6, 0xf8, 0xfa);
pub const GRAY_200: Color = Color::Rgb(0xe1, 0xe4, 0xe8);
pub const GRAY_300: Color = Color::Rgb(0xd1, 0xd5, 0xda);
pub const GRAY_400: Color = Color::Rgb(0x95, 0x9d, 0xa5);
pub const GRAY_500: Color = Color::Rgb(0x6a, 0x73, 0x7d);
pub const GRAY_600: Color = Color::Rgb(0x58, 0x60, 0x69);
pub const GRAY_700: Color = Color::Rgb(0x44, 0x4d, 0x56);
pub const GRAY_800: Color = Color::Rgb(0x2f, 0x36, 0x3d);
pub const GRAY_900: Color = Color::Rgb(0x24, 0x29, 0x2e);

pub const BLUE_000: Color = Color::Rgb(0xf1, 0xf8, 0xff);
pub const BLUE_100: Color = Color::Rgb(0xdb, 0xed, 0xff);
pub const BLUE_200: Color = Color::Rgb(0xc8, 0xe1, 0xff);
pub const BLUE_300: Color = Color::Rgb(0x79, 0xb8, 0xff);
pub const BLUE_400: Color = Color::Rgb(0x21, 0x88, 0xff);
pub const BLUE_500: Color = Color::Rgb(0x03, 0x66, 0xd6);
pub const BLUE_600: Color = Color::Rgb(0x00, 0x5c, 0xc5);
pub const BLUE_700: Color = Color::Rgb(0x04, 0x42, 0x89);
pub const BLUE_800: Color = Color::Rgb(0x03, 0x2f, 0x62);
pub const BLUE_900: Color = Color::Rgb(0x05, 0x26, 0x4c);

pub const GREEN_000: Color = Color::Rgb(0xf0, 0xff, 0xf4);
pub const GREEN_100: Color = Color::Rgb(0xdc, 0xff, 0xe4);
pub const GREEN_200: Color = Color::Rgb(0xbe, 0xf5, 0xcb);
pub const GREEN_300: Color = Color::Rgb(0x85, 0xe8, 0x9d);
pub const GREEN_400: Color = Color::Rgb(0x34, 0xd0, 0x58);
pub const GREEN_500: Color = Color::Rgb(0x28, 0xa7, 0x45);
pub const GREEN_600: Color = Color::Rgb(0x22, 0x86, 0x3a);
pub const GREEN_700: Color = Color::Rgb(0x17, 0x6f, 0x2c);
pub const GREEN_800: Color = Color::Rgb(0x16, 0x5c, 0x26);
pub const GREEN_900: Color = Color::Rgb(0x14, 0x46, 0x20);

pub const YELLOW_000: Color = Color::Rgb(0xff, 0xfd, 0xef);
pub const YELLOW_100: Color = Color::Rgb(0xff, 0xfb, 0xdd);
pub const YELLOW_200: Color = Color::Rgb(0xff, 0xf5, 0xb1);
pub const YELLOW_300: Color = Color::Rgb(0xff, 0xea, 0x7f);
pub const YELLOW_400: Color = Color::Rgb(0xff, 0xdf, 0x5d);
pub const YELLOW_500: Color = Color::Rgb(0xff, 0xd3, 0x3d);
pub const YELLOW_600: Color = Color::Rgb(0xf9, 0xc5, 0x13);
pub const YELLOW_700: Color = Color::Rgb(0xdb, 0xab, 0x09);
pub const YELLOW_800: Color = Color::Rgb(0xb0, 0x88, 0x00);
pub const YELLOW_900: Color = Color::Rgb(0x73, 0x5c, 0x0f);

pub const ORANGE_000: Color = Color::Rgb(0xff, 0xf8, 0xf2);
pub const ORANGE_100: Color = Color::Rgb(0xff, 0xeb, 0xda);
pub const ORANGE_200: Color = Color::Rgb(0xff, 0xd1, 0xac);
pub const ORANGE_300: Color = Color::Rgb(0xff, 0xab, 0x70);
pub const ORANGE_400: Color = Color::Rgb(0xfb, 0x85, 0x32);
pub const ORANGE_500: Color = Color::Rgb(0xf6, 0x6a, 0x0a);
pub const ORANGE_600: Color = Color::Rgb(0xe3, 0x62, 0x09);
pub const ORANGE_700: Color = Color::Rgb(0xd1, 0x57, 0x04);
pub const ORANGE_800: Color = Color::Rgb(0xc2, 0x4e, 0x00);
pub const ORANGE_900: Color = Color::Rgb(0xa0, 0x41, 0x00);

pub const RED_000: Color = Color::Rgb(0xff, 0xee, 0xf0);
pub const RED_100: Color = Color::Rgb(0xff, 0xdc, 0xe0);
pub const RED_200: Color = Color::Rgb(0xfd, 0xae, 0xb7);
pub const RED_300: Color = Color::Rgb(0xf9, 0x75, 0x83);
pub const RED_400: Color = Color::Rgb(0xea, 0x4a, 0x5a);
pub const RED_500: Color = Color::Rgb(0xd7, 0x3a, 0x49);
pub const RED_600: Color = Color::Rgb(0xcb, 0x24, 0x31);
pub const RED_700: Color = Color::Rgb(0xb3, 0x1d, 0x28);
pub const RED_800: Color = Color::Rgb(0x9e, 0x1c, 0x23);
pub const RED_900: Color = Color::Rgb(0x86, 0x18, 0x1d);

pub const PURPLE_000: Color = Color::Rgb(0xf5, 0xf0, 0xff);
pub const PURPLE_100: Color = Color::Rgb(0xe6, 0xdc, 0xfd);
pub const PURPLE_200: Color = Color::Rgb(0xd1, 0xbc, 0xf9);
pub const PURPLE_300: Color = Color::Rgb(0xb3, 0x92, 0xf0);
pub const PURPLE_400: Color = Color::Rgb(0x8a, 0x63, 0xd2);
pub const PURPLE_500: Color = Color::Rgb(0x6f, 0x42, 0xc1);
pub const PURPLE_600: Color = Color::Rgb(0x5a, 0x32, 0xa3);
pub const PURPLE_700: Color = Color::Rgb(0x4c, 0x28, 0x89);
pub const PURPLE_800: Color = Color::Rgb(0x3a, 0x1d, 0x6e);
pub const PURPLE_900: Color = Color::Rgb(0x29, 0x13, 0x4e);

pub const PINK_000: Color = Color::Rgb(0xff, 0xee, 0xf8);
pub const PINK_100: Color = Color::Rgb(0xfe, 0xdb, 0xf0);
pub const PINK_200: Color = Color::Rgb(0xf9, 0xb3, 0xdd);
pub const PINK_300: Color = Color::Rgb(0xf6, 0x92, 0xce);
pub const PINK_400: Color = Color::Rgb(0xec, 0x6c, 0xb9);
pub const PINK_500: Color = Color::Rgb(0xea, 0x4a, 0xaa);
pub const PINK_600: Color = Color::Rgb(0xd0, 0x35, 0x92);
pub const PINK_700: Color = Color::Rgb(0xb9, 0x3a, 0x86);
pub const PINK_800: Color = Color::Rgb(0x99, 0x30, 0x6f);
pub const PINK_900: Color = Color::Rgb(0x6d, 0x22, 0x4f);

// Background colors

pub const BG_GRAY_LIGHT: Color = GRAY_000;
pub const BG_GRAY: Color = GRAY_100;
pub const BG_GRAY_DARK: Color = GRAY_900;
pub const BG_BLUE_LIGHT: Color = BLUE_000;
pub const BG_BLUE: Color = BLUE_500;
pub const BG_GREEN_LIGHT: Color = GREEN_100;
pub const BG_GREEN: Color = GREEN_500;
pub const BG_PURPLE_LIGHT: Color = PURPLE_000;
pub const BG_PURPLE: Color = PURPLE_500;
pub const BG_YELLOW_LIGHT: Color = YELLOW_200;
pub const BG_YELLOW: Color = YELLOW_500;
pub const BG_YELLOW_DARK: Color = YELLOW_700;
pub const BG_ORANGE: Color = ORANGE_700;
pub const BG_RED_LIGHT: Color = RED_100;
pub const BG_RED: Color = RED_500;
pub const BG_PINK: Color = PINK_500;

// Foreground colors

pub const FG_GRAY_LIGHT: Color = GRAY_500;
pub const FG_GRAY: Color = GRAY_600;
pub const FG_GRAY_DARK: Color = GRAY_900;
pub const FG_BLUE: Color = BLUE_500;
pub const FG_GREEN: Color = GREEN_500;
pub const FG_PURPLE: Color = PURPLE_500;
pub const FG_YELLOW: Color = YELLOW_800;
pub const FG_ORANGE_LIGHT: Color = ORANGE_600;
pub const FG_ORANGE: Color = ORANGE_900;
pub const FG_RED: Color = RED_600;
pub const FG_PINK: Color = PINK_500;
