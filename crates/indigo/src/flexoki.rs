#![allow(dead_code)]

use ratatui::prelude::Color;

// https://stephango.com/flexoki

pub const WHITE: Color = Color::from_u32(0x00_FFFFFF);
pub const PAPER: Color = Color::from_u32(0x00_FFFCF0);
pub const BASE_50: Color = Color::from_u32(0x00_F2F0E5);
pub const BASE_100: Color = Color::from_u32(0x00_E6E4D9);
pub const BASE_150: Color = Color::from_u32(0x00_DAD8CE);
pub const BASE_200: Color = Color::from_u32(0x00_CECDC3);
pub const BASE_300: Color = Color::from_u32(0x00_B7B5AC);
pub const BASE_400: Color = Color::from_u32(0x00_9F9D96);
pub const BASE_500: Color = Color::from_u32(0x00_878580);
pub const BASE_600: Color = Color::from_u32(0x00_6F6E69);
pub const BASE_700: Color = Color::from_u32(0x00_575653);
pub const BASE_800: Color = Color::from_u32(0x00_403E3C);
pub const BASE_850: Color = Color::from_u32(0x00_343331);
pub const BASE_900: Color = Color::from_u32(0x00_282726);
pub const BASE_950: Color = Color::from_u32(0x00_1C1B1A);
pub const BLACK: Color = Color::from_u32(0x00_100F0F);

pub const RED_50: Color = Color::from_u32(0x00_FFE1D5);
pub const RED_100: Color = Color::from_u32(0x00_FFCABB);
pub const RED_150: Color = Color::from_u32(0x00_FDB2A2);
pub const RED_200: Color = Color::from_u32(0x00_F89A8A);
pub const RED_300: Color = Color::from_u32(0x00_E8705F);
pub const RED_400: Color = Color::from_u32(0x00_D14D41);
pub const RED_500: Color = Color::from_u32(0x00_C03E35);
pub const RED_600: Color = Color::from_u32(0x00_AF3029);
pub const RED_700: Color = Color::from_u32(0x00_942822);
pub const RED_800: Color = Color::from_u32(0x00_6C201C);
pub const RED_850: Color = Color::from_u32(0x00_551B18);
pub const RED_900: Color = Color::from_u32(0x00_3E1715);
pub const RED_950: Color = Color::from_u32(0x00_261312);

pub const ORANGE_50: Color = Color::from_u32(0x00_FFE7CE);
pub const ORANGE_100: Color = Color::from_u32(0x00_FED3AF);
pub const ORANGE_150: Color = Color::from_u32(0x00_FCC192);
pub const ORANGE_200: Color = Color::from_u32(0x00_F9AE77);
pub const ORANGE_300: Color = Color::from_u32(0x00_EC8B49);
pub const ORANGE_400: Color = Color::from_u32(0x00_DA702C);
pub const ORANGE_500: Color = Color::from_u32(0x00_CB6120);
pub const ORANGE_600: Color = Color::from_u32(0x00_BC5215);
pub const ORANGE_700: Color = Color::from_u32(0x00_9D4310);
pub const ORANGE_800: Color = Color::from_u32(0x00_71320D);
pub const ORANGE_850: Color = Color::from_u32(0x00_59290D);
pub const ORANGE_900: Color = Color::from_u32(0x00_40200D);
pub const ORANGE_950: Color = Color::from_u32(0x00_27180E);

pub const YELLOW_50: Color = Color::from_u32(0x00_FAEEC6);
pub const YELLOW_100: Color = Color::from_u32(0x00_F6E2A0);
pub const YELLOW_150: Color = Color::from_u32(0x00_F1D67E);
pub const YELLOW_200: Color = Color::from_u32(0x00_ECCB60);
pub const YELLOW_300: Color = Color::from_u32(0x00_DFB431);
pub const YELLOW_400: Color = Color::from_u32(0x00_D0A215);
pub const YELLOW_500: Color = Color::from_u32(0x00_BE9207);
pub const YELLOW_600: Color = Color::from_u32(0x00_AD8301);
pub const YELLOW_700: Color = Color::from_u32(0x00_8E6B01);
pub const YELLOW_800: Color = Color::from_u32(0x00_664D01);
pub const YELLOW_850: Color = Color::from_u32(0x00_503D02);
pub const YELLOW_900: Color = Color::from_u32(0x00_3A2D04);
pub const YELLOW_950: Color = Color::from_u32(0x00_241E08);

pub const GREEN_50: Color = Color::from_u32(0x00_EDEECF);
pub const GREEN_100: Color = Color::from_u32(0x00_DDE2B2);
pub const GREEN_150: Color = Color::from_u32(0x00_CDD597);
pub const GREEN_200: Color = Color::from_u32(0x00_BEC97E);
pub const GREEN_300: Color = Color::from_u32(0x00_A0AF54);
pub const GREEN_400: Color = Color::from_u32(0x00_879A39);
pub const GREEN_500: Color = Color::from_u32(0x00_768D21);
pub const GREEN_600: Color = Color::from_u32(0x00_66800B);
pub const GREEN_700: Color = Color::from_u32(0x00_536907);
pub const GREEN_800: Color = Color::from_u32(0x00_3D4C07);
pub const GREEN_850: Color = Color::from_u32(0x00_313D07);
pub const GREEN_900: Color = Color::from_u32(0x00_252D09);
pub const GREEN_950: Color = Color::from_u32(0x00_1A1E0C);

pub const CYAN_50: Color = Color::from_u32(0x00_DDF1E4);
pub const CYAN_100: Color = Color::from_u32(0x00_BFE8D9);
pub const CYAN_150: Color = Color::from_u32(0x00_A2DECE);
pub const CYAN_200: Color = Color::from_u32(0x00_87D3C3);
pub const CYAN_300: Color = Color::from_u32(0x00_5ABDAC);
pub const CYAN_400: Color = Color::from_u32(0x00_3AA99F);
pub const CYAN_500: Color = Color::from_u32(0x00_2F968D);
pub const CYAN_600: Color = Color::from_u32(0x00_24837B);
pub const CYAN_700: Color = Color::from_u32(0x00_1C6C66);
pub const CYAN_800: Color = Color::from_u32(0x00_164F4A);
pub const CYAN_850: Color = Color::from_u32(0x00_143F3C);
pub const CYAN_900: Color = Color::from_u32(0x00_122F2C);
pub const CYAN_950: Color = Color::from_u32(0x00_101F1D);

pub const BLUE_50: Color = Color::from_u32(0x00_E1ECEB);
pub const BLUE_100: Color = Color::from_u32(0x00_C6DDE8);
pub const BLUE_150: Color = Color::from_u32(0x00_ABCFE2);
pub const BLUE_200: Color = Color::from_u32(0x00_92BFDB);
pub const BLUE_300: Color = Color::from_u32(0x00_66A0C8);
pub const BLUE_400: Color = Color::from_u32(0x00_4385BE);
pub const BLUE_500: Color = Color::from_u32(0x00_3171B2);
pub const BLUE_600: Color = Color::from_u32(0x00_205EA6);
pub const BLUE_700: Color = Color::from_u32(0x00_1A4F8C);
pub const BLUE_800: Color = Color::from_u32(0x00_163B66);
pub const BLUE_850: Color = Color::from_u32(0x00_133051);
pub const BLUE_900: Color = Color::from_u32(0x00_12253B);
pub const BLUE_950: Color = Color::from_u32(0x00_101A24);

pub const PURPLE_50: Color = Color::from_u32(0x00_F0EAEC);
pub const PURPLE_100: Color = Color::from_u32(0x00_E2D9E9);
pub const PURPLE_150: Color = Color::from_u32(0x00_D3CAE6);
pub const PURPLE_200: Color = Color::from_u32(0x00_C4B9E0);
pub const PURPLE_300: Color = Color::from_u32(0x00_A699D0);
pub const PURPLE_400: Color = Color::from_u32(0x00_8B7EC8);
pub const PURPLE_500: Color = Color::from_u32(0x00_735EB5);
pub const PURPLE_600: Color = Color::from_u32(0x00_5E409D);
pub const PURPLE_700: Color = Color::from_u32(0x00_4F3685);
pub const PURPLE_800: Color = Color::from_u32(0x00_3C2A62);
pub const PURPLE_850: Color = Color::from_u32(0x00_31234E);
pub const PURPLE_900: Color = Color::from_u32(0x00_261C39);
pub const PURPLE_950: Color = Color::from_u32(0x00_1A1623);

pub const MAGENTA_50: Color = Color::from_u32(0x00_FEE4E5);
pub const MAGENTA_100: Color = Color::from_u32(0x00_FCCFDA);
pub const MAGENTA_150: Color = Color::from_u32(0x00_F9B9CF);
pub const MAGENTA_200: Color = Color::from_u32(0x00_F4A4C2);
pub const MAGENTA_300: Color = Color::from_u32(0x00_E47DA8);
pub const MAGENTA_400: Color = Color::from_u32(0x00_CE5D97);
pub const MAGENTA_500: Color = Color::from_u32(0x00_B74583);
pub const MAGENTA_600: Color = Color::from_u32(0x00_A02F6F);
pub const MAGENTA_700: Color = Color::from_u32(0x00_87285E);
pub const MAGENTA_800: Color = Color::from_u32(0x00_641F46);
pub const MAGENTA_850: Color = Color::from_u32(0x00_4F1B39);
pub const MAGENTA_900: Color = Color::from_u32(0x00_39172B);
pub const MAGENTA_950: Color = Color::from_u32(0x00_24131D);

pub mod light {
    #![allow(clippy::wildcard_imports)]

    use super::*;

    pub const TX: Color = BLACK;
    pub const TX_2: Color = BASE_600;
    pub const TX_3: Color = BASE_300;
    pub const UI_3: Color = BASE_200;
    pub const UI_2: Color = BASE_150;
    pub const UI: Color = BASE_100;
    pub const BG_2: Color = BASE_50;
    pub const BG: Color = PAPER;
    pub const RED: Color = RED_600;
    pub const ORANGE: Color = ORANGE_600;
    pub const YELLOW: Color = YELLOW_600;
    pub const GREEN: Color = GREEN_600;
    pub const CYAN: Color = CYAN_600;
    pub const BLUE: Color = BLUE_600;
    pub const PURPLE: Color = PURPLE_600;
    pub const MAGENTA: Color = MAGENTA_600;
}

pub mod dark {
    #![allow(clippy::wildcard_imports)]

    use super::*;

    pub const BG: Color = BLACK;
    pub const BG_2: Color = BASE_950;
    pub const UI: Color = BASE_900;
    pub const UI_2: Color = BASE_850;
    pub const UI_3: Color = BASE_800;
    pub const TX_3: Color = BASE_700;
    pub const TX_2: Color = BASE_500;
    pub const TX: Color = BASE_200;
    pub const RED: Color = RED_400;
    pub const ORANGE: Color = ORANGE_400;
    pub const YELLOW: Color = YELLOW_400;
    pub const GREEN: Color = GREEN_400;
    pub const CYAN: Color = CYAN_400;
    pub const BLUE: Color = BLUE_400;
    pub const PURPLE: Color = PURPLE_400;
    pub const MAGENTA: Color = MAGENTA_400;
}
