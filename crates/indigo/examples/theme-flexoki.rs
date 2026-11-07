use indigo::theme::flexoki;
use ratatui::prelude::Color;

macro_rules! ramp {
    ($color:ident) => {
        pastey::paste! { [
            (50, flexoki::[<$color _50>]), (100, flexoki::[<$color _100>]),
            (150, flexoki::[<$color _150>]), (200, flexoki::[<$color _200>]),
            (300, flexoki::[<$color _300>]), (400, flexoki::[<$color _400>]),
            (500, flexoki::[<$color _500>]), (600, flexoki::[<$color _600>]),
            (700, flexoki::[<$color _700>]), (800, flexoki::[<$color _800>]),
            (850, flexoki::[<$color _850>]), (900, flexoki::[<$color _900>]),
            (950, flexoki::[<$color _950>]),
        ] }
    };
}

fn main() {
    println!("Flexoki by Steph Ango (https://stephango.com/flexoki)\n");
    print_palette();
    print_themes();
}

const RAMPS: &[(&str, [(u16, Color); 13])] = &[
    ("base", ramp!(BASE)),
    ("red", ramp!(RED)),
    ("orange", ramp!(ORANGE)),
    ("yellow", ramp!(YELLOW)),
    ("green", ramp!(GREEN)),
    ("cyan", ramp!(CYAN)),
    ("blue", ramp!(BLUE)),
    ("purple", ramp!(PURPLE)),
    ("magenta", ramp!(MAGENTA)),
];

fn print_palette() {
    let name_width = RAMPS.iter().map(|(name, _)| name.len()).max().unwrap();

    println!("Palette");
    println!();

    print!("{:name_width$} ", "");
    for (shade, _) in RAMPS[0].1 {
        print!(" {shade:>3} ");
    }
    println!();
    for (name, ramp) in RAMPS {
        print!("{name:>name_width$} ");
        for (_, color) in ramp {
            let Color::Rgb(r, g, b) = color else {
                unreachable!()
            };
            print!("\x1b[48;2;{r};{g};{b}m     \x1b[0m");
        }
        println!();
    }
    println!();
}

const TEXT: &[&str] = &[
    "m>>=k", "f<$>x", "&'a T", " let ", "::<_>", "*omg*", "_wtf_", "bbq??", "\\o,o/", "(@_@)",
    "evan!", "<3 <3", "i+=42", "67..?", "69420",
];

fn print_themes() {
    let light_roles = [
        ("bg", flexoki::light::BG),
        ("bg-2", flexoki::light::BG_2),
        ("ui", flexoki::light::UI),
        ("ui-2", flexoki::light::UI_2),
        ("ui-3", flexoki::light::UI_3),
        ("tx-3", flexoki::light::TX_3),
        ("tx-2", flexoki::light::TX_2),
        ("tx", flexoki::light::TX),
    ];
    let dark_roles = [
        ("bg", flexoki::dark::BG),
        ("bg-2", flexoki::dark::BG_2),
        ("ui", flexoki::dark::UI),
        ("ui-2", flexoki::dark::UI_2),
        ("ui-3", flexoki::dark::UI_3),
        ("tx-3", flexoki::dark::TX_3),
        ("tx-2", flexoki::dark::TX_2),
        ("tx", flexoki::dark::TX),
    ];

    println!("Light theme");
    println!();
    print_theme(&light_roles);

    println!();

    println!("Dark theme");
    println!();
    print_theme(&dark_roles);
}

fn print_theme(roles: &[(&str, Color)]) {
    let name_width = RAMPS.iter().map(|(name, _)| name.len()).max().unwrap();

    print!("{:name_width$} ", "");
    for (shade, _) in RAMPS[0].1 {
        print!(" {shade:>3} ");
    }
    println!();
    let (_, bg) = roles.iter().find(|(name, _)| *name == "bg").unwrap();
    let Color::Rgb(bg_r, bg_g, bg_b) = bg else {
        unreachable!()
    };
    let mut i = 0;
    for (name, ramp) in RAMPS {
        print!("{name:>name_width$} \x1b[48;2;{bg_r};{bg_g};{bg_b}m");
        for (_, color) in ramp {
            let Color::Rgb(r, g, b) = color else {
                unreachable!()
            };
            print!("\x1b[38;2;{r};{g};{b}m{}", TEXT[i % TEXT.len()]);
            i += 1;
        }
        println!("\x1b[0m");
    }
    println!();

    let name_width = roles.iter().map(|(name, _)| name.len()).max().unwrap() + 1;

    print!("{:<name_width$} ", "bg\\fg");
    for (fg_name, _) in roles {
        print!("{fg_name:<name_width$}");
    }

    println!();

    for (bg_name, bg) in roles {
        let Color::Rgb(bg_r, bg_g, bg_b) = bg else {
            unreachable!()
        };
        print!("{bg_name:<name_width$} \x1b[48;2;{bg_r};{bg_g};{bg_b}m");
        for (fg_name, fg) in roles {
            let Color::Rgb(fg_r, fg_g, fg_b) = fg else {
                unreachable!()
            };
            print!("\x1b[38;2;{fg_r};{fg_g};{fg_b}m{fg_name:<name_width$}");
        }
        println!("\x1b[0m");
    }
}
