use indigo_ui::two::{App, View, button};

fn example(count: &mut i32) -> impl View<i32> + use<> {
    button(format!("click me! count = {count}"), |count| *count += 1)
}

fn main() {
    App::new(0, example).run();
}
