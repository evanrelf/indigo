use indigo_ui::{
    App,
    view::{Context, text},
};
use xilem_core::View;

fn logic(name: &mut String) -> impl View<String, (), Context> + use<> {
    text(format!("Hello, {name}!"))
}

fn main() -> anyhow::Result<()> {
    let state = String::from("world");
    let app = App::new(state, logic);
    app.run()?;
    Ok(())
}
