#[tokio::main]
async fn main() {
    indigo_event::event::run().await;
}
