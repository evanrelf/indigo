use anyhow::Context;
use clap::Parser as _;
use jiff::Timestamp;
use serde::Deserialize;
use std::{env, sync::LazyLock};

#[derive(clap::Parser)]
struct Args {}

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    let _args = Args::parse();

    let model_info = claude::get_model("claude-sonnet-4-20250514").await?;

    println!("{model_info:?}");

    Ok(())
}

mod claude {
    use super::*;

    pub const BASE_URL: &str = "https://api.anthropic.com";

    pub fn api_key() -> anyhow::Result<&'static str> {
        static API_KEY: LazyLock<Result<String, env::VarError>> =
            LazyLock::new(|| env::var("ANTHROPIC_API_KEY"));
        Ok(LazyLock::force(&API_KEY)
            .as_ref()
            .context("Missing `ANTHROPIC_API_KEY`")?)
    }

    pub fn client() -> &'static reqwest::Client {
        static CLIENT: LazyLock<reqwest::Client> = LazyLock::new(|| reqwest::Client::new());
        LazyLock::force(&CLIENT)
    }

    /// <https://platform.claude.com/docs/en/api/models/retrieve>
    #[derive(Debug, Deserialize)]
    pub struct ModelInfo {
        pub id: String,
        pub created_at: Timestamp,
        pub display_name: String,
        /// Always "model"
        pub r#type: String,
    }

    /// <https://platform.claude.com/docs/en/api/models/retrieve>
    pub async fn get_model(model_id: &str) -> anyhow::Result<ModelInfo> {
        let response = client()
            .get(format!("{BASE_URL}/v1/models/{model_id}"))
            .header("anthropic-version", "2023-06-01")
            .header("X-Api-Key", api_key()?)
            .send()
            .await?;
        let model_info: ModelInfo = response.json().await?;
        assert_eq!(model_info.r#type, "model");
        Ok(model_info)
    }
}
