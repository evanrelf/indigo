#![expect(dead_code)]

use anyhow::Context;
use clap::Parser as _;
use jiff::Timestamp;
use serde::{Deserialize, Serialize};
use std::{env, sync::LazyLock};

#[derive(clap::Parser)]
struct Args {}

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    let _args = Args::parse();

    Ok(())
}

async fn claude_demo() -> anyhow::Result<()> {
    let model_id = "claude-haiku-4-5";

    // let model_info = claude::models_retrieve(model_id).await?;

    // println!("model info:\n{model_info:?}\n");

    let messages_create_params = claude::MessageCreateParams {
        max_tokens: 1024,
        messages: vec![
            claude::MessageParam {
                content: String::from("Hello, Claude!"),
                role: String::from("user"),
            },
            claude::MessageParam {
                content: String::from("Hello, Evan!"),
                role: String::from("assistant"),
            },
        ],
        model: String::from(model_id),
        // system: None,
        // temperature: None,
    };

    println!(
        "request:\n{}\n",
        serde_json::to_string(&messages_create_params)?
    );

    let message = claude::messages_create(messages_create_params).await?;

    println!("response:\n{message:?}\n");

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

    #[derive(Debug, Deserialize)]
    pub struct ModelInfo {
        pub id: String,
        pub created_at: Timestamp,
        pub display_name: String,
        /// Always "model"
        pub r#type: String,
    }

    /// <https://platform.claude.com/docs/en/api/models/retrieve>
    pub async fn models_retrieve(model_id: &str) -> anyhow::Result<ModelInfo> {
        let response = client()
            .get(format!("{BASE_URL}/v1/models/{model_id}"))
            .header("anthropic-version", "2023-06-01")
            .header("X-Api-Key", api_key()?)
            .send()
            .await?;
        if !response.status().is_success() {
            let status = response.status();
            let text = response.text().await?;
            anyhow::bail!("failed\nstatus: {status}\ntext: {text}");
        }
        let model_info: ModelInfo = response.json().await?;
        assert_eq!(model_info.r#type, "model");
        Ok(model_info)
    }

    #[derive(Debug, Serialize)]
    pub struct MessageParam {
        pub content: String,
        /// One of "user" or "assistant"
        pub role: String,
    }

    // #[derive(Serialize)]
    // pub struct Tool {}

    #[derive(Debug, Serialize)]
    pub struct MessageCreateParams {
        pub max_tokens: usize,
        pub messages: Vec<MessageParam>,
        pub model: String,
        // pub system: Option<String>,
        // pub temperature: Option<f32>,
        // pub tools: Vec<Tool>,
    }

    #[derive(Debug, Deserialize)]
    pub struct TextBlock {
        // citations: Vec<()>,
        text: String,
        /// Always "text"
        r#type: String,
    }

    // #[derive(Debug, Deserialize)]
    // pub struct ToolUseBlock {
    //     // id: String,
    //     // input: (),
    //     // name: String,
    //     /// Always "tool_use"
    //     #[expect(clippy::doc_markdown)]
    //     r#type: String,
    // }

    #[derive(Debug, Deserialize)]
    pub enum ContentBlock {
        TextBlock(TextBlock),
        // ToolUseBlock(ToolUseBlock),
    }

    #[derive(Debug, Deserialize)]
    pub struct Message {
        pub id: String,
        pub content: Vec<ContentBlock>,
        pub model: String,
        /// Always "assistant"
        pub role: String,
    }

    /// <https://platform.claude.com/docs/en/api/messages/create>
    pub async fn messages_create(params: MessageCreateParams) -> anyhow::Result<Message> {
        let response = client()
            .post(format!("{BASE_URL}/v1/messages"))
            .header("anthropic-version", "2023-06-01")
            .header("X-Api-Key", api_key()?)
            .json(&params)
            .send()
            .await?;
        if !response.status().is_success() {
            let status = response.status();
            let text = response.text().await?;
            anyhow::bail!("failed\nstatus: {status}\ntext: {text}");
        }
        let message: Message = response.json().await?;
        Ok(message)
    }
}
