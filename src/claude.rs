use anyhow::Context as _;
use jiff::Timestamp;
use serde::{Deserialize, Serialize};
use std::{env, sync::LazyLock};

pub const BASE_URL: &str = "https://api.anthropic.com";

pub fn api_key() -> anyhow::Result<&'static str> {
    static API_KEY: LazyLock<Result<String, env::VarError>> =
        LazyLock::new(|| env::var("ANTHROPIC_API_KEY"));
    API_KEY.as_deref().context("Missing `ANTHROPIC_API_KEY`")
}

pub fn client() -> &'static reqwest::Client {
    static CLIENT: LazyLock<reqwest::Client> = LazyLock::new(reqwest::Client::new);
    &CLIENT
}

// Models API

#[derive(Debug, Deserialize)]
pub struct ModelInfo {
    pub id: String,
    pub created_at: Timestamp,
    pub display_name: String,
    /// Always "model"
    pub r#type: String,
}

/// <https://platform.claude.com/docs/en/api/models/retrieve>
pub async fn retrieve_model(model_id: &str) -> anyhow::Result<ModelInfo> {
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
    anyhow::ensure!(model_info.r#type == "model");
    Ok(model_info)
}

// Messages API

#[derive(Debug, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum Role {
    User,
    Assistant,
}

/// Message content: either a plain string or an array of content blocks.
#[derive(Debug, Serialize, Deserialize)]
#[serde(untagged)]
pub enum Content {
    Text(String),
    Blocks(Vec<ContentBlock>),
}

#[derive(Debug, Serialize, Deserialize)]
pub enum CacheControlTtl {
    #[serde(rename = "5m")]
    FiveMinutes,
    #[serde(rename = "1h")]
    OneHour,
}

#[derive(Debug, Serialize, Deserialize)]
#[serde(tag = "type", rename_all = "snake_case")]
pub enum CacheControl {
    Ephemeral {
        #[serde(default, skip_serializing_if = "Option::is_none")]
        ttl: Option<CacheControlTtl>,
    },
}

#[derive(Debug, Serialize, Deserialize)]
#[serde(tag = "type", rename_all = "snake_case")]
pub enum ImageSource {
    Base64 { data: String, media_type: String },
    Url { url: String },
}

/// Tool result content: either a plain string or an array of content blocks.
#[derive(Debug, Serialize, Deserialize)]
#[serde(untagged)]
pub enum ToolResultContent {
    Text(String),
    Blocks(Vec<ContentBlock>),
}

#[derive(Debug, Serialize)]
pub struct MessageParam {
    pub role: Role,
    pub content: Content,
}

/// Unified content block type used for both input and output.
///
/// The API's input and output content block types overlap heavily. Using one
/// enum with both `Serialize` and `Deserialize` lets us round-trip assistant
/// content blocks back into the next request without conversion.
#[derive(Debug, Serialize, Deserialize)]
#[serde(tag = "type", rename_all = "snake_case")]
pub enum ContentBlock {
    Text {
        text: String,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        cache_control: Option<CacheControl>,
    },
    Thinking {
        thinking: String,
        signature: String,
    },
    RedactedThinking {
        data: String,
    },
    Image {
        source: ImageSource,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        cache_control: Option<CacheControl>,
    },
    ToolUse {
        id: String,
        name: String,
        input: serde_json::Value,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        cache_control: Option<CacheControl>,
    },
    ToolResult {
        tool_use_id: String,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        content: Option<ToolResultContent>,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        is_error: Option<bool>,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        cache_control: Option<CacheControl>,
    },
}

#[derive(Debug, Serialize)]
#[serde(untagged)]
pub enum SystemPrompt {
    Text(String),
    Blocks(Vec<SystemBlock>),
}

#[derive(Debug, Serialize)]
#[serde(tag = "type", rename_all = "snake_case")]
pub enum SystemBlock {
    Text {
        text: String,
        #[serde(skip_serializing_if = "Option::is_none")]
        cache_control: Option<CacheControl>,
    },
}

#[derive(Debug, Serialize)]
pub struct MessageCreateParams {
    pub model: String,
    pub max_tokens: usize,
    pub messages: Vec<MessageParam>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub system: Option<SystemPrompt>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub stop_sequences: Option<Vec<String>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub stream: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub temperature: Option<f64>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub thinking: Option<ThinkingConfig>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub tool_choice: Option<ToolChoice>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub tools: Option<Vec<Tool>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub output_config: Option<OutputConfig>,
}

#[derive(Debug, Serialize)]
#[serde(tag = "type", rename_all = "snake_case")]
pub enum ThinkingConfig {
    Enabled { budget_tokens: usize },
    Disabled,
    Adaptive,
}

#[derive(Debug, Serialize)]
pub struct OutputConfig {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub effort: Option<Effort>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub format: Option<OutputFormat>,
}

#[derive(Debug, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum Effort {
    Low,
    Medium,
    High,
    Max,
}

#[derive(Debug, Serialize)]
#[serde(tag = "type")]
pub enum OutputFormat {
    #[serde(rename = "json_schema")]
    JsonSchema { schema: serde_json::Value },
}

#[derive(Debug, Serialize)]
pub struct Tool {
    pub name: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub description: Option<String>,
    pub input_schema: serde_json::Value,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub cache_control: Option<CacheControl>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub strict: Option<bool>,
}

#[derive(Debug, Serialize)]
#[serde(tag = "type", rename_all = "snake_case")]
pub enum ToolChoice {
    Auto {
        #[serde(skip_serializing_if = "Option::is_none")]
        disable_parallel_tool_use: Option<bool>,
    },
    Any {
        #[serde(skip_serializing_if = "Option::is_none")]
        disable_parallel_tool_use: Option<bool>,
    },
    Tool {
        name: String,
        #[serde(skip_serializing_if = "Option::is_none")]
        disable_parallel_tool_use: Option<bool>,
    },
    None,
}

#[derive(Debug, Deserialize)]
pub struct Message {
    pub id: String,
    pub content: Vec<ContentBlock>,
    pub model: String,
    pub role: Role,
    pub stop_reason: Option<StopReason>,
    pub stop_sequence: Option<String>,
    pub r#type: String,
    pub usage: Usage,
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum StopReason {
    EndTurn,
    MaxTokens,
    StopSequence,
    ToolUse,
    PauseTurn,
    Refusal,
}

#[derive(Debug, Deserialize)]
#[expect(clippy::struct_field_names)]
pub struct Usage {
    pub input_tokens: usize,
    pub output_tokens: usize,
    #[serde(default)]
    pub cache_creation_input_tokens: Option<usize>,
    #[serde(default)]
    pub cache_read_input_tokens: Option<usize>,
}

/// <https://platform.claude.com/docs/en/api/messages/create>
pub async fn create_message(params: MessageCreateParams) -> anyhow::Result<Message> {
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
