use anyhow::Context as _;
use jiff::Timestamp;
use serde::{Deserialize, Serialize};
use std::env;

pub const BASE_URL: &str = "https://api.anthropic.com";

#[derive(Clone)]
pub struct Client {
    http_client: reqwest::Client,
    api_key: String,
}

impl Client {
    pub fn from_env() -> anyhow::Result<Self> {
        let http_client = reqwest::Client::new();
        let api_key = env::var("ANTHROPIC_API_KEY").context("Missing `ANTHROPIC_API_KEY`")?;
        Ok(Self {
            http_client,
            api_key,
        })
    }

    /// <https://platform.claude.com/docs/en/api/models/retrieve>
    pub async fn retrieve_model(&self, model_id: &str) -> anyhow::Result<ModelInfo> {
        let response = self
            .http_client
            .get(format!("{BASE_URL}/v1/models/{model_id}"))
            .header("anthropic-version", "2023-06-01")
            .header("X-Api-Key", &self.api_key)
            .send()
            .await?;
        if !response.status().is_success() {
            let status = response.status();
            let text = response.text().await?;
            anyhow::bail!("failed\nstatus: {status}\ntext: {text}");
        }
        let text = response.text().await?;
        let de = &mut serde_json::Deserializer::from_str(&text);
        let model_info: ModelInfo = serde_path_to_error::deserialize(de)
            .with_context(|| format!("failed to parse response:\n{text}"))?;
        anyhow::ensure!(model_info.r#type == "model");
        Ok(model_info)
    }

    /// <https://platform.claude.com/docs/en/api/messages/create>
    pub async fn create_message(&self, params: MessageCreateParams) -> anyhow::Result<Message> {
        let response = self
            .http_client
            .post(format!("{BASE_URL}/v1/messages"))
            .header("anthropic-version", "2023-06-01")
            .header("X-Api-Key", &self.api_key)
            .json(&params)
            .send()
            .await?;
        if !response.status().is_success() {
            let status = response.status();
            let text = response.text().await?;
            anyhow::bail!("failed\nstatus: {status}\ntext: {text}");
        }
        let text = response.text().await?;
        let de = &mut serde_json::Deserializer::from_str(&text);
        let message: Message = serde_path_to_error::deserialize(de)
            .with_context(|| format!("failed to parse response:\n{text}"))?;
        Ok(message)
    }
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

// Messages API

#[derive(Debug, Deserialize, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum Role {
    User,
    Assistant,
}

/// Message content: either a plain string or an array of content blocks.
#[derive(Debug, Deserialize, Serialize)]
#[serde(untagged)]
pub enum Content {
    Text(String),
    Blocks(Vec<ContentBlock>),
}

#[derive(Debug, Deserialize, Serialize)]
pub enum CacheControlTtl {
    #[serde(rename = "5m")]
    FiveMinutes,
    #[serde(rename = "1h")]
    OneHour,
}

#[serde_with::skip_serializing_none]
#[derive(Debug, Deserialize, Serialize)]
#[serde(tag = "type", rename_all = "snake_case")]
pub enum CacheControl {
    Ephemeral {
        #[serde(default)]
        ttl: Option<CacheControlTtl>,
    },
}

#[derive(Debug, Deserialize, Serialize)]
#[serde(tag = "type", rename_all = "snake_case")]
pub enum ImageSource {
    Base64 { data: String, media_type: String },
    Url { url: String },
}

/// Tool result content: either a plain string or an array of content blocks.
#[derive(Debug, Deserialize, Serialize)]
#[serde(untagged)]
pub enum ToolResultContent {
    Text(String),
    Blocks(Vec<ContentBlock>),
}

#[derive(bon::Builder, Debug, Serialize)]
pub struct MessageParam {
    pub role: Role,
    pub content: Content,
}

/// Unified content block type used for both input and output.
///
/// The API's input and output content block types overlap heavily. Using one
/// enum with both `Serialize` and `Deserialize` lets us round-trip assistant
/// content blocks back into the next request without conversion.
#[serde_with::skip_serializing_none]
#[derive(Debug, Deserialize, Serialize)]
#[serde(tag = "type", rename_all = "snake_case")]
pub enum ContentBlock {
    Text {
        text: String,
        #[serde(default)]
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
        #[serde(default)]
        cache_control: Option<CacheControl>,
    },
    ToolUse {
        id: String,
        name: String,
        input: serde_json::Value,
        #[serde(default)]
        cache_control: Option<CacheControl>,
    },
    ToolResult {
        tool_use_id: String,
        #[serde(default)]
        content: Option<ToolResultContent>,
        #[serde(default)]
        is_error: Option<bool>,
        #[serde(default)]
        cache_control: Option<CacheControl>,
    },
}

#[derive(Debug, Serialize)]
#[serde(untagged)]
pub enum SystemPrompt {
    Text(String),
    Blocks(Vec<SystemBlock>),
}

#[serde_with::skip_serializing_none]
#[derive(Debug, Serialize)]
#[serde(tag = "type", rename_all = "snake_case")]
pub enum SystemBlock {
    Text {
        text: String,
        cache_control: Option<CacheControl>,
    },
}

#[serde_with::skip_serializing_none]
#[derive(bon::Builder, Debug, Serialize)]
pub struct MessageCreateParams {
    pub model: String,
    pub max_tokens: usize,
    pub messages: Vec<MessageParam>,
    pub system: Option<SystemPrompt>,
    pub stop_sequences: Option<Vec<String>>,
    pub stream: Option<bool>,
    pub temperature: Option<f64>,
    pub thinking: Option<ThinkingConfig>,
    pub tool_choice: Option<ToolChoice>,
    pub tools: Option<Vec<Tool>>,
    pub output_config: Option<OutputConfig>,
}

#[derive(Debug, Serialize)]
#[serde(tag = "type", rename_all = "snake_case")]
pub enum ThinkingConfig {
    Enabled { budget_tokens: usize },
    Disabled,
    Adaptive,
}

#[serde_with::skip_serializing_none]
#[derive(bon::Builder, Debug, Serialize)]
pub struct OutputConfig {
    pub effort: Option<Effort>,
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

#[serde_with::skip_serializing_none]
#[derive(bon::Builder, Debug, Serialize)]
pub struct Tool {
    pub name: String,
    pub description: Option<String>,
    pub input_schema: serde_json::Value,
    pub cache_control: Option<CacheControl>,
    pub strict: Option<bool>,
}

#[serde_with::skip_serializing_none]
#[derive(Debug, Serialize)]
#[serde(tag = "type", rename_all = "snake_case")]
pub enum ToolChoice {
    Auto {
        disable_parallel_tool_use: Option<bool>,
    },
    Any {
        disable_parallel_tool_use: Option<bool>,
    },
    Tool {
        name: String,
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
pub struct Usage {
    pub input_tokens: usize,
    pub output_tokens: usize,
    #[serde(default)]
    pub cache_creation_input_tokens: Option<usize>,
    #[serde(default)]
    pub cache_read_input_tokens: Option<usize>,
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::LazyLock;

    static CLIENT: LazyLock<Client> = LazyLock::new(|| Client::from_env().unwrap());

    const MODEL: &str = "claude-haiku-4-5";

    #[test]
    fn serialize_message_create_params_minimal() {
        let params = MessageCreateParams {
            model: MODEL.to_owned(),
            max_tokens: 128,
            messages: vec![MessageParam {
                role: Role::User,
                content: Content::Text("Hello".to_owned()),
            }],
            system: None,
            stop_sequences: None,
            stream: None,
            temperature: None,
            thinking: None,
            tool_choice: None,
            tools: None,
            output_config: None,
        };
        let json = serde_json::to_value(&params).unwrap();
        assert_eq!(json["model"], MODEL);
        assert_eq!(json["max_tokens"], 128);
        assert!(json.get("system").is_none());
        assert!(json.get("stream").is_none());
        assert!(json.get("tools").is_none());
    }

    #[test]
    fn serialize_message_create_params_full() {
        let params = MessageCreateParams {
            model: MODEL.to_owned(),
            max_tokens: 4096,
            messages: vec![
                MessageParam {
                    role: Role::User,
                    content: Content::Blocks(vec![
                        ContentBlock::Text {
                            text: "Describe this image".to_owned(),
                            cache_control: Some(CacheControl::Ephemeral {
                                ttl: Some(CacheControlTtl::FiveMinutes),
                            }),
                        },
                        ContentBlock::Image {
                            source: ImageSource::Url {
                                url: "https://example.com/image.png".to_owned(),
                            },
                            cache_control: None,
                        },
                    ]),
                },
                MessageParam {
                    role: Role::Assistant,
                    content: Content::Blocks(vec![ContentBlock::Text {
                        text: "I see an image".to_owned(),
                        cache_control: None,
                    }]),
                },
                MessageParam {
                    role: Role::User,
                    content: Content::Blocks(vec![ContentBlock::ToolResult {
                        tool_use_id: "tool_123".to_owned(),
                        content: Some(ToolResultContent::Text("result data".to_owned())),
                        is_error: Some(false),
                        cache_control: Some(CacheControl::Ephemeral { ttl: None }),
                    }]),
                },
            ],
            system: Some(SystemPrompt::Blocks(vec![SystemBlock::Text {
                text: "You are helpful.".to_owned(),
                cache_control: Some(CacheControl::Ephemeral {
                    ttl: Some(CacheControlTtl::OneHour),
                }),
            }])),
            stop_sequences: Some(vec!["STOP".to_owned()]),
            stream: Some(false),
            temperature: Some(0.7),
            thinking: Some(ThinkingConfig::Enabled {
                budget_tokens: 1024,
            }),
            tool_choice: Some(ToolChoice::Auto {
                disable_parallel_tool_use: Some(true),
            }),
            tools: Some(vec![Tool {
                name: "get_weather".to_owned(),
                description: Some("Get current weather".to_owned()),
                input_schema: serde_json::json!({
                    "type": "object",
                    "properties": {
                        "location": { "type": "string" }
                    },
                    "required": ["location"]
                }),
                cache_control: Some(CacheControl::Ephemeral { ttl: None }),
                strict: Some(true),
            }]),
            output_config: Some(OutputConfig {
                effort: Some(Effort::High),
                format: None,
            }),
        };
        let json = serde_json::to_value(&params).unwrap();
        assert_eq!(json["model"], MODEL);
        assert_eq!(json["max_tokens"], 4096);
        assert_eq!(json["stop_sequences"][0], "STOP");
        assert_eq!(json["stream"], false);
        assert_eq!(json["temperature"], 0.7);
        assert_eq!(json["thinking"]["type"], "enabled");
        assert_eq!(json["thinking"]["budget_tokens"], 1024);
        assert_eq!(json["tool_choice"]["type"], "auto");
        assert_eq!(json["tool_choice"]["disable_parallel_tool_use"], true);
        assert_eq!(json["tools"][0]["name"], "get_weather");
        assert_eq!(json["tools"][0]["strict"], true);
        assert_eq!(json["tools"][0]["cache_control"]["type"], "ephemeral");
        assert_eq!(json["output_config"]["effort"], "high");
        assert_eq!(json["system"][0]["type"], "text");
        assert_eq!(json["system"][0]["cache_control"]["ttl"], "1h");
    }

    #[test]
    fn serialize_tool_choice_variants() {
        let auto = serde_json::to_value(ToolChoice::Auto {
            disable_parallel_tool_use: None,
        })
        .unwrap();
        assert_eq!(auto["type"], "auto");
        assert!(auto.get("disable_parallel_tool_use").is_none());

        let any = serde_json::to_value(ToolChoice::Any {
            disable_parallel_tool_use: Some(false),
        })
        .unwrap();
        assert_eq!(any["type"], "any");
        assert_eq!(any["disable_parallel_tool_use"], false);

        let tool = serde_json::to_value(ToolChoice::Tool {
            name: "my_tool".to_owned(),
            disable_parallel_tool_use: Some(true),
        })
        .unwrap();
        assert_eq!(tool["type"], "tool");
        assert_eq!(tool["name"], "my_tool");

        let none = serde_json::to_value(ToolChoice::None).unwrap();
        assert_eq!(none["type"], "none");
    }

    #[test]
    fn serialize_thinking_config_variants() {
        let enabled = serde_json::to_value(ThinkingConfig::Enabled {
            budget_tokens: 2048,
        })
        .unwrap();
        assert_eq!(enabled["type"], "enabled");
        assert_eq!(enabled["budget_tokens"], 2048);

        let disabled = serde_json::to_value(ThinkingConfig::Disabled).unwrap();
        assert_eq!(disabled["type"], "disabled");

        let adaptive = serde_json::to_value(ThinkingConfig::Adaptive).unwrap();
        assert_eq!(adaptive["type"], "adaptive");
    }

    #[test]
    fn serialize_effort_variants() {
        assert_eq!(serde_json::to_value(Effort::Low).unwrap(), "low");
        assert_eq!(serde_json::to_value(Effort::Medium).unwrap(), "medium");
        assert_eq!(serde_json::to_value(Effort::High).unwrap(), "high");
        assert_eq!(serde_json::to_value(Effort::Max).unwrap(), "max");
    }

    #[test]
    fn serialize_output_format_json_schema() {
        let format = OutputFormat::JsonSchema {
            schema: serde_json::json!({
                "type": "object",
                "properties": { "answer": { "type": "string" } }
            }),
        };
        let json = serde_json::to_value(format).unwrap();
        assert_eq!(json["type"], "json_schema");
        assert_eq!(json["schema"]["type"], "object");
    }

    #[test]
    fn serialize_image_source_base64() {
        let source = ImageSource::Base64 {
            data: "aGVsbG8=".to_owned(),
            media_type: "image/png".to_owned(),
        };
        let json = serde_json::to_value(&source).unwrap();
        assert_eq!(json["type"], "base64");
        assert_eq!(json["data"], "aGVsbG8=");
        assert_eq!(json["media_type"], "image/png");
    }

    #[test]
    fn serialize_tool_result_with_blocks() {
        let block = ContentBlock::ToolResult {
            tool_use_id: "tu_abc".to_owned(),
            content: Some(ToolResultContent::Blocks(vec![ContentBlock::Text {
                text: "block result".to_owned(),
                cache_control: None,
            }])),
            is_error: Some(true),
            cache_control: None,
        };
        let json = serde_json::to_value(&block).unwrap();
        assert_eq!(json["type"], "tool_result");
        assert_eq!(json["tool_use_id"], "tu_abc");
        assert_eq!(json["is_error"], true);
        assert_eq!(json["content"][0]["type"], "text");
    }

    #[test]
    fn serialize_system_prompt_text() {
        let system = SystemPrompt::Text("Be concise.".to_owned());
        let json = serde_json::to_value(&system).unwrap();
        assert_eq!(json, "Be concise.");
    }

    #[test]
    fn serialize_cache_control_ttl_variants() {
        let five = CacheControl::Ephemeral {
            ttl: Some(CacheControlTtl::FiveMinutes),
        };
        let json = serde_json::to_value(&five).unwrap();
        assert_eq!(json["type"], "ephemeral");
        assert_eq!(json["ttl"], "5m");

        let one_hour = CacheControl::Ephemeral {
            ttl: Some(CacheControlTtl::OneHour),
        };
        let json = serde_json::to_value(&one_hour).unwrap();
        assert_eq!(json["ttl"], "1h");

        let no_ttl = CacheControl::Ephemeral { ttl: None };
        let json = serde_json::to_value(&no_ttl).unwrap();
        assert!(json.get("ttl").is_none());
    }

    #[test]
    fn deserialize_message_response() {
        let json = serde_json::json!({
            "id": "msg_01234",
            "type": "message",
            "role": "assistant",
            "model": MODEL,
            "content": [
                {
                    "type": "text",
                    "text": "Hello!"
                }
            ],
            "stop_reason": "end_turn",
            "stop_sequence": null,
            "usage": {
                "input_tokens": 10,
                "output_tokens": 5,
                "cache_creation_input_tokens": 100,
                "cache_read_input_tokens": 50
            }
        });
        let msg: Message = serde_json::from_value(json).unwrap();
        assert_eq!(msg.id, "msg_01234");
        assert_eq!(msg.model, MODEL);
        assert!(matches!(msg.role, Role::Assistant));
        assert!(matches!(msg.stop_reason, Some(StopReason::EndTurn)));
        assert!(msg.stop_sequence.is_none());
        assert_eq!(msg.usage.input_tokens, 10);
        assert_eq!(msg.usage.output_tokens, 5);
        assert_eq!(msg.usage.cache_creation_input_tokens, Some(100));
        assert_eq!(msg.usage.cache_read_input_tokens, Some(50));
        assert!(matches!(&msg.content[0], ContentBlock::Text { text, .. } if text == "Hello!"));
    }

    #[test]
    fn deserialize_message_with_tool_use() {
        let json = serde_json::json!({
            "id": "msg_tool",
            "type": "message",
            "role": "assistant",
            "model": MODEL,
            "content": [
                {
                    "type": "text",
                    "text": "Let me check the weather."
                },
                {
                    "type": "tool_use",
                    "id": "toolu_abc123",
                    "name": "get_weather",
                    "input": { "location": "San Francisco" }
                }
            ],
            "stop_reason": "tool_use",
            "stop_sequence": null,
            "usage": { "input_tokens": 20, "output_tokens": 15 }
        });
        let msg: Message = serde_json::from_value(json).unwrap();
        assert!(matches!(msg.stop_reason, Some(StopReason::ToolUse)));
        assert_eq!(msg.content.len(), 2);
        match &msg.content[1] {
            ContentBlock::ToolUse {
                id, name, input, ..
            } => {
                assert_eq!(id, "toolu_abc123");
                assert_eq!(name, "get_weather");
                assert_eq!(input["location"], "San Francisco");
            }
            other => panic!("expected ToolUse, got {other:?}"),
        }
    }

    #[test]
    fn deserialize_message_with_thinking() {
        let json = serde_json::json!({
            "id": "msg_think",
            "type": "message",
            "role": "assistant",
            "model": MODEL,
            "content": [
                {
                    "type": "thinking",
                    "thinking": "Let me reason about this...",
                    "signature": "sig_abc"
                },
                {
                    "type": "redacted_thinking",
                    "data": "encrypted_data_here"
                },
                {
                    "type": "text",
                    "text": "Here is my answer."
                }
            ],
            "stop_reason": "end_turn",
            "stop_sequence": null,
            "usage": { "input_tokens": 30, "output_tokens": 25 }
        });
        let msg: Message = serde_json::from_value(json).unwrap();
        assert_eq!(msg.content.len(), 3);
        match &msg.content[0] {
            ContentBlock::Thinking {
                thinking,
                signature,
                ..
            } => {
                assert_eq!(thinking, "Let me reason about this...");
                assert_eq!(signature, "sig_abc");
            }
            other => panic!("expected Thinking, got {other:?}"),
        }
        assert!(
            matches!(&msg.content[1], ContentBlock::RedactedThinking { data } if data == "encrypted_data_here")
        );
    }

    #[test]
    fn deserialize_stop_reason_variants() {
        for (input, expected) in [
            ("end_turn", "EndTurn"),
            ("max_tokens", "MaxTokens"),
            ("stop_sequence", "StopSequence"),
            ("tool_use", "ToolUse"),
            ("pause_turn", "PauseTurn"),
            ("refusal", "Refusal"),
        ] {
            let json = serde_json::json!({
                "id": "msg_sr",
                "type": "message",
                "role": "assistant",
                "model": MODEL,
                "content": [],
                "stop_reason": input,
                "stop_sequence": null,
                "usage": { "input_tokens": 1, "output_tokens": 1 }
            });
            let msg: Message = serde_json::from_value(json).unwrap();
            let actual = format!("{:?}", msg.stop_reason.unwrap());
            assert_eq!(actual, expected, "failed for {input}");
        }
    }

    #[test]
    fn deserialize_usage_without_cache_fields() {
        let json = serde_json::json!({
            "id": "msg_no_cache",
            "type": "message",
            "role": "assistant",
            "model": MODEL,
            "content": [],
            "stop_reason": "end_turn",
            "stop_sequence": null,
            "usage": { "input_tokens": 5, "output_tokens": 3 }
        });
        let msg: Message = serde_json::from_value(json).unwrap();
        assert!(msg.usage.cache_creation_input_tokens.is_none());
        assert!(msg.usage.cache_read_input_tokens.is_none());
    }

    #[test]
    fn deserialize_model_info() {
        let json = serde_json::json!({
            "id": MODEL,
            "created_at": "2025-04-15T00:00:00Z",
            "display_name": "Claude Haiku 4.5",
            "type": "model"
        });
        let info: ModelInfo = serde_json::from_value(json).unwrap();
        assert_eq!(info.id, MODEL);
        assert_eq!(info.display_name, "Claude Haiku 4.5");
        assert_eq!(info.r#type, "model");
    }

    #[test]
    fn deserialize_content_text_string() {
        let json = serde_json::json!("just a string");
        let content: Content = serde_json::from_value(json).unwrap();
        assert!(matches!(content, Content::Text(s) if s == "just a string"));
    }

    #[test]
    fn deserialize_content_blocks() {
        let json = serde_json::json!([
            { "type": "text", "text": "hello" },
            { "type": "image", "source": { "type": "url", "url": "https://example.com/img.png" } }
        ]);
        let content: Content = serde_json::from_value(json).unwrap();
        #[expect(clippy::match_wildcard_for_single_variants)]
        match content {
            Content::Blocks(blocks) => {
                assert_eq!(blocks.len(), 2);
                assert!(matches!(&blocks[0], ContentBlock::Text { text, .. } if text == "hello"));
                assert!(
                    matches!(&blocks[1], ContentBlock::Image { source: ImageSource::Url { url }, .. } if url == "https://example.com/img.png")
                );
            }
            other => panic!("expected Blocks, got {other:?}"),
        }
    }

    #[test]
    fn deserialize_message_with_stop_sequence() {
        let json = serde_json::json!({
            "id": "msg_ss",
            "type": "message",
            "role": "assistant",
            "model": MODEL,
            "content": [{ "type": "text", "text": "partial STOP" }],
            "stop_reason": "stop_sequence",
            "stop_sequence": "STOP",
            "usage": { "input_tokens": 5, "output_tokens": 3 }
        });
        let msg: Message = serde_json::from_value(json).unwrap();
        assert!(matches!(msg.stop_reason, Some(StopReason::StopSequence)));
        assert_eq!(msg.stop_sequence, Some("STOP".to_owned()));
    }

    #[test]
    fn roundtrip_content_block_text() {
        let block = ContentBlock::Text {
            text: "roundtrip".to_owned(),
            cache_control: Some(CacheControl::Ephemeral {
                ttl: Some(CacheControlTtl::FiveMinutes),
            }),
        };
        let json = serde_json::to_value(&block).unwrap();
        let deserialized: ContentBlock = serde_json::from_value(json).unwrap();
        match deserialized {
            ContentBlock::Text {
                text,
                cache_control,
            } => {
                assert_eq!(text, "roundtrip");
                assert!(cache_control.is_some());
            }
            other => panic!("expected Text, got {other:?}"),
        }
    }

    #[test]
    fn roundtrip_content_block_tool_use() {
        let block = ContentBlock::ToolUse {
            id: "toolu_rt".to_owned(),
            name: "search".to_owned(),
            input: serde_json::json!({"query": "test"}),
            cache_control: None,
        };
        let json = serde_json::to_value(&block).unwrap();
        let deserialized: ContentBlock = serde_json::from_value(json).unwrap();
        match deserialized {
            ContentBlock::ToolUse {
                id, name, input, ..
            } => {
                assert_eq!(id, "toolu_rt");
                assert_eq!(name, "search");
                assert_eq!(input["query"], "test");
            }
            other => panic!("expected ToolUse, got {other:?}"),
        }
    }

    #[tokio::test]
    #[ignore = "live API test; run with --ignored --test-threads=1"]
    async fn retrieve_model_live() {
        let info = CLIENT.retrieve_model(MODEL).await.unwrap();
        assert!(info.id.starts_with("claude-haiku-4-5"));
        assert_eq!(info.r#type, "model");
        assert!(!info.display_name.is_empty());
    }

    #[tokio::test]
    #[ignore = "live API test; run with --ignored --test-threads=1"]
    async fn create_message_live_simple() {
        let params = MessageCreateParams {
            model: MODEL.to_owned(),
            max_tokens: 64,
            messages: vec![MessageParam {
                role: Role::User,
                content: Content::Text("Reply with only the word 'pong'.".to_owned()),
            }],
            system: Some(SystemPrompt::Text("You are a test bot.".to_owned())),
            stop_sequences: None,
            stream: Some(false),
            temperature: Some(0.0),
            thinking: Some(ThinkingConfig::Disabled),
            tool_choice: None,
            tools: None,
            output_config: None,
        };
        let msg = CLIENT.create_message(params).await.unwrap();
        assert!(matches!(msg.stop_reason, Some(StopReason::EndTurn)));
        assert!(!msg.content.is_empty());
        assert!(msg.usage.input_tokens > 0);
        assert!(msg.usage.output_tokens > 0);
    }

    #[tokio::test]
    #[ignore = "live API test; run with --ignored --test-threads=1"]
    async fn create_message_live_with_tools() {
        let params = MessageCreateParams {
            model: MODEL.to_owned(),
            max_tokens: 256,
            messages: vec![MessageParam {
                role: Role::User,
                content: Content::Text("What's the weather in Tokyo?".to_owned()),
            }],
            system: None,
            stop_sequences: None,
            stream: Some(false),
            temperature: Some(0.0),
            thinking: None,
            tool_choice: Some(ToolChoice::Any {
                disable_parallel_tool_use: Some(true),
            }),
            tools: Some(vec![Tool {
                name: "get_weather".to_owned(),
                description: Some("Get weather for a location".to_owned()),
                input_schema: serde_json::json!({
                    "type": "object",
                    "properties": {
                        "location": {
                            "type": "string",
                            "description": "City name"
                        }
                    },
                    "required": ["location"]
                }),
                cache_control: None,
                strict: None,
            }]),
            output_config: None,
        };
        let msg = CLIENT.create_message(params).await.unwrap();
        assert!(matches!(msg.stop_reason, Some(StopReason::ToolUse)));
        let has_tool_use = msg
            .content
            .iter()
            .any(|b| matches!(b, ContentBlock::ToolUse { .. }));
        assert!(has_tool_use, "expected a tool_use block in response");
    }
}
