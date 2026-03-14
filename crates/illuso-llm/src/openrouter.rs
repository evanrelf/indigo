use anyhow::Context as _;
use serde::{Deserialize, Serialize};
use std::{env, sync::LazyLock};

pub const BASE_URL: &str = "https://openrouter.ai";

pub fn api_key() -> anyhow::Result<&'static str> {
    static API_KEY: LazyLock<Result<String, env::VarError>> =
        LazyLock::new(|| env::var("OPENROUTER_API_KEY"));
    API_KEY.as_deref().context("Missing `OPENROUTER_API_KEY`")
}

#[must_use]
pub fn client() -> &'static reqwest::Client {
    static CLIENT: LazyLock<reqwest::Client> = LazyLock::new(reqwest::Client::new);
    &CLIENT
}

// Content types

#[derive(Debug, Deserialize, Serialize)]
#[serde(untagged)]
pub enum Content {
    Text(String),
    Parts(Vec<ContentPart>),
}

#[derive(Debug, Deserialize, Serialize)]
#[serde(tag = "type", rename_all = "snake_case")]
pub enum ContentPart {
    Text { text: String },
    ImageUrl { image_url: ImageUrl },
}

#[serde_with::skip_serializing_none]
#[derive(Debug, Deserialize, Serialize)]
pub struct ImageUrl {
    pub url: String,
    #[serde(default)]
    pub detail: Option<String>,
}

// Messages

#[serde_with::skip_serializing_none]
#[derive(Debug, Deserialize, Serialize)]
#[serde(tag = "role", rename_all = "snake_case")]
pub enum Message {
    System {
        content: Content,
        #[serde(default)]
        name: Option<String>,
    },
    Developer {
        content: Content,
        #[serde(default)]
        name: Option<String>,
    },
    User {
        content: Content,
        #[serde(default)]
        name: Option<String>,
    },
    Assistant {
        #[serde(default)]
        content: Option<Content>,
        #[serde(default)]
        tool_calls: Option<Vec<ToolCall>>,
    },
    Tool {
        content: Content,
        tool_call_id: String,
    },
}

// Stop

#[derive(Debug, Deserialize, Serialize)]
#[serde(untagged)]
pub enum Stop {
    String(String),
    Array(Vec<String>),
}

// Tools

#[derive(Debug, Serialize)]
pub struct ToolDefinition {
    r#type: ToolType,
    pub function: FunctionDefinition,
}

impl ToolDefinition {
    #[must_use]
    pub fn function(f: FunctionDefinition) -> Self {
        Self {
            r#type: ToolType::Function,
            function: f,
        }
    }
}

#[derive(Debug, Serialize)]
#[serde(rename_all = "snake_case")]
enum ToolType {
    Function,
}

#[serde_with::skip_serializing_none]
#[derive(bon::Builder, Debug, Serialize)]
pub struct FunctionDefinition {
    pub name: String,
    pub description: Option<String>,
    pub parameters: Option<serde_json::Value>,
    pub strict: Option<bool>,
}

#[derive(Debug, Serialize)]
#[serde(untagged)]
pub enum ToolChoice {
    Mode(ToolChoiceMode),
    Named(NamedToolChoice),
}

#[derive(Debug, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum ToolChoiceMode {
    None,
    Auto,
    Required,
}

#[derive(Debug, Serialize)]
pub struct NamedToolChoice {
    pub r#type: NamedToolChoiceType,
    pub function: NamedToolChoiceFunction,
}

#[derive(Debug, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum NamedToolChoiceType {
    Function,
}

#[derive(Debug, Serialize)]
pub struct NamedToolChoiceFunction {
    pub name: String,
}

#[derive(Debug, Deserialize, Serialize)]
pub struct ToolCall {
    pub id: String,
    pub r#type: String,
    pub function: ToolCallFunction,
}

#[derive(Debug, Deserialize, Serialize)]
pub struct ToolCallFunction {
    pub name: String,
    pub arguments: String,
}

// Response format

#[derive(Debug, Serialize)]
#[serde(tag = "type", rename_all = "snake_case")]
pub enum ResponseFormat {
    Text,
    JsonObject,
    JsonSchema { json_schema: JsonSchemaFormat },
}

#[serde_with::skip_serializing_none]
#[derive(Debug, Serialize)]
pub struct JsonSchemaFormat {
    pub name: String,
    pub schema: serde_json::Value,
    pub strict: Option<bool>,
}

// Reasoning

#[derive(Debug, Serialize)]
pub struct ReasoningConfig {
    pub effort: ReasoningEffort,
}

#[derive(Debug, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum ReasoningEffort {
    #[serde(rename = "xhigh")]
    XHigh,
    High,
    Medium,
    Low,
    Minimal,
    #[serde(rename = "none")]
    None,
}

// Request

#[serde_with::skip_serializing_none]
#[derive(bon::Builder, Debug, Serialize)]
pub struct ChatCompletionParams {
    pub messages: Vec<Message>,
    pub model: Option<String>,
    pub max_tokens: Option<usize>,
    pub temperature: Option<f64>,
    pub stop: Option<Stop>,
    pub stream: Option<bool>,
    pub tools: Option<Vec<ToolDefinition>>,
    pub tool_choice: Option<ToolChoice>,
    pub response_format: Option<ResponseFormat>,
    pub reasoning: Option<ReasoningConfig>,
}

// Response

#[derive(Debug, Deserialize)]
pub struct ChatCompletion {
    pub id: String,
    pub choices: Vec<Choice>,
    pub created: Option<u64>,
    pub model: String,
    pub object: String,
    #[serde(default)]
    pub usage: Option<Usage>,
    #[serde(default)]
    pub system_fingerprint: Option<String>,
}

#[derive(Debug, Deserialize)]
pub struct Choice {
    pub finish_reason: Option<FinishReason>,
    pub index: usize,
    pub message: AssistantMessage,
}

#[derive(Debug, Deserialize)]
pub struct AssistantMessage {
    pub role: String,
    pub content: Option<String>,
    #[serde(default)]
    pub tool_calls: Option<Vec<ToolCall>>,
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum FinishReason {
    Stop,
    Length,
    ToolCalls,
    ContentFilter,
    Error,
}

#[derive(Debug, Deserialize)]
pub struct Usage {
    pub prompt_tokens: usize,
    pub completion_tokens: usize,
    pub total_tokens: usize,
}

// Models

#[derive(Debug, Deserialize)]
pub struct Model {
    pub id: String,
    pub name: String,
    pub created: Option<u64>,
    pub context_length: Option<usize>,
}

#[derive(Debug, Deserialize)]
pub struct ModelsResponse {
    pub data: Vec<Model>,
}

// API functions

pub async fn list_models() -> anyhow::Result<Vec<Model>> {
    let response = client()
        .get(format!("{BASE_URL}/api/v1/models"))
        .bearer_auth(api_key()?)
        .send()
        .await?;
    if !response.status().is_success() {
        let status = response.status();
        let text = response.text().await?;
        anyhow::bail!("failed\nstatus: {status}\ntext: {text}");
    }
    let text = response.text().await?;
    let de = &mut serde_json::Deserializer::from_str(&text);
    let models_response: ModelsResponse = serde_path_to_error::deserialize(de)
        .with_context(|| format!("failed to parse response:\n{text}"))?;
    Ok(models_response.data)
}

pub async fn create_chat_completion(
    params: ChatCompletionParams,
) -> anyhow::Result<ChatCompletion> {
    let response = client()
        .post(format!("{BASE_URL}/api/v1/chat/completions"))
        .bearer_auth(api_key()?)
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
    let completion: ChatCompletion = serde_path_to_error::deserialize(de)
        .with_context(|| format!("failed to parse response:\n{text}"))?;
    Ok(completion)
}

#[cfg(test)]
mod tests {
    use super::*;

    const MODEL: &str = "anthropic/claude-haiku-4.5";

    // Serialization tests

    #[test]
    fn serialize_chat_completion_params_minimal() {
        let params = ChatCompletionParams {
            messages: vec![Message::User {
                content: Content::Text("Hello".to_owned()),
                name: None,
            }],
            model: None,
            max_tokens: None,
            temperature: None,
            stop: None,
            stream: None,
            tools: None,
            tool_choice: None,
            response_format: None,
            reasoning: None,
        };
        let json = serde_json::to_value(&params).unwrap();
        assert_eq!(json["messages"][0]["role"], "user");
        assert_eq!(json["messages"][0]["content"], "Hello");
        assert!(json.get("model").is_none());
        assert!(json.get("max_tokens").is_none());
        assert!(json.get("stream").is_none());
        assert!(json.get("tools").is_none());
    }

    #[test]
    fn serialize_chat_completion_params_full() {
        let params = ChatCompletionParams {
            messages: vec![
                Message::System {
                    content: Content::Text("You are helpful.".to_owned()),
                    name: None,
                },
                Message::User {
                    content: Content::Text("Hi".to_owned()),
                    name: Some("alice".to_owned()),
                },
            ],
            model: Some(MODEL.to_owned()),
            max_tokens: Some(4096),
            temperature: Some(0.7),
            stop: Some(Stop::String("STOP".to_owned())),
            stream: Some(false),
            tools: Some(vec![ToolDefinition::function(FunctionDefinition {
                name: "get_weather".to_owned(),
                description: Some("Get current weather".to_owned()),
                parameters: Some(serde_json::json!({
                    "type": "object",
                    "properties": {
                        "location": { "type": "string" }
                    },
                    "required": ["location"]
                })),
                strict: Some(true),
            })]),
            tool_choice: Some(ToolChoice::Mode(ToolChoiceMode::Auto)),
            response_format: Some(ResponseFormat::Text),
            reasoning: Some(ReasoningConfig {
                effort: ReasoningEffort::High,
            }),
        };
        let json = serde_json::to_value(&params).unwrap();
        assert_eq!(json["model"], MODEL);
        assert_eq!(json["max_tokens"], 4096);
        assert_eq!(json["temperature"], 0.7);
        assert_eq!(json["stop"], "STOP");
        assert_eq!(json["stream"], false);
        assert_eq!(json["tools"][0]["type"], "function");
        assert_eq!(json["tools"][0]["function"]["name"], "get_weather");
        assert_eq!(json["tools"][0]["function"]["strict"], true);
        assert_eq!(json["tool_choice"], "auto");
        assert_eq!(json["response_format"]["type"], "text");
        assert_eq!(json["reasoning"]["effort"], "high");
        assert_eq!(json["messages"][1]["name"], "alice");
    }

    #[test]
    fn serialize_message_variants() {
        let system = serde_json::to_value(Message::System {
            content: Content::Text("sys".to_owned()),
            name: None,
        })
        .unwrap();
        assert_eq!(system["role"], "system");

        let developer = serde_json::to_value(Message::Developer {
            content: Content::Text("dev".to_owned()),
            name: None,
        })
        .unwrap();
        assert_eq!(developer["role"], "developer");

        let user = serde_json::to_value(Message::User {
            content: Content::Text("usr".to_owned()),
            name: None,
        })
        .unwrap();
        assert_eq!(user["role"], "user");

        let assistant = serde_json::to_value(Message::Assistant {
            content: Some(Content::Text("asst".to_owned())),
            tool_calls: None,
        })
        .unwrap();
        assert_eq!(assistant["role"], "assistant");

        let tool = serde_json::to_value(Message::Tool {
            content: Content::Text("result".to_owned()),
            tool_call_id: "call_123".to_owned(),
        })
        .unwrap();
        assert_eq!(tool["role"], "tool");
        assert_eq!(tool["tool_call_id"], "call_123");
    }

    #[test]
    fn serialize_tool_choice_variants() {
        let none = serde_json::to_value(ToolChoice::Mode(ToolChoiceMode::None)).unwrap();
        assert_eq!(none, "none");

        let auto = serde_json::to_value(ToolChoice::Mode(ToolChoiceMode::Auto)).unwrap();
        assert_eq!(auto, "auto");

        let required = serde_json::to_value(ToolChoice::Mode(ToolChoiceMode::Required)).unwrap();
        assert_eq!(required, "required");

        let named = serde_json::to_value(ToolChoice::Named(NamedToolChoice {
            r#type: NamedToolChoiceType::Function,
            function: NamedToolChoiceFunction {
                name: "my_func".to_owned(),
            },
        }))
        .unwrap();
        assert_eq!(named["type"], "function");
        assert_eq!(named["function"]["name"], "my_func");
    }

    #[test]
    fn serialize_tool_definition() {
        let tool = ToolDefinition::function(FunctionDefinition {
            name: "search".to_owned(),
            description: Some("Search the web".to_owned()),
            parameters: Some(serde_json::json!({"type": "object"})),
            strict: None,
        });
        let json = serde_json::to_value(&tool).unwrap();
        assert_eq!(json["type"], "function");
        assert_eq!(json["function"]["name"], "search");
        assert_eq!(json["function"]["description"], "Search the web");
        assert_eq!(json["function"]["parameters"]["type"], "object");
        assert!(json["function"].get("strict").is_none());
    }

    #[test]
    fn serialize_response_format_variants() {
        let text = serde_json::to_value(ResponseFormat::Text).unwrap();
        assert_eq!(text["type"], "text");

        let json_obj = serde_json::to_value(ResponseFormat::JsonObject).unwrap();
        assert_eq!(json_obj["type"], "json_object");

        let json_schema = serde_json::to_value(ResponseFormat::JsonSchema {
            json_schema: JsonSchemaFormat {
                name: "my_schema".to_owned(),
                schema: serde_json::json!({"type": "object"}),
                strict: Some(true),
            },
        })
        .unwrap();
        assert_eq!(json_schema["type"], "json_schema");
        assert_eq!(json_schema["json_schema"]["name"], "my_schema");
        assert_eq!(json_schema["json_schema"]["strict"], true);
    }

    #[test]
    fn serialize_reasoning_effort_variants() {
        assert_eq!(
            serde_json::to_value(ReasoningEffort::XHigh).unwrap(),
            "xhigh"
        );
        assert_eq!(serde_json::to_value(ReasoningEffort::High).unwrap(), "high");
        assert_eq!(
            serde_json::to_value(ReasoningEffort::Medium).unwrap(),
            "medium"
        );
        assert_eq!(serde_json::to_value(ReasoningEffort::Low).unwrap(), "low");
        assert_eq!(
            serde_json::to_value(ReasoningEffort::Minimal).unwrap(),
            "minimal"
        );
        assert_eq!(serde_json::to_value(ReasoningEffort::None).unwrap(), "none");
    }

    #[test]
    fn serialize_content_parts() {
        let parts = Content::Parts(vec![
            ContentPart::Text {
                text: "Look at this".to_owned(),
            },
            ContentPart::ImageUrl {
                image_url: ImageUrl {
                    url: "https://example.com/img.png".to_owned(),
                    detail: Some("high".to_owned()),
                },
            },
        ]);
        let json = serde_json::to_value(&parts).unwrap();
        assert_eq!(json[0]["type"], "text");
        assert_eq!(json[0]["text"], "Look at this");
        assert_eq!(json[1]["type"], "image_url");
        assert_eq!(json[1]["image_url"]["url"], "https://example.com/img.png");
        assert_eq!(json[1]["image_url"]["detail"], "high");
    }

    #[test]
    fn serialize_stop_variants() {
        let single = serde_json::to_value(Stop::String("STOP".to_owned())).unwrap();
        assert_eq!(single, "STOP");

        let array =
            serde_json::to_value(Stop::Array(vec!["STOP".to_owned(), "END".to_owned()])).unwrap();
        assert_eq!(array, serde_json::json!(["STOP", "END"]));
    }

    // Deserialization tests

    #[test]
    fn deserialize_chat_completion_response() {
        let json = serde_json::json!({
            "id": "gen-abc123",
            "choices": [{
                "finish_reason": "stop",
                "index": 0,
                "message": {
                    "role": "assistant",
                    "content": "Hello!"
                }
            }],
            "created": 1_700_000_000_u64,
            "model": "openai/gpt-4o",
            "object": "chat.completion",
            "usage": {
                "prompt_tokens": 10,
                "completion_tokens": 5,
                "total_tokens": 15
            }
        });
        let completion: ChatCompletion = serde_json::from_value(json).unwrap();
        assert_eq!(completion.id, "gen-abc123");
        assert_eq!(completion.model, "openai/gpt-4o");
        assert_eq!(completion.object, "chat.completion");
        assert_eq!(completion.created, Some(1_700_000_000));
        assert_eq!(completion.choices.len(), 1);
        assert_eq!(completion.choices[0].index, 0);
        assert_eq!(
            completion.choices[0].message.content.as_deref(),
            Some("Hello!")
        );
        assert!(matches!(
            completion.choices[0].finish_reason,
            Some(FinishReason::Stop)
        ));
        let usage = completion.usage.unwrap();
        assert_eq!(usage.prompt_tokens, 10);
        assert_eq!(usage.completion_tokens, 5);
        assert_eq!(usage.total_tokens, 15);
    }

    #[test]
    fn deserialize_chat_completion_with_tool_calls() {
        let json = serde_json::json!({
            "id": "gen-tool",
            "choices": [{
                "finish_reason": "tool_calls",
                "index": 0,
                "message": {
                    "role": "assistant",
                    "content": null,
                    "tool_calls": [{
                        "id": "call_abc",
                        "type": "function",
                        "function": {
                            "name": "get_weather",
                            "arguments": "{\"location\":\"Tokyo\"}"
                        }
                    }]
                }
            }],
            "created": 1_700_000_000_u64,
            "model": "openai/gpt-4o",
            "object": "chat.completion"
        });
        let completion: ChatCompletion = serde_json::from_value(json).unwrap();
        assert!(matches!(
            completion.choices[0].finish_reason,
            Some(FinishReason::ToolCalls)
        ));
        let tool_calls = completion.choices[0].message.tool_calls.as_ref().unwrap();
        assert_eq!(tool_calls.len(), 1);
        assert_eq!(tool_calls[0].id, "call_abc");
        assert_eq!(tool_calls[0].r#type, "function");
        assert_eq!(tool_calls[0].function.name, "get_weather");
        assert_eq!(tool_calls[0].function.arguments, "{\"location\":\"Tokyo\"}");
    }

    #[test]
    fn deserialize_finish_reason_variants() {
        for (input, expected) in [
            ("stop", "Stop"),
            ("length", "Length"),
            ("tool_calls", "ToolCalls"),
            ("content_filter", "ContentFilter"),
            ("error", "Error"),
        ] {
            let json = serde_json::json!({
                "id": "gen-fr",
                "choices": [{
                    "finish_reason": input,
                    "index": 0,
                    "message": { "role": "assistant", "content": "" }
                }],
                "created": null,
                "model": "test",
                "object": "chat.completion"
            });
            let completion: ChatCompletion = serde_json::from_value(json).unwrap();
            let actual = format!(
                "{:?}",
                completion.choices[0].finish_reason.as_ref().unwrap()
            );
            assert_eq!(actual, expected, "failed for {input}");
        }
    }

    #[test]
    fn deserialize_usage() {
        let json = serde_json::json!({
            "prompt_tokens": 100,
            "completion_tokens": 50,
            "total_tokens": 150
        });
        let usage: Usage = serde_json::from_value(json).unwrap();
        assert_eq!(usage.prompt_tokens, 100);
        assert_eq!(usage.completion_tokens, 50);
        assert_eq!(usage.total_tokens, 150);
    }

    #[test]
    fn deserialize_model() {
        let json = serde_json::json!({
            "id": MODEL,
            "name": "Claude Haiku 4.5",
            "created": 1_700_000_000_u64,
            "context_length": 128_000
        });
        let model: Model = serde_json::from_value(json).unwrap();
        assert_eq!(model.id, MODEL);
        assert_eq!(model.name, "Claude Haiku 4.5");
        assert_eq!(model.created, Some(1_700_000_000));
        assert_eq!(model.context_length, Some(128_000));
    }

    #[test]
    fn deserialize_assistant_message_no_content() {
        let json = serde_json::json!({
            "id": "gen-nc",
            "choices": [{
                "finish_reason": "tool_calls",
                "index": 0,
                "message": {
                    "role": "assistant",
                    "content": null,
                    "tool_calls": [{
                        "id": "call_1",
                        "type": "function",
                        "function": {
                            "name": "search",
                            "arguments": "{}"
                        }
                    }]
                }
            }],
            "created": null,
            "model": "test",
            "object": "chat.completion"
        });
        let completion: ChatCompletion = serde_json::from_value(json).unwrap();
        assert!(completion.choices[0].message.content.is_none());
        assert!(completion.choices[0].message.tool_calls.is_some());
    }

    // Roundtrip tests

    #[test]
    fn roundtrip_message_user() {
        let msg = Message::User {
            content: Content::Text("hello world".to_owned()),
            name: Some("bob".to_owned()),
        };
        let json = serde_json::to_value(&msg).unwrap();
        let deserialized: Message = serde_json::from_value(json).unwrap();
        match deserialized {
            Message::User { content, name } => {
                assert!(matches!(content, Content::Text(s) if s == "hello world"));
                assert_eq!(name, Some("bob".to_owned()));
            }
            other => panic!("expected User, got {other:?}"),
        }
    }

    #[test]
    fn roundtrip_message_assistant_with_tool_calls() {
        let msg = Message::Assistant {
            content: None,
            tool_calls: Some(vec![ToolCall {
                id: "call_rt".to_owned(),
                r#type: "function".to_owned(),
                function: ToolCallFunction {
                    name: "search".to_owned(),
                    arguments: "{\"q\":\"test\"}".to_owned(),
                },
            }]),
        };
        let json = serde_json::to_value(&msg).unwrap();
        let deserialized: Message = serde_json::from_value(json).unwrap();
        match deserialized {
            Message::Assistant {
                content,
                tool_calls,
            } => {
                assert!(content.is_none());
                let calls = tool_calls.unwrap();
                assert_eq!(calls.len(), 1);
                assert_eq!(calls[0].id, "call_rt");
                assert_eq!(calls[0].function.name, "search");
            }
            other => panic!("expected Assistant, got {other:?}"),
        }
    }

    #[test]
    fn roundtrip_tool_call() {
        let tc = ToolCall {
            id: "call_abc".to_owned(),
            r#type: "function".to_owned(),
            function: ToolCallFunction {
                name: "get_weather".to_owned(),
                arguments: "{\"location\":\"NYC\"}".to_owned(),
            },
        };
        let json = serde_json::to_value(&tc).unwrap();
        let deserialized: ToolCall = serde_json::from_value(json).unwrap();
        assert_eq!(deserialized.id, "call_abc");
        assert_eq!(deserialized.r#type, "function");
        assert_eq!(deserialized.function.name, "get_weather");
        assert_eq!(deserialized.function.arguments, "{\"location\":\"NYC\"}");
    }

    // Live API tests

    #[tokio::test]
    #[ignore = "live API test; run with --ignored --test-threads=1"]
    async fn list_models_live() {
        let models = list_models().await.unwrap();
        assert!(!models.is_empty());
    }

    #[tokio::test]
    #[ignore = "live API test; run with --ignored --test-threads=1"]
    async fn create_chat_completion_live_simple() {
        let params = ChatCompletionParams {
            messages: vec![Message::User {
                content: Content::Text("Reply with only the word 'pong'.".to_owned()),
                name: None,
            }],
            model: Some(MODEL.to_owned()),
            max_tokens: Some(64),
            temperature: Some(0.0),
            stop: None,
            stream: Some(false),
            tools: None,
            tool_choice: None,
            response_format: None,
            reasoning: None,
        };
        let completion = create_chat_completion(params).await.unwrap();
        assert!(!completion.choices.is_empty());
        assert!(completion.choices[0].message.content.is_some());
    }

    #[tokio::test]
    #[ignore = "live API test; run with --ignored --test-threads=1"]
    async fn create_chat_completion_live_with_tools() {
        let params = ChatCompletionParams {
            messages: vec![Message::User {
                content: Content::Text("What's the weather in Tokyo?".to_owned()),
                name: None,
            }],
            model: Some(MODEL.to_owned()),
            max_tokens: Some(256),
            temperature: Some(0.0),
            stop: None,
            stream: Some(false),
            tools: Some(vec![ToolDefinition::function(FunctionDefinition {
                name: "get_weather".to_owned(),
                description: Some("Get weather for a location".to_owned()),
                parameters: Some(serde_json::json!({
                    "type": "object",
                    "properties": {
                        "location": {
                            "type": "string",
                            "description": "City name"
                        }
                    },
                    "required": ["location"]
                })),
                strict: None,
            })]),
            tool_choice: Some(ToolChoice::Mode(ToolChoiceMode::Required)),
            response_format: None,
            reasoning: None,
        };
        let completion = create_chat_completion(params).await.unwrap();
        assert!(!completion.choices.is_empty());
        let tool_calls = completion.choices[0].message.tool_calls.as_ref();
        assert!(
            tool_calls.is_some() && !tool_calls.unwrap().is_empty(),
            "expected tool_calls in response"
        );
    }
}
