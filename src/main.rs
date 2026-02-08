#![expect(dead_code)]

use anyhow::{Context as _, anyhow};
use clap::Parser as _;
use crossterm::event::{self, Event, KeyCode, KeyModifiers};
use jiff::Timestamp;
use ratatui::{DefaultTerminal, Frame};
use serde::{Deserialize, Serialize};
use std::{env, process::ExitCode, sync::LazyLock, thread, time::Duration};

#[derive(clap::Parser)]
struct Args {}

fn main() -> anyhow::Result<ExitCode> {
    let args = Args::parse();

    let mut terminal = ratatui::init();

    let result = run(&args, &mut terminal);

    ratatui::restore();

    result
}

#[derive(Clone)]
struct State {
    message: String,
}

fn run(args: &Args, terminal: &mut DefaultTerminal) -> anyhow::Result<ExitCode> {
    let state = State {
        message: String::from("Hello, world!"),
    };

    let (state_tx, state_rx) = tokio::sync::watch::channel(state.clone());
    let (event_tx, event_rx) = tokio::sync::mpsc::channel(64);

    thread::scope(|scope| {
        let app_handle = scope.spawn(|| run_app(args, state, event_rx, state_tx));
        let tui_result = run_tui(terminal, state_rx, event_tx);
        let exit_code = app_handle
            .join()
            .map_err(|_| anyhow!("app thread panicked"))??;
        tui_result?;
        Ok(exit_code)
    })
}

fn run_tui(
    terminal: &mut DefaultTerminal,
    state_rx: tokio::sync::watch::Receiver<State>,
    event_tx: tokio::sync::mpsc::Sender<Event>,
) -> anyhow::Result<()> {
    loop {
        // Stop TUI thread if app thread stops
        if event_tx.is_closed() {
            break Ok(());
        }

        let state = state_rx.borrow();
        terminal.draw(|frame| render(frame, &state))?;

        if event::poll(Duration::from_millis(1_000 / 60))? {
            let event = event::read()?;
            // Drop events when app is overloaded instead of blocking rendering
            let _ = event_tx.try_send(event);
        }
    }
}

#[tokio::main(flavor = "current_thread")]
async fn run_app(
    _args: &Args,
    mut state: State,
    mut event_rx: tokio::sync::mpsc::Receiver<Event>,
    state_tx: tokio::sync::watch::Sender<State>,
) -> anyhow::Result<ExitCode> {
    while let Some(event) = event_rx.recv().await {
        let mut render = true;

        match event {
            Event::Key(key_event) => match (key_event.modifiers, key_event.code) {
                (m, KeyCode::Char(char)) if m == KeyModifiers::NONE || m == KeyModifiers::SHIFT => {
                    state.message.push(char);
                }
                (m, KeyCode::Char('c')) if m == KeyModifiers::CONTROL => break,
                _ => render = false,
            },
            _ => render = false,
        }

        if render {
            state_tx.send(state.clone())?;
        }
    }

    Ok(ExitCode::SUCCESS)
}

fn render(frame: &mut Frame<'_>, state: &State) {
    frame.render_widget(&*state.message, frame.area());
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
