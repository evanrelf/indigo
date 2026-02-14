#![expect(dead_code)]

mod claude;

use anyhow::anyhow;
use clap::Parser as _;
use crossterm::event::{self, Event as TerminalEvent, KeyCode, KeyModifiers};
use std::{fmt::Write as _, mem, process::ExitCode, thread, time::Duration};

#[derive(clap::Parser)]
struct Args {}

#[expect(clippy::let_and_return)]
fn main() -> anyhow::Result<ExitCode> {
    let args = Args::parse();

    // TODO: init TUI

    let result = run(&args);

    // TODO: restore TUI

    result
}

enum ClaudeEvent {
    MessageStart { request_id: u64 },
    ContentBlockDelta { request_id: u64, text: String },
    ContentBlockStop { request_id: u64 },
    MessageStop { request_id: u64 },
    Error { request_id: u64, error: String },
}

#[derive(Clone)]
enum Message {
    User(String),
    Assistant {
        request_id: u64,
        text: String,
        streaming: bool,
        done: bool,
    },
}

#[derive(Clone)]
struct State {
    messages: Vec<Message>,
    queued_messages: Vec<String>,
    input: String,
    next_request_id: u64,
    exit_code: Option<ExitCode>,
}

fn run(args: &Args) -> anyhow::Result<ExitCode> {
    let state = State {
        messages: Vec::new(),
        queued_messages: Vec::new(),
        input: String::new(),
        next_request_id: 0,
        exit_code: None,
    };

    let (state_tx, state_rx) = tokio::sync::watch::channel(state.clone());
    let (terminal_tx, terminal_rx) = tokio::sync::mpsc::channel(64);
    let (claude_tx, claude_rx) = tokio::sync::mpsc::channel(1024);

    thread::scope(|scope| {
        let app_handle =
            scope.spawn(|| run_app(args, state, terminal_rx, claude_rx, claude_tx, state_tx));
        let tui_result = run_tui(state_rx, terminal_tx);
        let exit_code = app_handle
            .join()
            .map_err(|_| anyhow!("app thread panicked"))??;
        tui_result?;
        Ok(exit_code)
    })
}

#[expect(clippy::needless_pass_by_value)]
fn run_tui(
    state_rx: tokio::sync::watch::Receiver<State>,
    terminal_tx: tokio::sync::mpsc::Sender<TerminalEvent>,
) -> anyhow::Result<()> {
    loop {
        // Stop TUI thread if app thread stops
        if terminal_tx.is_closed() {
            break Ok(());
        }

        let state = state_rx.borrow();

        render(&state);

        if event::poll(Duration::from_millis(1_000 / 60))? {
            let event = event::read()?;
            // Drop terminal events when app is overloaded instead of blocking rendering
            let _ = terminal_tx.try_send(event);
        }
    }
}

#[expect(clippy::needless_pass_by_value)]
#[tokio::main(flavor = "current_thread")]
async fn run_app(
    _args: &Args,
    mut state: State,
    mut terminal_rx: tokio::sync::mpsc::Receiver<TerminalEvent>,
    mut claude_rx: tokio::sync::mpsc::Receiver<ClaudeEvent>,
    claude_tx: tokio::sync::mpsc::Sender<ClaudeEvent>,
    state_tx: tokio::sync::watch::Sender<State>,
) -> anyhow::Result<ExitCode> {
    let mut terminal_events_buffer = Vec::with_capacity(8);
    let mut claude_events_buffer = Vec::with_capacity(32);

    loop {
        tokio::select! {
            terminal_events_count = terminal_rx.recv_many(&mut terminal_events_buffer, 8) => {
                if terminal_events_count == 0 {
                    break;
                }
                for terminal_event in terminal_events_buffer.drain(..) {
                    handle_terminal_event(&mut state, terminal_event);
                }
            }

            claude_events_count = claude_rx.recv_many(&mut claude_events_buffer, 32) => {
                if claude_events_count == 0 {
                    break;
                }
                for claude_event in claude_events_buffer.drain(..) {
                    handle_claude_event(&mut state, claude_event)?;
                }
            }

            else => break,
        }

        for queued_message in state.queued_messages.drain(..) {
            let request_id = state.next_request_id;
            state.next_request_id += 1;

            state.messages.push(Message::User(queued_message));
            state.messages.push(Message::Assistant {
                request_id,
                text: String::new(),
                streaming: false,
                done: false,
            });

            let tx = claude_tx.clone();
            let messages: Vec<claude::MessageParam> = state
                .messages
                .iter()
                .filter_map(|m| match m {
                    Message::User(text) => Some(
                        claude::MessageParam::builder()
                            .role(claude::Role::User)
                            .content(claude::Content::Text(text.clone()))
                            .build(),
                    ),
                    Message::Assistant {
                        text, done: true, ..
                    } => Some(
                        claude::MessageParam::builder()
                            .role(claude::Role::Assistant)
                            .content(claude::Content::Text(text.clone()))
                            .build(),
                    ),
                    Message::Assistant { .. } => None,
                })
                .collect();
            tokio::spawn(async move {
                send_message(tx, request_id, messages).await;
            });
        }

        if state.exit_code.is_some() {
            break;
        }

        state_tx.send(state.clone())?;
    }

    Ok(state.exit_code.unwrap_or(ExitCode::SUCCESS))
}

#[expect(clippy::needless_pass_by_value)]
fn handle_terminal_event(state: &mut State, terminal_event: TerminalEvent) {
    #[expect(clippy::single_match)]
    match terminal_event {
        TerminalEvent::Key(key_event) => match (key_event.modifiers, key_event.code) {
            (m, KeyCode::Char(char)) if m == KeyModifiers::NONE || m == KeyModifiers::SHIFT => {
                state.input.push(char);
            }
            (m, KeyCode::Backspace) if m == KeyModifiers::NONE => {
                // TODO: Not grapheme aware
                state.input.pop();
            }
            (m, KeyCode::Enter) if m == KeyModifiers::NONE => {
                let message = mem::take(&mut state.input);
                if !message.is_empty() {
                    state.queued_messages.push(message);
                }
            }
            (m, KeyCode::Char('c')) if m == KeyModifiers::CONTROL => {
                state.exit_code = Some(ExitCode::SUCCESS);
            }
            _ => {}
        },
        _ => {}
    }
}

fn handle_claude_event(state: &mut State, claude_event: ClaudeEvent) -> anyhow::Result<()> {
    match claude_event {
        ClaudeEvent::MessageStart { request_id } => {
            if let Some(Message::Assistant { streaming, .. }) =
                find_assistant_message(&mut state.messages, request_id)
            {
                *streaming = true;
            }
        }
        ClaudeEvent::ContentBlockDelta { request_id, text } => {
            if let Some(Message::Assistant { text: t, .. }) =
                find_assistant_message(&mut state.messages, request_id)
            {
                t.push_str(&text);
            }
        }
        ClaudeEvent::ContentBlockStop { request_id: _ } => {}
        ClaudeEvent::MessageStop { request_id } => {
            if let Some(Message::Assistant { done, .. }) =
                find_assistant_message(&mut state.messages, request_id)
            {
                *done = true;
            }
        }
        ClaudeEvent::Error { request_id, error } => {
            if let Some(Message::Assistant { text, done, .. }) =
                find_assistant_message(&mut state.messages, request_id)
            {
                write!(text, "[Error: {error}]")?;
                *done = true;
            }
        }
    }

    Ok(())
}

fn find_assistant_message(messages: &mut [Message], request_id: u64) -> Option<&mut Message> {
    messages
        .iter_mut()
        .find(|m| matches!(m, Message::Assistant { request_id: rid, .. } if *rid == request_id))
}

async fn send_message(
    tx: tokio::sync::mpsc::Sender<ClaudeEvent>,
    request_id: u64,
    messages: Vec<claude::MessageParam>,
) {
    let params = claude::MessageCreateParams::builder()
        .model(String::from("claude-haiku-4-5"))
        .max_tokens(4096)
        .messages(messages)
        .build();

    match claude::create_message(params).await {
        Ok(response) => {
            let _ = tx.send(ClaudeEvent::MessageStart { request_id }).await;
            for block in &response.content {
                if let claude::ContentBlock::Text { text, .. } = block {
                    let _ = tx
                        .send(ClaudeEvent::ContentBlockDelta {
                            request_id,
                            text: text.clone(),
                        })
                        .await;
                    let _ = tx.send(ClaudeEvent::ContentBlockStop { request_id }).await;
                }
            }
            let _ = tx.send(ClaudeEvent::MessageStop { request_id }).await;
        }
        Err(err) => {
            let _ = tx
                .send(ClaudeEvent::Error {
                    request_id,
                    error: err.to_string(),
                })
                .await;
        }
    }
}

fn render(_state: &State) {
    todo!()
}
