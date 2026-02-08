#![expect(dead_code)]

use anyhow::anyhow;
use clap::Parser as _;
use crossterm::event::{self, Event, KeyCode, KeyModifiers};
use ratatui::{DefaultTerminal, Frame};
use std::{mem, process::ExitCode, thread, time::Duration};

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
enum Message {
    User(String),
    Assistant(String),
}

#[derive(Clone)]
struct State {
    messages: Vec<Message>,
    input: String,
}

fn run(args: &Args, terminal: &mut DefaultTerminal) -> anyhow::Result<ExitCode> {
    let state = State {
        messages: Vec::new(),
        input: String::new(),
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

#[expect(clippy::needless_pass_by_value)]
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

#[expect(clippy::needless_pass_by_value)]
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
                    state.input.push(char);
                }
                (m, KeyCode::Backspace) if m == KeyModifiers::NONE => {
                    // TODO: Not grapheme aware
                    state.input.pop();
                }
                (m, KeyCode::Enter) if m == KeyModifiers::NONE => {
                    let input = mem::take(&mut state.input);
                    state.messages.push(Message::User(input));
                    state.messages.push(Message::Assistant(String::from("k")));
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

#[expect(clippy::similar_names)]
fn render(frame: &mut Frame<'_>, state: &State) {
    use ratatui::{
        layout::{Constraint, Layout, Rect, Size},
        widgets::{Block, Padding, Paragraph, Wrap},
    };
    use tui_scrollview::{ScrollView, ScrollViewState, ScrollbarVisibility};

    let layout = Layout::vertical([Constraint::Fill(1), Constraint::Length(5)]);
    let [messages_area, input_area] = layout.areas(frame.area());

    let messages_block_widget = Block::bordered().title("Messages").padding(Padding::ZERO);
    let messages_block_inner_area = messages_block_widget.inner(messages_area);
    let mut messages_scroll_widget = ScrollView::new(Size::from(messages_block_inner_area))
        .vertical_scrollbar_visibility(ScrollbarVisibility::Never)
        .horizontal_scrollbar_visibility(ScrollbarVisibility::Never);
    let mut height = 0;
    for message in &state.messages {
        let message_area = match message {
            Message::User(_) => {
                let [_, area] =
                    Layout::horizontal([Constraint::Fill(1), Constraint::Percentage(80)])
                        .areas(messages_block_inner_area);
                area
            }
            Message::Assistant(_) => {
                let [area, _] =
                    Layout::horizontal([Constraint::Percentage(80), Constraint::Fill(1)])
                        .areas(messages_block_inner_area);
                area
            }
        };
        let message_widget = match message {
            Message::User(string) => Paragraph::new(string.clone())
                .wrap(Wrap { trim: false })
                .right_aligned(),
            Message::Assistant(string) => Paragraph::new(string.clone())
                .wrap(Wrap { trim: false })
                .left_aligned(),
        };
        let message_height = message_widget.line_count(message_area.width);
        messages_scroll_widget.render_widget(
            message_widget,
            Rect::new(
                message_area.x - messages_block_inner_area.x,
                u16::try_from(height).unwrap(),
                message_area.width,
                u16::try_from(message_height).unwrap(),
            ),
        );
        height += message_height;
    }
    let mut messages_scroll_state = ScrollViewState::default();
    frame.render_widget(messages_block_widget, messages_area);
    frame.render_stateful_widget(
        messages_scroll_widget,
        messages_block_inner_area,
        &mut messages_scroll_state,
    );

    let input_widget = Paragraph::new(&*state.input)
        .wrap(Wrap { trim: false })
        .block(Block::bordered().title("Input"));
    frame.render_widget(input_widget, input_area);
}

async fn claude_demo() -> anyhow::Result<()> {
    let model_id = "claude-haiku-4-5";

    // let model_info = claude::models_retrieve(model_id).await?;

    // println!("model info:\n{model_info:?}\n");

    let messages_create_params = claude::MessageCreateParams {
        model: String::from(model_id),
        max_tokens: 1024,
        messages: vec![
            claude::MessageParam {
                role: claude::Role::User,
                content: claude::Content::Text(String::from("Hello, Claude!")),
            },
            claude::MessageParam {
                role: claude::Role::Assistant,
                content: claude::Content::Text(String::from("Hello, Evan!")),
            },
        ],
        system: None,
        stop_sequences: None,
        stream: None,
        temperature: None,
        thinking: None,
        tool_choice: None,
        tools: None,
        output_config: None,
    };

    println!(
        "request:\n{}\n",
        serde_json::to_string(&messages_create_params)?
    );

    let message = claude::create_message(messages_create_params).await?;

    println!("response:\n{message:?}\n");

    Ok(())
}

mod claude;
