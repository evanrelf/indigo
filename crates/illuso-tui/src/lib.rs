#![allow(unused)]

use bitflags::bitflags;
use rustix::event::{PollFd, PollFlags, Timespec, poll};
use rustix::termios::{self, OptionalActions, Termios};
use std::{
    collections::VecDeque,
    fmt::{self, Display},
    io::{self, Read as _, Write as _},
    mem, slice,
    sync::Mutex,
    time::{Duration, Instant},
};

// Raw mode

static ORIGINAL_TERMIOS: Mutex<Option<Termios>> = Mutex::new(None);

pub fn enable_raw_mode() -> io::Result<()> {
    let mut original = ORIGINAL_TERMIOS.lock().unwrap();
    if original.is_some() {
        return Ok(());
    }

    let mut termios = termios::tcgetattr(io::stdin())?;
    *original = Some(termios.clone());

    termios.make_raw();
    termios::tcsetattr(io::stdin(), OptionalActions::Flush, &termios)?;

    Ok(())
}

pub fn disable_raw_mode() -> io::Result<()> {
    let mut original = ORIGINAL_TERMIOS.lock().unwrap();
    if let Some(termios) = original.take() {
        termios::tcsetattr(io::stdin(), OptionalActions::Flush, &termios)?;
    }

    Ok(())
}

// Cursor

pub struct MoveUp(pub u16);

impl Display for MoveUp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "\x1b[{}A", self.0)
    }
}

pub struct MoveDown(pub u16);

impl Display for MoveDown {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "\x1b[{}B", self.0)
    }
}

pub struct MoveForward(pub u16);

impl Display for MoveForward {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "\x1b[{}C", self.0)
    }
}

pub struct MoveBackward(pub u16);

impl Display for MoveBackward {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "\x1b[{}D", self.0)
    }
}

pub struct MoveTo(pub u16, pub u16);

impl Display for MoveTo {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "\x1b[{};{}H", self.0, self.1)
    }
}

pub struct MoveToColumn(pub u16);

impl Display for MoveToColumn {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "\x1b[{}G", self.0)
    }
}

pub const SAVE_CURSOR: &str = "\x1b[s";
pub const RESTORE_CURSOR: &str = "\x1b[u";
pub const HIDE_CURSOR: &str = "\x1b[?25l";
pub const SHOW_CURSOR: &str = "\x1b[?25h";

// Clearing

pub const CLEAR_TO_END_OF_LINE: &str = "\x1b[K";
pub const CLEAR_TO_START_OF_LINE: &str = "\x1b[1K";
pub const CLEAR_LINE: &str = "\x1b[2K";
pub const CLEAR_TO_END_OF_SCREEN: &str = "\x1b[J";
pub const CLEAR_TO_START_OF_SCREEN: &str = "\x1b[1J";
pub const CLEAR_SCREEN: &str = "\x1b[2J";

// Modes

pub struct QueryMode(pub u16);

impl Display for QueryMode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "\x1b[?{}$p", self.0)
    }
}

pub struct SetMode(pub u16);

impl Display for SetMode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "\x1b[?{}h", self.0)
    }
}

pub struct ResetMode(pub u16);

impl Display for ResetMode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "\x1b[?{}l", self.0)
    }
}

// In-band resize
// https://gist.github.com/rockorager/e695fb2924d36b2bcf1fff4a3704bd83

pub const IN_BAND_RESIZE_QUERY: QueryMode = QueryMode(2048);
pub const IN_BAND_RESIZE_SET: SetMode = SetMode(2048);
pub const IN_BAND_RESIZE_RESET: ResetMode = ResetMode(2048);

// Synchronized output
// https://github.com/contour-terminal/vt-extensions/blob/master/synchronized-output.md

pub const SYNC_UPDATE_QUERY: QueryMode = QueryMode(2026);
pub const SYNC_UPDATE_SET: SetMode = SetMode(2026);
pub const SYNC_UPDATE_RESET: ResetMode = ResetMode(2026);

enum Message {
    Event(Event),
    Reply(Reply),
}

pub enum Event {
    Key(Key),
    Resize { height: u16, width: u16 },
}

pub struct Key {
    pub code: KeyCode,
    pub modifiers: KeyModifiers,
}

pub enum KeyCode {
    Esc,
    Backspace,
    Tab,
    Enter,
    Left,
    Right,
    Up,
    Down,
    Char(char),
}

bitflags! {
    pub struct KeyModifiers: u8 {
        const SHIFT = 1;
        const ALT = 2;
        const CTRL = 4;
    }
}

enum Reply {
    /// <https://vt100.net/docs/vt510-rm/DECRPM.html>
    Decrpm { mode: u16, value: u8 },
}

// Parser

/// State machine for parsing terminal escape sequences.
///
/// Sequences we care about are CSI (Control Sequence Introducer) sequences,
/// which have the form: `ESC [ [private_marker] params [intermediates] final_byte`
///
/// - Private marker: a single byte (`?`, `>`, `<`, `=`) before the parameters
///   that indicates a private/DEC-specific sequence
/// - Params: semicolon-separated decimal numbers (e.g. `2048;2`)
/// - Intermediates: bytes between the params and final byte (e.g. `$`)
/// - Final byte: a single byte in 0x40-0x7e that identifies the sequence
#[derive(Default)]
enum ParserState {
    #[default]
    Ground,
    /// Seen `ESC` (0x1b), waiting for `[` to start a CSI sequence.
    Escape,
    /// Inside a CSI sequence (`ESC [`), accumulating parameters.
    Csi(CsiState),
}

struct CsiState {
    /// e.g. `?` in `ESC [ ? 2048 ; 2 $ y` (DEC private sequences).
    private_marker: Option<u8>,
    /// Completed semicolon-separated numeric parameters.
    params: Vec<u32>,
    /// Parameter currently being accumulated digit-by-digit.
    current_param: u32,
    /// Bytes between the parameters and the final byte, e.g. `$`.
    intermediates: Vec<u8>,
    /// Whether we've seen any digit or `;` yet (private marker is only
    /// valid before any parameter bytes).
    seen_param_byte: bool,
}

#[derive(Default)]
pub struct Parser {
    state: ParserState,
    /// Raw bytes read from stdin but not yet fed to the state machine.
    pending_bytes: VecDeque<u8>,
    /// Events buffered by `read_reply` (events that arrived while waiting for a reply).
    pending_events: VecDeque<Event>,
}

impl Parser {
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    /// Feed one byte through the state machine, returning a message if a
    /// complete sequence was recognized.
    fn step(&mut self, byte: u8) -> Option<Message> {
        let (new_state, msg) = match mem::replace(&mut self.state, ParserState::Ground) {
            ParserState::Ground => {
                let new = if byte == 0x1b {
                    ParserState::Escape
                } else {
                    ParserState::Ground
                };
                (new, None)
            }
            ParserState::Escape => {
                let new = if byte == b'[' {
                    ParserState::Csi(CsiState {
                        private_marker: None,
                        params: Vec::new(),
                        current_param: 0,
                        intermediates: Vec::new(),
                        seen_param_byte: false,
                    })
                } else {
                    ParserState::Ground
                };
                (new, None)
            }
            ParserState::Csi(csi) => Self::step_csi(csi, byte),
        };
        self.state = new_state;
        msg
    }

    /// Process one byte while inside a CSI sequence. Accumulates parameters
    /// and intermediates until a final byte (0x40-0x7e) completes the sequence.
    fn step_csi(mut csi: CsiState, byte: u8) -> (ParserState, Option<Message>) {
        match byte {
            // Private marker (only before any param bytes)
            0x3c..=0x3f
                if !csi.seen_param_byte
                    && csi.intermediates.is_empty()
                    && csi.private_marker.is_none() =>
            {
                csi.private_marker = Some(byte);
                (ParserState::Csi(csi), None)
            }
            // Digit
            b'0'..=b'9' if csi.intermediates.is_empty() => {
                csi.seen_param_byte = true;
                csi.current_param = csi
                    .current_param
                    .saturating_mul(10)
                    .saturating_add(u32::from(byte - b'0'));
                (ParserState::Csi(csi), None)
            }
            // Parameter separator
            b';' if csi.intermediates.is_empty() => {
                csi.seen_param_byte = true;
                csi.params.push(csi.current_param);
                csi.current_param = 0;
                (ParserState::Csi(csi), None)
            }
            // Intermediate bytes
            0x20..=0x2f => {
                csi.intermediates.push(byte);
                (ParserState::Csi(csi), None)
            }
            // Final byte — dispatch
            0x40..=0x7e => {
                csi.params.push(csi.current_param);
                let msg = Self::dispatch_csi(&csi, byte);
                (ParserState::Ground, msg)
            }
            // ESC aborts the current sequence and starts a new one
            0x1b => (ParserState::Escape, None),
            // Invalid byte
            _ => (ParserState::Ground, None),
        }
    }

    /// Try to interpret a completed CSI sequence as a known message. Returns
    /// `None` for valid-but-unrecognized sequences (they're silently discarded).
    #[allow(clippy::cast_possible_truncation)]
    fn dispatch_csi(csi: &CsiState, final_byte: u8) -> Option<Message> {
        match (csi.private_marker, final_byte, csi.intermediates.as_slice()) {
            // DECRPM: CSI ? mode ; value $ y
            (Some(b'?'), b'y', [b'$']) => {
                let mode = *csi.params.first()? as u16;
                let value = *csi.params.get(1)? as u8;
                Some(Message::Reply(Reply::Decrpm { mode, value }))
            }
            // In-band resize: CSI 48 ; height ; width [; height_pix ; width_pix] t
            (None, b't', []) => {
                if csi.params.first() != Some(&48) {
                    return None;
                }
                let height = *csi.params.get(1)? as u16;
                let width = *csi.params.get(2)? as u16;
                Some(Message::Event(Event::Resize { height, width }))
            }
            _ => None,
        }
    }
}

// Reading

fn read_message(parser: &mut Parser, timeout: Option<Duration>) -> io::Result<Option<Message>> {
    let deadline = timeout.map(|t| Instant::now() + t);
    let stdin = io::stdin();

    loop {
        // Drain buffered bytes through the state machine
        while let Some(byte) = parser.pending_bytes.pop_front() {
            if let Some(msg) = parser.step(byte) {
                return Ok(Some(msg));
            }
        }

        // Calculate remaining timeout
        let remaining = match deadline {
            Some(deadline) => {
                let remaining = deadline.saturating_duration_since(Instant::now());
                if remaining.is_zero() {
                    return Ok(None);
                }
                Some(remaining)
            }
            None => None,
        };

        // Poll stdin for readability
        let mut pollfd = PollFd::new(&stdin, PollFlags::IN);
        let timespec = remaining.map(|d| Timespec {
            tv_sec: d.as_secs().cast_signed(),
            tv_nsec: d.subsec_nanos().into(),
        });
        let ready = poll(slice::from_mut(&mut pollfd), timespec.as_ref())?;
        if ready == 0 {
            return Ok(None);
        }

        // Read available bytes
        let mut buf = [0u8; 256];
        let n = stdin.lock().read(&mut buf)?;
        if n == 0 {
            return Ok(None);
        }
        parser.pending_bytes.extend(&buf[..n]);
    }
}

pub fn read_event(parser: &mut Parser, timeout: Option<Duration>) -> io::Result<Option<Event>> {
    if let Some(event) = parser.pending_events.pop_front() {
        return Ok(Some(event));
    }

    let deadline = timeout.map(|t| Instant::now() + t);
    loop {
        let remaining = match deadline {
            Some(deadline) => {
                let remaining = deadline.saturating_duration_since(Instant::now());
                if remaining.is_zero() {
                    return Ok(None);
                }
                Some(remaining)
            }
            None => None,
        };
        match read_message(parser, remaining)? {
            Some(Message::Event(event)) => return Ok(Some(event)),
            Some(Message::Reply(_)) => {}
            None => return Ok(None),
        }
    }
}

fn read_reply(parser: &mut Parser, timeout: Option<Duration>) -> io::Result<Option<Reply>> {
    let deadline = timeout.map(|t| Instant::now() + t);
    loop {
        let remaining = match deadline {
            Some(deadline) => {
                let remaining = deadline.saturating_duration_since(Instant::now());
                if remaining.is_zero() {
                    return Ok(None);
                }
                Some(remaining)
            }
            None => None,
        };
        match read_message(parser, remaining)? {
            Some(Message::Reply(reply)) => return Ok(Some(reply)),
            Some(Message::Event(event)) => parser.pending_events.push_back(event),
            None => return Ok(None),
        }
    }
}

pub enum ModeSetting {
    ModeNotRecognized,
    Set,
    Reset,
    PermanentlySet,
    PermanentlyReset,
}

impl ModeSetting {
    #[must_use]
    pub fn is_recognized(&self) -> bool {
        !matches!(self, Self::ModeNotRecognized)
    }

    #[must_use]
    pub fn is_enabled(&self) -> bool {
        matches!(self, Self::Set | Self::PermanentlySet)
    }

    #[must_use]
    pub fn is_mutable(&self) -> bool {
        matches!(self, Self::Set | Self::Reset)
    }
}

#[tracing::instrument(skip(parser))]
pub fn query_mode(
    parser: &mut Parser,
    mode: u16,
    timeout: Option<Duration>,
) -> io::Result<Option<ModeSetting>> {
    // Request mode, host to terminal (https://vt100.net/docs/vt510-rm/DECRQM.html)
    let mut stdout = io::stdout().lock();
    write!(stdout, "\x1b[?{mode}$p")?;
    stdout.flush()?;

    // Report mode, terminal to host (https://vt100.net/docs/vt510-rm/DECRPM.html)
    let deadline = timeout.map(|timeout| Instant::now() + timeout);
    loop {
        let timeout = deadline.map(|deadline| deadline.saturating_duration_since(Instant::now()));
        if let Some(timeout) = timeout
            && timeout.is_zero()
        {
            return Ok(None);
        }
        match read_reply(parser, timeout)? {
            Some(Reply::Decrpm {
                mode: reply_mode,
                value,
            }) if mode == reply_mode => {
                return Ok(match value {
                    0 => Some(ModeSetting::ModeNotRecognized),
                    1 => Some(ModeSetting::Set),
                    2 => Some(ModeSetting::Reset),
                    3 => Some(ModeSetting::PermanentlySet),
                    4 => Some(ModeSetting::PermanentlyReset),
                    _ => {
                        tracing::error!("invalid mode setting {value}");
                        None
                    }
                });
            }
            Some(_) => {}
            None => return Ok(None),
        }
    }
}

const DEFAULT_TIMEOUT: Option<Duration> = Some(Duration::from_millis(500));

#[tracing::instrument(skip(parser))]
pub fn query_in_band_resize(
    parser: &mut Parser,
    timeout: Option<Duration>,
) -> io::Result<Option<ModeSetting>> {
    query_mode(parser, 2048, timeout)
}

#[tracing::instrument(skip(parser))]
pub fn enable_in_band_resize(parser: &mut Parser) -> io::Result<bool> {
    match query_in_band_resize(parser, DEFAULT_TIMEOUT)? {
        None | Some(ModeSetting::ModeNotRecognized | ModeSetting::PermanentlyReset) => Ok(false),
        Some(ModeSetting::Set | ModeSetting::PermanentlySet) => Ok(true),
        Some(ModeSetting::Reset) => {
            let mut stdout = io::stdout().lock();
            write!(stdout, "{IN_BAND_RESIZE_SET}")?;
            stdout.flush()?;
            Ok(true)
        }
    }
}

#[tracing::instrument(skip(parser))]
pub fn disable_in_band_resize(parser: &mut Parser) -> io::Result<bool> {
    match query_in_band_resize(parser, DEFAULT_TIMEOUT)? {
        None | Some(ModeSetting::PermanentlySet) => Ok(false),
        Some(
            ModeSetting::ModeNotRecognized | ModeSetting::Reset | ModeSetting::PermanentlyReset,
        ) => Ok(true),
        Some(ModeSetting::Set) => {
            let mut stdout = io::stdout().lock();
            write!(stdout, "{IN_BAND_RESIZE_RESET}")?;
            stdout.flush()?;
            Ok(true)
        }
    }
}

#[tracing::instrument(skip(parser))]
pub fn query_sync_update(
    parser: &mut Parser,
    timeout: Option<Duration>,
) -> io::Result<Option<ModeSetting>> {
    query_mode(parser, 2026, timeout)
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Feed all bytes through the parser, collecting any messages produced.
    fn feed(parser: &mut Parser, bytes: &[u8]) -> Vec<Message> {
        let mut messages = Vec::new();
        for &byte in bytes {
            if let Some(msg) = parser.step(byte) {
                messages.push(msg);
            }
        }
        messages
    }

    #[test]
    fn decrpm_reply() {
        let mut parser = Parser::new();
        // CSI ? 2048 ; 2 $ y
        let msgs = feed(&mut parser, b"\x1b[?2048;2$y");
        assert_eq!(msgs.len(), 1);
        assert!(matches!(
            msgs[0],
            Message::Reply(Reply::Decrpm {
                mode: 2048,
                value: 2
            })
        ));
    }

    #[test]
    fn in_band_resize() {
        let mut parser = Parser::new();
        // CSI 48 ; 24 ; 80 ; 240 ; 1600 t
        let msgs = feed(&mut parser, b"\x1b[48;24;80;240;1600t");
        assert_eq!(msgs.len(), 1);
        assert!(matches!(
            msgs[0],
            Message::Event(Event::Resize {
                height: 24,
                width: 80
            })
        ));
    }

    #[test]
    fn in_band_resize_without_pixel_dimensions() {
        let mut parser = Parser::new();
        let msgs = feed(&mut parser, b"\x1b[48;24;80t");
        assert_eq!(msgs.len(), 1);
        assert!(matches!(
            msgs[0],
            Message::Event(Event::Resize {
                height: 24,
                width: 80
            })
        ));
    }

    #[test]
    fn garbage_bytes() {
        let mut parser = Parser::new();
        let msgs = feed(&mut parser, b"hello world\x01\x02\x03");
        assert!(msgs.is_empty());
    }

    #[test]
    fn partial_escape_then_garbage() {
        let mut parser = Parser::new();
        // ESC followed by non-[ should reset to Ground
        let msgs = feed(&mut parser, b"\x1bX");
        assert!(msgs.is_empty());
    }

    #[test]
    fn unrecognized_csi_sequence() {
        let mut parser = Parser::new();
        // Valid CSI structure but not a sequence we handle
        let msgs = feed(&mut parser, b"\x1b[1;2H");
        assert!(msgs.is_empty());
    }

    #[test]
    fn invalid_byte_mid_csi() {
        let mut parser = Parser::new();
        // Start a CSI sequence, then feed an invalid byte (0x80), then a valid sequence
        let msgs = feed(&mut parser, b"\x1b[?\x80\x1b[?2048;2$y");
        assert_eq!(msgs.len(), 1);
        assert!(matches!(
            msgs[0],
            Message::Reply(Reply::Decrpm {
                mode: 2048,
                value: 2
            })
        ));
    }

    #[test]
    fn back_to_back_sequences() {
        let mut parser = Parser::new();
        let msgs = feed(&mut parser, b"\x1b[?2048;2$y\x1b[48;24;80t");
        assert_eq!(msgs.len(), 2);
        assert!(matches!(
            msgs[0],
            Message::Reply(Reply::Decrpm {
                mode: 2048,
                value: 2
            })
        ));
        assert!(matches!(
            msgs[1],
            Message::Event(Event::Resize {
                height: 24,
                width: 80
            })
        ));
    }

    #[test]
    fn partial_then_continue() {
        let mut parser = Parser::new();
        // Feed half the sequence
        let msgs = feed(&mut parser, b"\x1b[?2048");
        assert!(msgs.is_empty());
        // Feed the rest
        let msgs = feed(&mut parser, b";2$y");
        assert_eq!(msgs.len(), 1);
        assert!(matches!(
            msgs[0],
            Message::Reply(Reply::Decrpm {
                mode: 2048,
                value: 2
            })
        ));
    }

    #[test]
    fn csi_with_no_params_discarded() {
        let mut parser = Parser::new();
        // CSI with just a final byte (e.g. `ESC [ A` for cursor up)
        let msgs = feed(&mut parser, b"\x1b[A");
        assert!(msgs.is_empty());
    }

    #[test]
    fn esc_mid_csi_aborts_and_starts_new_sequence() {
        let mut parser = Parser::new();
        // Start a CSI, then ESC aborts it and begins a new valid sequence
        let msgs = feed(&mut parser, b"\x1b[123\x1b[?2048;2$y");
        assert_eq!(msgs.len(), 1);
        assert!(matches!(
            msgs[0],
            Message::Reply(Reply::Decrpm {
                mode: 2048,
                value: 2
            })
        ));
    }

    #[test]
    fn resize_wrong_first_param() {
        let mut parser = Parser::new();
        // CSI t but first param isn't 48
        let msgs = feed(&mut parser, b"\x1b[1;24;80t");
        assert!(msgs.is_empty());
    }
}
