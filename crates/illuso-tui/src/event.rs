use crate::escape;
use rustix::{
    event::{PollFd, PollFlags, poll},
    time::Timespec,
};
use std::{
    io::{self, Read, Write},
    os::fd::AsFd,
    time::Duration,
};

pub enum Event {
    Resize { height: u16, width: u16 },
}

enum InternalEvent {
    Public(Event),
    Decrpm { mode: u16, value: u8 },
}

pub fn read_event(timeout: Option<Duration>) -> io::Result<Option<Event>> {
    match read_event_internal(timeout)? {
        Some(InternalEvent::Public(event)) => Ok(Some(event)),
        _ => Ok(None),
    }
}

fn duration_to_timespec(d: Duration) -> Timespec {
    Timespec {
        tv_sec: d.as_secs().cast_signed(),
        tv_nsec: i64::from(d.subsec_nanos()),
    }
}

fn read_event_internal(timeout: Option<Duration>) -> io::Result<Option<InternalEvent>> {
    let stdin = io::stdin();
    let fd = PollFd::from_borrowed_fd(stdin.as_fd(), PollFlags::IN);
    let timespec = timeout.map(|d| duration_to_timespec(d));

    let ready = poll(&mut [fd], timespec.as_ref())?;
    if ready == 0 {
        return Ok(None);
    }

    let mut buf = [0u8; 1];
    let mut stdin = stdin.lock();

    if stdin.read(&mut buf)? == 0 {
        return Ok(None);
    }

    if buf[0] != b'\x1b' {
        return Ok(None);
    }

    // Expect '['
    if stdin.read(&mut buf)? == 0 || buf[0] != b'[' {
        return Ok(None);
    }

    // Collect parameter and intermediate bytes
    let mut params = Vec::new();
    let mut intermediates = Vec::new();
    let mut current_param = String::new();
    let mut has_question_mark = false;

    loop {
        if stdin.read(&mut buf)? == 0 {
            return Ok(None);
        }
        let b = buf[0];
        match b {
            b'?' => has_question_mark = true,
            b'0'..=b'9' => current_param.push(b as char),
            b';' => {
                params.push(current_param.clone());
                current_param.clear();
            }
            0x20..=0x2F => intermediates.push(b),
            0x40..=0x7E => {
                // Final byte
                if !current_param.is_empty() {
                    params.push(current_param);
                }
                return Ok(parse_csi(&params, &intermediates, has_question_mark, b));
            }
            _ => return Ok(None),
        }
    }
}

fn parse_csi(
    params: &[String],
    intermediates: &[u8],
    has_question_mark: bool,
    final_byte: u8,
) -> Option<InternalEvent> {
    // Resize notification: CSI 48 ; height_cells ; width_cells ; height_pixels ; width_pixels t
    if final_byte == b't' && params.first().is_some_and(|p| p == "48") && params.len() >= 3 {
        return Some(InternalEvent::Public(Event::Resize {
            height: params[1].parse::<u16>().ok()?,
            width: params[2].parse::<u16>().ok()?,
            // height_pixels: params[3].parse::<u16>().ok()?,
            // width_pixels: params[4].parse::<u16>().ok()?,
        }));
    }

    // DECRPM response: CSI ? mode ; value $ y
    if final_byte == b'y' && has_question_mark && intermediates == [b'$'] && params.len() == 2 {
        let mode = params[0].parse::<u16>().ok()?;
        let value = params[1].parse::<u8>().ok()?;
        return Some(InternalEvent::Decrpm { mode, value });
    }

    None
}

pub fn supports_in_band_resize() -> io::Result<bool> {
    let mut stdout = io::stdout().lock();
    stdout.write_all(escape::QUERY_IN_BAND_RESIZE.as_bytes())?;
    stdout.flush()?;

    let deadline = std::time::Instant::now() + Duration::from_secs(1);
    loop {
        let remaining = deadline.saturating_duration_since(std::time::Instant::now());
        if remaining.is_zero() {
            return Ok(false);
        }
        match read_event_internal(Some(remaining))? {
            Some(InternalEvent::Decrpm { mode: 2048, value }) => {
                return Ok(value == 1 || value == 2);
            }
            None => return Ok(false),
            _ => {}
        }
    }
}

pub fn supports_sync_update() -> io::Result<bool> {
    let mut stdout = io::stdout().lock();
    stdout.write_all(escape::QUERY_SYNC_UPDATE.as_bytes())?;
    stdout.flush()?;

    let deadline = std::time::Instant::now() + Duration::from_secs(1);
    loop {
        let remaining = deadline.saturating_duration_since(std::time::Instant::now());
        if remaining.is_zero() {
            return Ok(false);
        }
        match read_event_internal(Some(remaining))? {
            Some(InternalEvent::Decrpm { mode: 2026, value }) => {
                return Ok(value == 1 || value == 2);
            }
            None => return Ok(false),
            _ => {}
        }
    }
}
