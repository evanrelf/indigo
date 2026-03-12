use illuso_term::{KeyboardEnhancementFlags as KEF, *};
use std::{
    collections::VecDeque,
    io::{self, Write},
};

pub struct Terminal {
    tty: Tty,
    reader: Reader,
    events: VecDeque<Event>,
}

impl Terminal {
    pub fn init() -> io::Result<Self> {
        let mut this = Self {
            tty: Tty::init()?,
            reader: Reader::new(),
            events: VecDeque::new(),
        };

        assert!(
            this.enable_mode(IN_BAND_RESIZE_MODE)?,
            "terminal does not support in-band resize"
        );
        write!(this.tty, "{}", IN_BAND_RESIZE_SET)?;
        this.tty.flush()?;

        assert!(
            this.query_keyboard_flags()?.is_some(),
            "terminal does not support kitty keyboard protocol"
        );
        write!(
            this.tty,
            "{}",
            // NOTE: All these flags must be enabled to get full keyboard functionality, since the
            // raw/legacy keys are intentionally not supported.
            KeyboardEnhancementFlagsPush(
                KEF::DISAMBIGUATE | KEF::REPORT_EVENTS | KEF::REPORT_ALL_KEYS
            )
        )?;

        Ok(this)
    }

    pub fn query_mode(&mut self, mode: u16) -> io::Result<ModeSetting> {
        write!(self.tty, "{}{}", ModeQuery(mode), DA1_QUERY)?;
        self.tty.flush()?;

        let mut setting = ModeSetting::NotRecognized;

        loop {
            // We record the setting from the reply, but continue reading events until a DA1 event
            // is consumed. Leaving it in the event stream would confuse future queries.
            match self.reader.read_event(&mut self.tty)? {
                Event::Decrpm {
                    mode: event_mode,
                    setting: event_setting,
                } if mode == event_mode => setting = event_setting,
                Event::Da1 => return Ok(setting),
                event => self.events.push_back(event),
            }
        }
    }

    pub fn enable_mode(&mut self, mode: u16) -> io::Result<bool> {
        #[allow(clippy::enum_glob_use)]
        use ModeSetting::*;

        match self.query_mode(mode)? {
            NotRecognized | PermanentlyReset => Ok(false),
            Set | PermanentlySet => Ok(true),
            Reset => {
                write!(self.tty, "{}", ModeSet(mode))?;
                self.tty.flush()?;
                Ok(true)
            }
        }
    }

    pub fn disable_mode(&mut self, mode: u16) -> io::Result<bool> {
        #[allow(clippy::enum_glob_use)]
        use ModeSetting::*;

        match self.query_mode(mode)? {
            NotRecognized | Reset | PermanentlyReset => Ok(true),
            PermanentlySet => Ok(false),
            Set => {
                write!(self.tty, "{}", ModeReset(mode))?;
                self.tty.flush()?;
                Ok(true)
            }
        }
    }

    pub fn query_keyboard_flags(&mut self) -> io::Result<Option<KeyboardEnhancementFlags>> {
        write!(
            self.tty,
            "{}{}",
            KEYBOARD_ENHANCEMENT_FLAGS_QUERY, DA1_QUERY
        )?;
        self.tty.flush()?;

        let mut flags = None;

        loop {
            // We record the setting from the reply, but continue reading events until a DA1 event
            // is consumed. Leaving it in the event stream would confuse future queries.
            match self.reader.read_event(&mut self.tty)? {
                Event::KeyboardEnhancementFlags(event_flags) => flags = Some(event_flags),
                Event::Da1 => return Ok(flags),
                event => self.events.push_back(event),
            }
        }
    }

    pub fn push_keyboard_flags(&mut self, flags: KeyboardEnhancementFlags) -> io::Result<()> {
        write!(self.tty, "{}", KeyboardEnhancementFlagsPush(flags))?;
        self.tty.flush()?;
        Ok(())
    }

    pub fn pop_keyboard_flags(&mut self) -> io::Result<()> {
        write!(self.tty, "{}", KeyboardEnhancementFlagsPop)?;
        self.tty.flush()?;
        Ok(())
    }

    pub fn size(&self) -> io::Result<(u16, u16)> {
        self.tty.size()
    }

    pub fn read_event(&mut self) -> io::Result<Event> {
        if let Some(event) = self.events.pop_front() {
            Ok(event)
        } else {
            self.reader.read_event(&mut self.tty)
        }
    }
}

impl Write for Terminal {
    #[inline]
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        self.tty.write(buf)
    }

    #[inline]
    fn flush(&mut self) -> io::Result<()> {
        self.tty.flush()
    }
}

impl Drop for Terminal {
    fn drop(&mut self) {
        let _ = write!(self.tty, "{}", IN_BAND_RESIZE_RESET);
        let _ = write!(self.tty, "{}", KeyboardEnhancementFlagsPop);
        let _ = self.tty.flush();
    }
}
