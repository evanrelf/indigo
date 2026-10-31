use std::{
    cmp::min,
    sync::LazyLock,
    time::{Duration, Instant},
};

const FRAMES_PER_SECOND: u32 = 60;

const FRAME_INTERVAL: Duration = Duration::from_secs(1)
    .checked_div(FRAMES_PER_SECOND)
    .unwrap();

/// Shared starting point for animations, so they stay in sync with each other.
static EPOCH: LazyLock<Instant> = LazyLock::new(Instant::now);

/// Time as seen while drawing a frame, plus requests for when to draw the next one.
pub struct Clock {
    /// When this frame is being drawn.
    now: Instant,
    /// Earliest time a redraw has been requested for.
    wakeup: Option<Instant>,
}

impl Clock {
    #[must_use]
    pub fn now() -> Self {
        Self {
            now: Instant::now(),
            wakeup: None,
        }
    }

    #[must_use]
    pub fn wakeup(&self) -> Option<Instant> {
        self.wakeup
    }

    /// Request a redraw at `instant`.
    pub fn wake_at(&mut self, instant: Instant) {
        self.wakeup = Some(self.wakeup.map_or(instant, |wakeup| min(wakeup, instant)));
    }

    /// Time into the current cycle of a repeating animation, from zero up to `period`.
    ///
    /// Requests a redraw on the next frame, since the result changes continuously.
    pub fn cycle(&mut self, period: Duration) -> Duration {
        self.wake_at(self.now + FRAME_INTERVAL);
        let elapsed = self.now.saturating_duration_since(*EPOCH);
        let nanos = elapsed.as_nanos() % period.as_nanos();
        Duration::from_nanos(u64::try_from(nanos).expect("Period is shorter than 584 years"))
    }
}
