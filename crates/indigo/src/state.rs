#[derive(Default)]
pub struct State {
    pub mouse_capture: Option<MouseCapture>,
}

#[derive(Clone, Copy)]
pub enum MouseCapture {
    ScrollBarThumb {
        row_at_grab: u16,
        thumb_start_at_grab: usize,
    },
    ScrollBar,
}
