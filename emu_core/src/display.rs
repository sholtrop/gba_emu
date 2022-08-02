use crate::ram::{Oam, PaletteRam, Vram};

pub trait EmuDisplay {
    fn draw_frame(&mut self, frame: &[u8]);
}

pub struct DummyDisplay {}

impl EmuDisplay for DummyDisplay {
    fn draw_frame(&mut self, _frame: &[u8]) {}
}
