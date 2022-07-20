const GBA_WIDTH: usize = 280;
const GBA_HEIGHT: usize = 160;

pub struct GbaScreen {
    buffer: Box<[u16; GBA_WIDTH * GBA_HEIGHT]>,
}

impl GbaScreen {}
