use crate::mmio::{background::BgControlRegister, graphics::DisplayControlRegister};

pub struct RenderContext {
    disp_cont_reg: DisplayControlRegister,
    bg_cont_regs: [BgControlRegister; 4],
    // bg_scroll_regs: [Background]
}

pub struct Renderer {}

impl Renderer {
    pub fn render_frame(&mut self, buffer: &mut &[u8], render_ctx: &RenderContext) {}
}
