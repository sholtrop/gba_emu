use std::{cmp::Ordering, mem::size_of};

use crate::{
    memcontroller::{Shared, KB},
    mmio::{
        background::{BgControlRegister, BgRotScaleRegister, BgScrollDoubleReg},
        graphics::{DisplayControlRegister, VideoMode},
        IoRam,
    },
    ram::{Oam, PaletteRam, Vram},
};
use modular_bitfield::prelude::*;

const BG_MAP_TILE_SIZE: usize = 96 * KB;
const BG_OBJ_TILE_SIZE: usize = 32 * KB;

/// A char block holds a tileset
const CHAR_BLOCK_SIZE: usize = 16 * KB;

/// A screen block holds a tilemap
const SCREEN_BLOCK_SIZE: usize = 2 * KB;

const TILE_MAP_ENTRY_SIZE: usize = size_of::<TextBgTileMapEntry>();

/// In [VideoMode::Mode0] or [VideoMode::Mode1], interpret every 2 bytes in the BG map area as this entry.
#[bitfield(bits = 16)]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct TextBgTileMapEntry {
    pub tile_nr: B10,
    pub hori_flip: B1,
    pub vert_flip: B1,
    pub pal_nr: B4,
}

pub struct RenderContext {
    io_ram: Shared<IoRam>,
    vram: Shared<Vram>,
    pal_ram: Shared<PaletteRam>,
    oam: Shared<Oam>,
}

pub struct Renderer {}

impl Renderer {
    pub fn render_frame(&mut self, pixel_buffer: &mut [u8], render_ctx: &RenderContext) {
        let disp_ctrl = render_ctx
            .io_ram
            .borrow()
            .read_mmio_reg::<DisplayControlRegister>();

        if disp_ctrl.force_blank_display() == 1 {
            todo!("Fill pixel buffer with white pixels")
        }

        match disp_ctrl.video_mode() {
            VideoMode::Mode0 => self.render_mode0(pixel_buffer, render_ctx),
            VideoMode::Mode1 => {
                todo!("Render mode1")
            }
            VideoMode::Mode2 => {
                todo!("Render mode2")
            }
            VideoMode::Mode3 => {
                todo!("Render mode3")
            }
            VideoMode::Mode4 => {
                todo!("Render mode4")
            }
            VideoMode::Mode5 => {
                todo!("Render mode5")
            }
        }
    }

    fn render_mode0(
        &mut self,
        pixel_buffer: &mut [u8],
        RenderContext {
            io_ram,
            oam,
            pal_ram,
            vram,
        }: &RenderContext,
    ) {
        let io_ram = io_ram.borrow();
        let vram = vram.borrow();

        let disp_ctrl = io_ram.read_mmio_reg::<DisplayControlRegister>();
        let bg_scroll_regs = io_ram.read_mmio_reg::<BgScrollDoubleReg>();
        let visible_bgs = [
            disp_ctrl.display_bg0(),
            disp_ctrl.display_bg1(),
            disp_ctrl.display_bg2(),
            disp_ctrl.display_bg3(),
        ];

        // Bg's are rendered in opposite `priority` order so higher prio bg's can overwrite lower prio ones
        let bg_control_regs = {
            let mut regs = io_ram.read_mmio_reg::<BgControlRegister>();
            regs.sort_by(|reg1, reg2| {
                reg1.priority()
                    .partial_cmp(&reg2.priority())
                    .unwrap_or(Ordering::Greater)
                    .reverse()
            });
            regs
        };

        let (bg_tileset_and_map, obj_tiles) = vram.as_buffer().split_at(BG_MAP_TILE_SIZE);

        // TODO:
        // 1. Get OBJs
        // 2. Determine prio of all entities together, render will happen in opposite order
        // 3. Determine windows, which BGs & OBJs are inside/outside
        // 4. OBJ rotation/scale
        // 5. Special effects/mosaic

        for (bg_nr, bg) in bg_control_regs.iter().enumerate() {
            if visible_bgs[bg_nr] == 0 {
                // This background is not visible, so don't render
                continue;
            }
            let bg_tile_map_size = bg.tile_map_size().text_mode_size();
            let char_start = bg.char_base_block() as usize * CHAR_BLOCK_SIZE;
            let screen_start = bg.screen_base_block() as usize * SCREEN_BLOCK_SIZE;
            let screen_end = screen_start + bg_tile_map_size;
            let (bg_scroll_x, bg_scroll_y) = {
                let bg_scroll = &bg_scroll_regs[bg_nr];
                (bg_scroll.x_offset(), bg_scroll.y_offset())
            };
            let bg_tile_map = &bg_tileset_and_map[screen_start..screen_end];

            for entry in bg_tile_map.chunks(TILE_MAP_ENTRY_SIZE) {
                let entry = TextBgTileMapEntry::from_bytes(entry.try_into().unwrap());
            }
            // TODO: Figure out how color palettes work?
            // TODO: Figure out how windowing works?
        }
        // [0..BG_MAP_TILE_SIZE];

        // let obj_tiles = &vram.as_buffer()[BG_MAP_TILE_SIZE..BG_OBJ_TILE_SIZE];
    }
}
