use crate::{
    bios::Bios,
    cartridge::Cartridge,
    cpu::{Cpu, Cycles},
    display::EmuDisplay,
    memcontroller::{shared, MemoryController, Shared, CART_ROM_START},
    mmio::{
        graphics::{DisplayControlRegister, DisplayStatusRegister},
        IoRam,
    },
    ram::*,
};

const CYCLES_PER_FRAME: usize = Cpu::CYCLES_PER_SECOND / Cpu::FRAMES_PER_SECOND;
const HDRAW_CYCLES: Cycles = Cycles(960);
const HBLANK_CYCLES: Cycles = Cycles(272);
const H_PIXELS: usize = 240;
const V_PIXELS: usize = 160;

pub struct EmulatorMemory {
    // BIOS memory
    // 0x00 ~ 0x3FFF
    pub bios: Bios,

    // External work RAM
    // 0x200_0000 ~ 0x0203_FFFF
    pub ew_ram: Shared<EwRam>,

    // Internal work RAM
    // 0x300_0000 ~ 0x300_7FFF
    pub iw_ram: Shared<IwRam>,

    // Mem-mapped io registers
    // 0x400_0000 ~ 0x400_03FF
    pub io_ram: Shared<IoRam>,

    // Memory for two palettes with 256 entries of 15-bit colors each.
    // First is for background, second for sprites.
    // 0x500_0000 ~ 0x500_03FF
    pub pal_ram: Shared<PaletteRam>,

    // Video RAM. Data for backgrounds, sprites.
    // 0x600_0000 ~ 0x601_7FFF
    pub vram: Shared<Vram>,

    // Object Attribute Memory. For controlling sprites
    // 0x700_0000 ~ 0x700_03FF
    pub oam: Shared<Oam>,

    // Contains game data, and save data. Variable size, about 64kb.
    // 0x800_000 ~ var
    pub cartridge: Shared<Option<Cartridge>>,
}

impl EmulatorMemory {
    pub fn new() -> Self {
        Self {
            bios: Bios::new(),
            ew_ram: EwRam::new_shared(),
            iw_ram: IwRam::new_shared(),
            io_ram: IoRam::new_shared(),
            pal_ram: PaletteRam::new_shared(),
            vram: Vram::new_shared(),
            oam: Oam::new_shared(),
            cartridge: shared(None),
        }
    }

    pub fn insert_cartridge(&mut self, cartridge: Cartridge) {
        *self.cartridge.borrow_mut() = Some(cartridge);
        // TODO: Reset cpu
    }
}

pub struct Emulator<D: EmuDisplay> {
    cpu: Cpu,
    memory: EmulatorMemory,
    hdraw_counter: Cycles,
    display: D,
}

impl<D: EmuDisplay> Emulator<D> {
    pub fn new(display: D) -> Self {
        let memory = EmulatorMemory::new();
        let mem_controller = MemoryController::new(&memory);
        Emulator {
            cpu: Cpu::new(mem_controller, CART_ROM_START as u32),
            memory,
            hdraw_counter: Cycles(0),
            display,
        }
    }

    pub fn insert_cartridge(&mut self, cartridge: Cartridge) {
        self.memory.insert_cartridge(cartridge);
    }

    pub fn run(&mut self) -> ! {
        loop {
            self.hdraw_counter += self.cpu.tick();
            if self.hdraw_counter > HDRAW_CYCLES {}
        }
    }

    fn render_frame(&mut self) -> Vec<u8> {
        let mmio = self.memory.io_ram.borrow();
        let disp_cont = mmio.read_mmio_reg::<DisplayControlRegister>();
        let disp_stat = mmio.read_mmio_reg::<DisplayStatusRegister>();

        todo!("render frame")
    }
}
