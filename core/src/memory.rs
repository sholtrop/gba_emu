use rangemap::RangeMap;

const KB: usize = 1024;

const SYSTEM_ROM_START: u32 = 0x00;
const SYSTEM_ROM_END: u32 = 0x3FFF;

const EW_RAM_START: u32 = 0x200_0000;
const EW_RAM_END: u32 = 0x203_FFFF;

const IW_RAM_START: u32 = 0x300_0000;
const IW_RAM_END: u32 = 0x300_7FFF;

const IO_RAM_START: u32 = 0x400_0000;
const IO_RAM_END: u32 = 0x400_03FF;

const PAL_RAM_START: u32 = 0x500_0000;
const PAL_RAM_END: u32 = 0x500_03FF;

const VRAM_START: u32 = 0x600_0000;
const VRAM_END: u32 = 0x601_7FFF;

const OAM_START: u32 = 0x700_0000;
const OAM_END: u32 = 0x700_03FF;

const PAK_ROM_START: u32 = 0x800_0000;

const CART_RAM_START: u32 = 0xE00_0000;

pub(crate) struct EmulatorMemory {
    // BIOS memory
    // 0x00 ~ 0x3FFF
    system_rom: [u8; 16 * KB],

    // External work RAM
    // 0x200_0000 ~ 0x0203_FFFF
    ew_ram: [u8; 256 * KB],

    // Internal work RAM
    // 0x300_0000 ~ 0x300_7FFF
    iw_ram: [u8; 32 * KB],

    // Mem-mapped io registers
    // 0x400_0000 ~ 0x400_03FF
    io_ram: [u8; KB],

    // Memory for two palettes with 256 entries of 15-bit colors each.
    // First is for background, second for sprites.
    // 0x500_0000 ~ 0x500_03FF
    pal_ram: [u8; KB],

    // Video RAM. Data for backgrounds, sprites.
    // 0x600_0000 ~ 0x601_7FFF
    vram: [u8; 96 * KB],

    // Object Attribute Memory. For controlling sprites
    // 0x700_0000 ~ 0x700_03FF
    oam: [u8; KB],

    // Game Pak ROM. Variable size, but max. 32MB.
    // 0x800_0000 ~ var
    pak_rom: Vec<u8>,

    // Where save data is stored. Variable size, about 64kb.
    // 0xE00_000 ~ var
    cart_ram: Vec<u8>,
}

impl EmulatorMemory {
    pub fn new() -> Self {
        Self {
            system_rom: [0; 16 * KB],
            ew_ram: [0; 256 * KB],
            iw_ram: [0; 32 * KB],
            io_ram: [0; KB],
            pal_ram: [0; KB],
            vram: [0; 96 * KB],
            oam: [0; KB],
            pak_rom: vec![],
            cart_ram: vec![],
        }
    }

    pub fn read(&self, address: u32) -> u8 {
        match address {
            SYSTEM_ROM_START..=SYSTEM_ROM_END => self.system_rom[address as usize],
            EW_RAM_START..=EW_RAM_END => self.ew_ram[(address - EW_RAM_START) as usize],
            IW_RAM_START..=IW_RAM_END => self.iw_ram[(address - IW_RAM_START) as usize],
            IO_RAM_START..=IO_RAM_END => self.io_ram[(address - IO_RAM_START) as usize],
            PAL_RAM_START..=PAL_RAM_END => self.pal_ram[(address - PAL_RAM_START) as usize],
            VRAM_START..=VRAM_END => self.vram[(address - VRAM_START) as usize],
            OAM_START..=OAM_END => self.oam[(address - OAM_START) as usize],
            PAK_ROM_START..=CART_RAM_START => self.pak_rom[(address - PAK_ROM_START) as usize],
            _ => self.cart_ram[(address - CART_RAM_START) as usize],
        }
    }
}
