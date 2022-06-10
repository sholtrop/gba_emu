use std::ops::{Add, AddAssign};

use bitvec::macros::internal::funty::Numeric;

use crate::cartridge::Cartridge;

const KB: usize = 1024;

const SYSTEM_ROM_START: usize = 0x00;
const SYSTEM_ROM_END: usize = 0x3FFF;

const EW_RAM_START: usize = 0x200_0000;
const EW_RAM_END: usize = 0x203_FFFF;

const IW_RAM_START: usize = 0x300_0000;
const IW_RAM_END: usize = 0x300_7FFF;

const IO_RAM_START: usize = 0x400_0000;
const IO_RAM_END: usize = 0x400_03FF;

const PAL_RAM_START: usize = 0x500_0000;
const PAL_RAM_END: usize = 0x500_03FF;

const VRAM_START: usize = 0x600_0000;
const VRAM_END: usize = 0x601_7FFF;

const OAM_START: usize = 0x700_0000;
const OAM_END: usize = 0x700_03FF;

const PAK_ROM_START: usize = 0x800_0000;

const CART_RAM_START: usize = 0xE00_0000;

pub const BYTE: usize = 1;
pub const HALFWORD: usize = 2;
pub const WORD: usize = 4;

#[derive(Clone, Copy, Debug)]
pub struct CycleCost(pub u8);

impl Add for CycleCost {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Self(self.0 + rhs.0)
    }
}

impl AddAssign for CycleCost {
    fn add_assign(&mut self, rhs: Self) {
        self.0 += rhs.0;
    }
}

#[derive(Debug)]
pub struct GbaMemory {
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
    cartridge: Cartridge,

    /// Address of the last performed mem read/write
    /// Used to determine whether current access is sequential (S) or non-sequential (N)
    last_access_addr: u32,
}

impl GbaMemory {
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
            cartridge: Cartridge::new(),
            last_access_addr: 0x0,
        }
    }

    pub fn read<const SIZE: usize>(&self, address: u32) -> ([u8; SIZE], CycleCost) {
        let address = address as usize;
        let (offset, src): (usize, &[u8]) = match address {
            SYSTEM_ROM_START..=SYSTEM_ROM_END => (0, &self.system_rom),
            EW_RAM_START..=EW_RAM_END => (EW_RAM_START, &self.ew_ram),
            IW_RAM_START..=IW_RAM_END => (IW_RAM_START, &self.iw_ram),
            IO_RAM_START..=IO_RAM_END => (IO_RAM_START, &self.io_ram),
            PAL_RAM_START..=PAL_RAM_END => (PAL_RAM_START, &self.pal_ram),
            VRAM_START..=VRAM_END => (VRAM_START, &self.vram),
            OAM_START..=OAM_END => (OAM_START, &self.oam),
            PAK_ROM_START..=CART_RAM_START => (PAK_ROM_START, &self.pak_rom),
            _ => (CART_RAM_START, self.cartridge.ram()),
        };
        let range = (address - offset)..(address - offset + SIZE);
        let bytes: [u8; SIZE] = src[range].try_into().unwrap();
        // TODO: Actual cost
        let cost = CycleCost(1);

        (bytes, cost)
    }

    pub fn read_byte(&self, address: u32) -> (u8, CycleCost) {
        let (val, cycles) = self.read::<BYTE>(address);
        (val[0], cycles)
    }

    pub fn read_le_halfword(&self, address: u32) -> (u16, CycleCost) {
        let (val, cycles) = self.read::<HALFWORD>(address);
        (u16::from_le_bytes(val), cycles)
    }

    pub fn read_le_word(&self, address: u32) -> (u32, CycleCost) {
        let (val, cycles) = self.read::<WORD>(address);
        (u32::from_le_bytes(val), cycles)
    }

    pub fn write<const SIZE: usize>(&mut self, address: u32, value: [u8; SIZE]) -> CycleCost {
        CycleCost(1)
    }

    pub fn write_byte(&mut self, address: u32, value: u8) -> CycleCost {
        self.write::<BYTE>(address, value.to_le_bytes())
    }

    pub fn write_le_halfword(&mut self, address: u32, value: u16) -> CycleCost {
        self.write::<HALFWORD>(address, value.to_le_bytes())
    }

    pub fn write_le_word(&mut self, address: u32, value: u32) -> CycleCost {
        self.write::<WORD>(address, value.to_le_bytes())
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum MemSize {
    Byte = 1,
    Halfword = 2,
    Word = 4,
}

#[derive(Clone, PartialEq, Eq)]
pub struct MemWrite {
    pub address: u32,
    pub value: u32,
    pub size: MemSize,
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct MemRead {
    pub address: u32,
    pub size: MemSize,
}
