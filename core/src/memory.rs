use std::{
    cell::RefCell,
    ops::{Add, AddAssign},
    rc::Rc,
};

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

pub const CART_ROM_START: usize = 0x800_0000;
const CART_SRAM_START: usize = 0xE00_0000;

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

enum MemoryRegion {
    System,
    EwRam,
    IwRam,
    IoRam,
    PalRam,
    Vram,
    Oam,
    Cartridge,
}

#[derive(Debug)]
pub struct GbaMemory {
    // BIOS memory
    // 0x00 ~ 0x3FFF
    system_rom: Box<[u8; 16 * KB]>,

    // External work RAM
    // 0x200_0000 ~ 0x0203_FFFF
    ew_ram: Box<[u8; 256 * KB]>,

    // Internal work RAM
    // 0x300_0000 ~ 0x300_7FFF
    iw_ram: Box<[u8; 32 * KB]>,

    // Mem-mapped io registers
    // 0x400_0000 ~ 0x400_03FF
    io_ram: Box<[u8; KB]>,

    // Memory for two palettes with 256 entries of 15-bit colors each.
    // First is for background, second for sprites.
    // 0x500_0000 ~ 0x500_03FF
    pal_ram: Box<[u8; KB]>,

    // Video RAM. Data for backgrounds, sprites.
    // 0x600_0000 ~ 0x601_7FFF
    vram: Box<[u8; 96 * KB]>,

    // Object Attribute Memory. For controlling sprites
    // 0x700_0000 ~ 0x700_03FF
    oam: Box<[u8; KB]>,

    // Where save data is stored. Variable size, about 64kb.
    // 0xE00_000 ~ var
    cartridge: Cartridge,

    /// Address of the last performed mem read/write
    /// Used to determine whether current access is sequential (S) or non-sequential (N)
    last_access_addr: u32,
}

impl GbaMemory {
    pub fn new() -> Rc<RefCell<Self>> {
        Rc::new(RefCell::new(Self {
            system_rom: Box::new([0; 16 * KB]),
            ew_ram: Box::new([0; 256 * KB]),
            iw_ram: Box::new([0; 32 * KB]),
            io_ram: Box::new([0; KB]),
            pal_ram: Box::new([0; KB]),
            vram: Box::new([0; 96 * KB]),
            oam: Box::new([0; KB]),
            cartridge: Cartridge::new(),
            last_access_addr: 0x0,
        }))
    }

    pub fn write_byte(&mut self, address: u32, value: u8) -> CycleCost {
        self.write::<BYTE>(address, &value.to_le_bytes())
    }

    pub fn write_le_halfword(&mut self, address: u32, value: u16) -> CycleCost {
        self.write::<HALFWORD>(address, &value.to_le_bytes())
    }

    pub fn write_le_word(&mut self, address: u32, value: u32) -> CycleCost {
        self.write::<WORD>(address, &value.to_le_bytes())
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

    pub fn insert_cartridge(&mut self, cartridge: Cartridge) {
        self.cartridge = cartridge;
    }

    fn read<const SIZE: usize>(&self, address: u32) -> ([u8; SIZE], CycleCost) {
        let (src, start_addr): (&[u8], usize) = match self.get_mem_region(address) {
            (MemoryRegion::System, addr) => (&*self.system_rom, addr),
            (MemoryRegion::EwRam, addr) => (&*self.ew_ram, addr),
            (MemoryRegion::IwRam, addr) => (&*self.iw_ram, addr),
            (MemoryRegion::IoRam, addr) => (&*self.io_ram, addr),
            (MemoryRegion::PalRam, addr) => (&*self.pal_ram, addr),
            (MemoryRegion::Vram, addr) => (&*self.vram, addr),
            (MemoryRegion::Oam, addr) => (&*self.oam, addr),
            (MemoryRegion::Cartridge, addr) => (self.cartridge.mem(), addr),
        };
        let end_addr = start_addr + SIZE;
        let range = start_addr..end_addr;

        let bytes: [u8; SIZE] = src[range].try_into().unwrap();
        // TODO: Actual cost
        let cost = CycleCost(1);
        if SIZE == 4 {
            println!(
                "Read {:#X} from {:#X}",
                u32::from_le_bytes(bytes[0..4].try_into().unwrap()),
                address
            );
        }
        (bytes, cost)
    }

    fn write<const SIZE: usize>(&mut self, address: u32, value: &[u8; SIZE]) -> CycleCost {
        if SIZE == 4 {
            println!(
                "Write {:#X} to {:#X}",
                u32::from_le_bytes(value[0..4].try_into().unwrap()),
                address
            );
        }
        let (src, mut addr): (&mut [u8], usize) = match self.get_mem_region(address) {
            (MemoryRegion::System, addr) => (&mut *self.system_rom, addr),
            (MemoryRegion::EwRam, addr) => (&mut *self.ew_ram, addr),
            (MemoryRegion::IwRam, addr) => (&mut *self.iw_ram, addr),
            (MemoryRegion::IoRam, addr) => (&mut *self.io_ram, addr),
            (MemoryRegion::PalRam, addr) => (&mut *self.pal_ram, addr),
            (MemoryRegion::Vram, addr) => (&mut *self.vram, addr),
            (MemoryRegion::Oam, addr) => (&mut *self.oam, addr),
            (MemoryRegion::Cartridge, addr) => (self.cartridge.mem_mut(), addr),
        };
        // dbg!("WRITE", &addr);
        for byte in value.iter() {
            src[addr] = *byte;
            addr += 1;
        }
        // TODO: Actual cost
        CycleCost(1)
    }

    /// Returns the [MemoryRegion] corresponding to this address along with an address used to index it, according to the passed in `address`.
    fn get_mem_region(&self, address: u32) -> (MemoryRegion, usize) {
        // TODO: error handling on invalid addresses
        let address = address as usize;
        let region_bits = (address >> 24 << 24) as usize;
        let region = match region_bits {
            SYSTEM_ROM_START => MemoryRegion::System,
            EW_RAM_START => MemoryRegion::EwRam,
            IW_RAM_START => MemoryRegion::IwRam,
            IO_RAM_START => MemoryRegion::IoRam,
            PAL_RAM_START => MemoryRegion::PalRam,
            VRAM_START => MemoryRegion::Vram,
            OAM_START => MemoryRegion::Oam,
            _ => MemoryRegion::Cartridge,
        };
        let offset_addr = address - region_bits;
        (region, offset_addr)
    }
}

// pub mod test {
//     use super::*;

//     const TEST_MEM_SIZE: usize = 1024 * KB;
//     pub struct TestMemory {
//         mem: Box<[u8]>,
//     }

//     impl TestMemory {
//         pub fn new() -> Rc<RefCell<Self>> {
//             Rc::new(RefCell::new(Self {
//                 mem: vec![0; TEST_MEM_SIZE].into_boxed_slice(),
//             }))
//         }

//         pub fn load_program(&mut self, program: Vec<u8>) {
//             for (idx, byte) in program.iter().enumerate() {
//                 self.mem[idx] = *byte;
//             }
//         }
//     }

//     impl Memory for TestMemory {
//         fn write_byte(&mut self, address: u32, value: u8) -> CycleCost {
//             self.mem[address as usize] = value;
//             CycleCost(0)
//         }

//         fn write_le_halfword(&mut self, mut address: u32, value: u16) -> CycleCost {
//             let bytes = value.to_le_bytes();
//             for b in bytes {
//                 self.mem[address as usize] = b;
//                 address += 1;
//             }
//             CycleCost(0)
//         }

//         fn write_le_word(&mut self, mut address: u32, value: u32) -> CycleCost {
//             let bytes = value.to_le_bytes();
//             for b in bytes {
//                 self.mem[address as usize] = b;
//                 address += 1;
//             }
//             CycleCost(0)
//         }

//         fn read_byte(&self, address: u32) -> (u8, CycleCost) {
//             (self.mem[address as usize], CycleCost(0))
//         }

//         fn read_le_halfword(&self, address: u32) -> (u16, CycleCost) {
//             let address = address as usize;
//             (
//                 u16::from_le_bytes(self.mem[address..address + 2].try_into().unwrap()),
//                 CycleCost(0),
//             )
//         }

//         fn read_le_word(&self, address: u32) -> (u32, CycleCost) {
//             let address = address as usize;
//             (
//                 u32::from_le_bytes(self.mem[address..address + 4].try_into().unwrap()),
//                 CycleCost(0),
//             )
//         }
//     }
// }
