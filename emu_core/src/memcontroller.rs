use std::{cell::RefCell, rc::Rc};

use crate::{
    bios::Bios,
    bus::{Bus, BYTE, HALFWORD, WORD},
    cartridge::Cartridge,
    cycles::Cycles,
    emulator::EmulatorMemory,
    mmio::IoRam,
    ram::{EwRam, IwRam, Oam, PaletteRam, Vram},
};

pub const KB: usize = 1024;

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
pub const CART_SRAM_START: usize = 0xE00_0000;

pub type Shared<T> = Rc<RefCell<T>>;

pub fn shared<T>(t: T) -> Shared<T> {
    Rc::new(RefCell::new(t))
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
pub struct MemoryController {
    // BIOS memory
    // 0x00 ~ 0x3FFF
    bios: Bios,

    // External work RAM
    // 0x200_0000 ~ 0x0203_FFFF
    ew_ram: Shared<EwRam>,

    // Internal work RAM
    // 0x300_0000 ~ 0x300_7FFF
    iw_ram: Shared<IwRam>,

    // Mem-mapped io registers
    // 0x400_0000 ~ 0x400_03FF
    io_ram: Shared<IoRam>,

    // Memory for two palettes with 256 entries of 15-bit colors each.
    // First is for background, second for sprites.
    // 0x500_0000 ~ 0x500_03FF
    pal_ram: Shared<PaletteRam>,

    // Video RAM. Data for backgrounds, sprites.
    // 0x600_0000 ~ 0x601_7FFF
    vram: Shared<Vram>,

    // Object Attribute Memory. For controlling sprites
    // 0x700_0000 ~ 0x700_03FF
    oam: Shared<Oam>,

    // Where save data is stored. Variable size, about 64kb.
    // 0x800_000 ~ var
    cartridge: Shared<Option<Cartridge>>,

    /// Address of the last performed mem read/write
    /// Used to determine whether current access is sequential (S) or non-sequential (N)
    last_access_addr: u32,
}

impl MemoryController {
    pub fn new(
        EmulatorMemory {
            bios,
            ew_ram,
            iw_ram,
            io_ram,
            pal_ram,
            vram,
            oam,
            cartridge,
        }: &EmulatorMemory,
    ) -> Self {
        Self {
            bios: bios.clone(),
            ew_ram: ew_ram.clone(),
            iw_ram: iw_ram.clone(),
            io_ram: io_ram.clone(),
            pal_ram: pal_ram.clone(),
            vram: vram.clone(),
            oam: oam.clone(),
            cartridge: cartridge.clone(),
            last_access_addr: 0x0,
        }
    }

    fn log_write(&self, size: usize, address: u32, value: &[u8]) {
        match size {
            BYTE => {
                log::debug!("Write {:#X} to {:#X}", value[0], address);
            }
            HALFWORD => {
                log::debug!(
                    "Write {:#X} to {:#X}",
                    u16::from_le_bytes(value[0..2].try_into().unwrap()),
                    address
                );
            }
            WORD => {
                log::debug!(
                    "Write {:#X} to {:#X}",
                    u32::from_le_bytes(value[0..4].try_into().unwrap()),
                    address
                );
            }
            _ => {}
        }
    }

    fn log_read(&self, size: usize, address: u32, value: &[u8]) {
        match size {
            BYTE => {
                log::debug!("Read {:#X} from {:#X}", value[0], address);
            }
            HALFWORD => {
                log::debug!(
                    "Read {:#X} from {:#X}",
                    u16::from_le_bytes(value[0..2].try_into().unwrap()),
                    address
                );
            }
            WORD => {
                log::debug!(
                    "Read {:#X} from {:#X}",
                    u32::from_le_bytes(value[0..4].try_into().unwrap()),
                    address
                );
            }
            _ => {}
        }
    }

    /// Returns the [MemoryRegion] corresponding to this address along with an address used to index it, according to the passed in `address`.
    fn get_mem_region(&self, address: u32) -> (MemoryRegion, u32) {
        // TODO: error handling on invalid addresses
        let region_bits = address >> 24 << 24;
        let region = match region_bits as usize {
            SYSTEM_ROM_START => MemoryRegion::System,
            EW_RAM_START => MemoryRegion::EwRam,
            IW_RAM_START => MemoryRegion::IwRam,
            IO_RAM_START => MemoryRegion::IoRam,
            PAL_RAM_START => MemoryRegion::PalRam,
            VRAM_START => MemoryRegion::Vram,
            OAM_START => MemoryRegion::Oam,
            CART_ROM_START | CART_SRAM_START => MemoryRegion::Cartridge,
            _ => unreachable!("`region_bits` did not correspond to any memory region"),
        };
        let offset_addr = address - region_bits;
        (region, offset_addr)
    }
}

impl Bus for MemoryController {
    fn read<const SIZE: usize>(&self, address: u32) -> ([u8; SIZE], Cycles) {
        let size = SIZE;
        let (value, cycles) = match self.get_mem_region(address) {
            (MemoryRegion::System, address) => self.bios.read(address),
            (MemoryRegion::EwRam, address) => self.ew_ram.borrow().read(address),
            (MemoryRegion::IwRam, address) => self.iw_ram.borrow().read(address),
            (MemoryRegion::IoRam, address) => self.io_ram.borrow().read(address),
            (MemoryRegion::PalRam, address) => self.pal_ram.borrow().read(address),
            (MemoryRegion::Vram, address) => self.vram.borrow().read(address),
            (MemoryRegion::Oam, address) => self.oam.borrow().read(address),
            (MemoryRegion::Cartridge, address) => self
                .cartridge
                .borrow()
                .as_ref()
                .expect("No cartridge inserted")
                .read(address),
        };
        self.log_read(size, address, &value);
        (value, cycles)
    }

    fn write<const SIZE: usize>(&mut self, address: u32, value: &[u8; SIZE]) -> Cycles {
        let size = SIZE;
        self.log_write(size, address, value);
        match self.get_mem_region(address) {
            (MemoryRegion::System, address) => self.bios.write(address, value),
            (MemoryRegion::EwRam, address) => (*self.ew_ram).borrow_mut().write(address, value),
            (MemoryRegion::IwRam, address) => (*self.iw_ram).borrow_mut().write(address, value),
            (MemoryRegion::IoRam, address) => (*self.io_ram).borrow_mut().write(address, value),
            (MemoryRegion::PalRam, address) => (*self.pal_ram).borrow_mut().write(address, value),
            (MemoryRegion::Vram, address) => (*self.vram).borrow_mut().write(address, value),
            (MemoryRegion::Oam, address) => (*self.oam).borrow_mut().write(address, value),
            (MemoryRegion::Cartridge, address) => (*self.cartridge)
                .borrow_mut()
                .as_mut()
                .expect("No cartridge inserted")
                .write(address, value),
        }
    }
}
