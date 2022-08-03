use crate::{
    bus::{Bus, HALFWORD, WORD},
    cpu::Cycles,
    memcontroller::{shared, Shared, KB},
};

#[derive(Debug)]
pub struct Ram<const MEMSIZE: usize, const ACCESS_CYCLES: usize, const BUSWIDTH_BYTES: usize> {
    mem: Box<[u8; MEMSIZE]>,
}

impl<const MEMSIZE: usize, const ACCESS_CYCLES: usize, const BUSWIDTH_BYTES: usize>
    Ram<MEMSIZE, ACCESS_CYCLES, BUSWIDTH_BYTES>
{
    const ACCESS_TIME: Cycles = Cycles(ACCESS_CYCLES as u32);

    pub fn new_shared() -> Shared<Self> {
        shared(Self {
            mem: Box::new([0; MEMSIZE]),
        })
    }

    pub fn as_buffer(&self) -> &[u8] {
        &self.mem[..]
    }

    const fn access_cycles<const ACCESS_SIZE: usize>() -> Cycles {
        let access_bytes = ACCESS_SIZE;
        let accesses = if access_bytes > BUSWIDTH_BYTES { 2 } else { 1 };
        // Ugly, but custom Mul impl cannot be used in const fn
        Cycles(Self::ACCESS_TIME.0 * accesses)
    }
}

impl<const MEMSIZE: usize, const ACCESS_CYCLES: usize, const BUSWIDTH: usize> Bus
    for Ram<MEMSIZE, ACCESS_CYCLES, BUSWIDTH>
{
    fn read<const SIZE: usize>(&self, address: u32) -> ([u8; SIZE], Cycles) {
        let address = address as usize;
        let cycles = Self::access_cycles::<SIZE>();
        let range = address..(address + SIZE);
        let val: [u8; SIZE] = self.mem[range].try_into().unwrap();
        (val, cycles)
    }

    fn write<const SIZE: usize>(&mut self, address: u32, val: &[u8; SIZE]) -> Cycles {
        let address = address as usize;
        let range = address..(address + SIZE);
        self.mem[range].copy_from_slice(val);
        Self::access_cycles::<SIZE>()
    }
}

pub type EwRam = Ram<{ 256 * KB }, 3, HALFWORD>;

pub type IwRam = Ram<{ 32 * KB }, 1, WORD>;

pub type PaletteRam = Ram<{ KB }, 1, HALFWORD>;

pub type Vram = Ram<{ 96 * KB }, 1, HALFWORD>;

pub type Oam = Ram<{ KB }, 1, WORD>;
