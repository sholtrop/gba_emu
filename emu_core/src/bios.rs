use crate::{bus::Bus, cpu::Cycles};

// TODO: write
#[derive(Debug, Clone)]
pub struct Bios {}

impl Bios {
    pub fn new() -> Self {
        Self {}
    }
}

impl Bus for Bios {
    /// No-op, BIOS cannot be written
    fn write<const T: usize>(&mut self, address: u32, val: &[u8; T]) -> Cycles {
        Cycles(0)
    }

    fn read<const T: usize>(&self, address: u32) -> ([u8; T], Cycles) {
        // Reads will give instruction after inst. used to read
        todo!("Write bios")
    }
}
