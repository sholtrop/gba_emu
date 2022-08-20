// TODO:
// Split memory into a MemoryController that then accesses several memories (RAM, ROM, cartridge, hardware)

use crate::cycles::Cycles;

pub const BYTE: usize = 1;
pub const HALFWORD: usize = 2;
pub const WORD: usize = 4;

pub trait Bus {
    fn read<const T: usize>(&self, address: u32) -> ([u8; T], Cycles);

    fn write<const T: usize>(&mut self, address: u32, val: &[u8; T]) -> Cycles;

    fn write_byte(&mut self, address: u32, value: u8) -> Cycles {
        self.write::<BYTE>(address, &value.to_le_bytes())
    }

    fn write_le_halfword(&mut self, address: u32, value: u16) -> Cycles {
        self.write::<HALFWORD>(address, &value.to_le_bytes())
    }

    fn write_le_word(&mut self, address: u32, value: u32) -> Cycles {
        self.write::<WORD>(address, &value.to_le_bytes())
    }

    fn read_byte(&self, address: u32) -> (u8, Cycles) {
        let (val, cycles) = self.read::<BYTE>(address);
        (val[0], cycles)
    }

    fn read_le_halfword(&self, address: u32) -> (u16, Cycles) {
        let (val, cycles) = self.read::<HALFWORD>(address);
        (u16::from_le_bytes(val), cycles)
    }

    fn read_le_word(&self, address: u32) -> (u32, Cycles) {
        let (val, cycles) = self.read::<WORD>(address);
        (u32::from_le_bytes(val), cycles)
    }
}
