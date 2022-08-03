use std::fs;
use std::path::Path;

use crate::bus::{Bus, BYTE, HALFWORD, WORD};
use crate::cpu::Cycles;
use crate::memcontroller::CART_SRAM_START;

const CARTRIDGE_HEADER_SIZE: usize = 192;

// const GAMEPAK_ROM_START: usize = 0x08000000;
// const GAMEPAK_SRAM_START: usize = 0x0E000000;
// GamePak ROM    16     8/16/32   -          5/5/8 **/***
//  GamePak Flash 16     8/16/32   16/32      5/5/8 **/***
//  GamePak SRAM  8      8         8          5     **

// External Memory (Game Pak)

//   08000000-09FFFFFF   Game Pak ROM/FlashROM (max 32MB) - Wait State 0
//   0A000000-0BFFFFFF   Game Pak ROM/FlashROM (max 32MB) - Wait State 1
//   0C000000-0DFFFFFF   Game Pak ROM/FlashROM (max 32MB) - Wait State 2
//   0E000000-0E00FFFF   Game Pak SRAM    (max 64 KBytes) - 8bit Bus width

#[derive(Debug)]
pub struct Cartridge {
    blob: Vec<u8>,
}

impl Cartridge {
    pub fn new() -> Self {
        Self { blob: vec![] }
    }

    pub fn new_with_contents(blob: Vec<u8>) -> Self {
        Self { blob }
    }

    // TODO: size checks, format checks on ROM
    pub fn read_file(mut self, path: impl AsRef<Path>) -> Self {
        self.blob = fs::read(path).unwrap();
        self
    }

    pub fn game_title(&self) -> String {
        let bytes = self.read_bytes(0xA0, 12);
        let nullbyte_index = bytes.iter().position(|&b| b == 0);
        // Read until possible nullbyte
        let bytes = if let Some(index) = nullbyte_index {
            &bytes[0..index]
        } else {
            bytes
        };
        if let Ok(title) = String::from_utf8(bytes.to_vec()) {
            title
        } else {
            "ERROR READING GAME TITLE".to_string()
        }
    }

    pub fn header(&self) -> &[u8] {
        &self.blob[0..CARTRIDGE_HEADER_SIZE]
    }

    pub fn first_instr(&self) -> u32 {
        let arr: [u8; 4] = self.blob[0..4].try_into().unwrap();
        u32::from_le_bytes(arr)
    }

    fn read_bytes(&self, start: usize, amount: usize) -> &[u8] {
        &self.blob[start..start + amount]
    }

    fn access_cycles<const ACCESS_SIZE: usize>(address: u32) -> Cycles {
        let size = ACCESS_SIZE;
        let address = address as usize;
        if address >= CART_SRAM_START {
            match size {
                BYTE | HALFWORD => Cycles(5),
                WORD => Cycles(8),
                _ => unreachable!(),
            }
        } else {
            Cycles(5) * (size as u32)
        }
    }
}

impl Bus for Cartridge {
    fn read<const SIZE: usize>(&self, address: u32) -> ([u8; SIZE], Cycles) {
        let size = SIZE;
        let cycles = Cartridge::access_cycles::<SIZE>(address);
        let address = address as usize;
        let range = address..(address + size);
        let val: [u8; SIZE] = self.blob[range].try_into().unwrap();
        (val, cycles)
    }

    fn write<const SIZE: usize>(&mut self, address: u32, val: &[u8; SIZE]) -> Cycles {
        let size = SIZE;
        let cycles = Cartridge::access_cycles::<SIZE>(address);
        let address = address as usize;
        let range = address..(address + size);
        self.blob[range].copy_from_slice(val);
        cycles
    }
}
