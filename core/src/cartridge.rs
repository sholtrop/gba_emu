use std::fs;
use std::path::Path;

const CARTRIDGE_HEADER_START: usize = 0x0100;
const CARTRIDGE_HEADER_END: usize = 0x014F;
const CARTRIDGE_CODE_START: usize = 0x08000000;

pub struct Cartridge {
    blob: Vec<u8>,
}

impl Cartridge {
    // TODO: size checks on ROM
    pub fn open(path: impl AsRef<Path>) -> Self {
        Self {
            blob: fs::read(path).unwrap(),
        }
    }

    pub fn game_title(&self) -> String {
        let bytes = self.read_bytes(0x0A0, 12);
        let index = bytes.iter().position(|&b| b == 0);
        // Read until possible nullbyte
        let bytes = if let Some(index) = index {
            &bytes[0..index]
        } else {
            bytes
        };
        String::from_utf8(bytes.to_vec()).unwrap()
    }

    pub fn header(&self) -> &[u8] {
        &self.blob[CARTRIDGE_HEADER_START..CARTRIDGE_HEADER_END]
    }

    pub fn game_instructions(&self) -> &[u8] {
        self.read_bytes(CARTRIDGE_CODE_START, 1)
    }

    fn read_bytes(&self, start: usize, amount: usize) -> &[u8] {
        &self.blob[start..start + amount]
    }
}
