use std::fs;
use std::path::Path;

const CARTRIDGE_HEADER_SIZE: usize = 192;

#[derive(Debug)]
pub struct Cartridge {
    blob: Vec<u8>,
}

impl Cartridge {
    pub fn new() -> Self {
        Self { blob: vec![] }
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

    pub fn mem(&self) -> &[u8] {
        &self.blob
    }

    pub fn mem_mut(&mut self) -> &mut [u8] {
        &mut self.blob
    }

    fn read_bytes(&self, start: usize, amount: usize) -> &[u8] {
        &self.blob[start..start + amount]
    }
}
