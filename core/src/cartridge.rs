use std::fs;
use std::path::Path;

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

    fn read_bytes(&self, start: usize, amount: usize) -> &[u8] {
        &self.blob[start..start + amount]
    }
}
