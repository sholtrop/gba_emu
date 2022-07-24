use crate::memcontroller::{shared, Shared, KB};

#[derive(Debug)]
pub struct EwRam {
    mem: Box<[u8; 256 * KB]>,
}

impl EwRam {
    pub fn new() -> Shared<Self> {
        shared(Self {
            mem: Box::new([0; 256 * KB]),
        })
    }
}

#[derive(Debug)]
pub struct IwRam {
    mem: Box<[u8; 32 * KB]>,
}

impl IwRam {
    pub fn new() -> Shared<Self> {
        shared(Self {
            mem: Box::new([0; 32 * KB]),
        })
    }
}

#[derive(Debug)]
pub struct IoRam {
    mem: Box<[u8; 1 * KB]>,
}

impl IoRam {
    pub fn new() -> Shared<Self> {
        shared(Self {
            mem: Box::new([0; 1 * KB]),
        })
    }
}

#[derive(Debug)]
pub struct PaletteRam {
    mem: Box<[u8; 1 * KB]>,
}

impl PaletteRam {
    pub fn new() -> Shared<Self> {
        shared(Self {
            mem: Box::new([0; 1 * KB]),
        })
    }
}
#[derive(Debug)]
pub struct Vram {
    mem: Box<[u8; 96 * KB]>,
}

impl Vram {
    pub fn new() -> Shared<Self> {
        shared(Self {
            mem: Box::new([0; 96 * KB]),
        })
    }
}
#[derive(Debug)]
pub struct Oam {
    mem: Box<[u8; 1 * KB]>,
}

impl Oam {
    pub fn new() -> Shared<Self> {
        shared(Self {
            mem: Box::new([0; 1 * KB]),
        })
    }
}
