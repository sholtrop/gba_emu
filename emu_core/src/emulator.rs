use std::{cell::RefCell, rc::Rc};

use crate::{
    cartridge::Cartridge,
    cpu::Cpu,
    memory::{Cycles, GbaMemory, CART_ROM_START},
};

const CYCLES_PER_FRAME: usize = Cpu::CYCLES_PER_SECOND / Cpu::FRAMES_PER_SECOND;
const HDRAW_CYCLES: usize = 960;
const HDRAW_BLANK: usize = 272;

pub struct Emulator {
    cpu: Cpu,
    memory: Rc<RefCell<GbaMemory>>,
    cycles_til_hblank: Cycles,
}

impl Emulator {
    pub fn new() -> Self {
        let memory = Rc::new(RefCell::new(GbaMemory::new()));
        Emulator {
            cpu: Cpu::new(memory.clone(), CART_ROM_START as u32),
            memory,
            cycles_til_hblank: Cycles(0),
        }
    }

    pub fn insert_cartridge(&mut self, cartridge: Cartridge) {
        self.memory.borrow_mut().insert_cartridge(cartridge);
    }

    pub fn run(&mut self) -> ! {
        loop {
            self.cycles_til_hblank -= self.cpu.tick();
        }
    }
}
