use std::{cell::RefCell, rc::Rc};

use crate::{
    cartridge::Cartridge,
    cpu::{Cpu, Cycles},
    memcontroller::{MemoryController, CART_ROM_START},
};

const CYCLES_PER_FRAME: usize = Cpu::CYCLES_PER_SECOND / Cpu::FRAMES_PER_SECOND;
const HDRAW_CYCLES: usize = 960;
const HBLANK_CYCLES: usize = 272;
const H_PIXELS: usize = 240;
const V_PIXELS: usize = 160;

pub struct Emulator {
    cpu: Cpu,
    memory: Rc<RefCell<MemoryController>>,
    hdraw_counter: Cycles,
}

impl Emulator {
    pub fn new() -> Self {
        let memory = Rc::new(RefCell::new(MemoryController::new()));
        Emulator {
            cpu: Cpu::new(memory.clone(), CART_ROM_START as u32),
            memory,
            hdraw_counter: Cycles(0),
        }
    }

    pub fn insert_cartridge(&mut self, cartridge: Cartridge) {
        self.memory.borrow_mut().insert_cartridge(cartridge);
    }

    pub fn run(&mut self) -> ! {
        loop {
            self.hdraw_counter += self.cpu.tick();
        }
    }
}
