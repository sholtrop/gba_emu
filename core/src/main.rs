mod alu;
mod bios;
mod cartridge;
mod cpu;
mod hardware_regs;
mod instruction;
mod memory;
mod registers;

use std::{cell::RefCell, rc::Rc};

use armv4t_decoder::decode_arm;
use cartridge::Cartridge;

use crate::{cpu::Cpu, memory::GbaMemory};

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let cartridge_filename = &args[1];
    let cartridge = Cartridge::new().read_file(cartridge_filename);
    println!("{}", cartridge.game_title());
    println!("{}", decode_arm(cartridge.first_instr()));

    let memory = Rc::new(RefCell::new(GbaMemory::new()));

    let mut cpu = Cpu::new(memory);

    loop {
        cpu.tick();
    }
}
