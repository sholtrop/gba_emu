mod alu;
mod bios;
mod cartridge;
mod cpu;
mod hardware_regs;
mod instruction;
mod memory;
mod registers;

use cartridge::Cartridge;
use cpu::Cpu;
use memory::GbaMemory;

use crate::memory::CART_ROM_START;

const CARTRIDGE_FILENAME: &str = "gba-tests/arm/arm.gba";

fn main() {
    let cartridge_filename = CARTRIDGE_FILENAME;
    let cartridge = Cartridge::new().read_file(cartridge_filename);

    println!("{}", cartridge.game_title());
    let memory = GbaMemory::new();
    memory.borrow_mut().insert_cartridge(cartridge);
    let mut cpu = Cpu::new(memory, CART_ROM_START as u32);

    loop {
        cpu.tick();
    }
}
