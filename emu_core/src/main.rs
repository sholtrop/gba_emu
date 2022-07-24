mod alu;
mod bios;
mod bus;
mod cartridge;
mod cpu;
mod emulator;
mod instruction;
mod memcontroller;
mod mmio;
mod ram;
mod registers;

use cartridge::Cartridge;
use cpu::Cpu;
use memcontroller::{MemoryController, CART_ROM_START};
use std::cell::RefCell;
use std::rc::Rc;

const CARTRIDGE_FILENAME: &str = "gba-tests/arm/arm.gba";

fn setup_logging() {
    let env = env_logger::Env::default().filter_or(env_logger::DEFAULT_FILTER_ENV, "trace");
    env_logger::builder()
        .format_timestamp(None)
        .parse_env(env)
        .init();
}

fn main() {
    setup_logging();
    let cartridge_filename = CARTRIDGE_FILENAME;
    let cartridge = Cartridge::new().read_file(cartridge_filename);

    log::debug!("{}", cartridge.game_title());
    let memory = Rc::new(RefCell::new(MemoryController::new()));
    memory.borrow_mut().insert_cartridge(cartridge);
    let mut cpu = Cpu::new(memory, CART_ROM_START as u32);

    loop {
        cpu.tick();
    }
}
