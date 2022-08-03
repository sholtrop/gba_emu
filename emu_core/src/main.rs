mod alu;
mod bios;
mod bus;
mod cartridge;
mod cpu;
mod display;
mod emulator;
mod instruction;
mod memcontroller;
mod mmio;
mod ram;
mod registers;
mod renderer;

use crate::{display::DummyDisplay, emulator::Emulator};
use cartridge::Cartridge;

const CARTRIDGE_FILENAME: &str = "gba-tests/arm/arm.gba";

fn setup_logging() {
    let env = env_logger::Env::default().filter_or(env_logger::DEFAULT_FILTER_ENV, "trace");
    env_logger::builder()
        .format_timestamp(None)
        .parse_env(env)
        .init();
}

fn main() -> ! {
    setup_logging();
    let cartridge_filename = CARTRIDGE_FILENAME;
    let cartridge = Cartridge::new().read_file(cartridge_filename);
    log::debug!("{}", cartridge.game_title());
    let mut emu = Emulator::new(DummyDisplay {});
    emu.insert_cartridge(cartridge);
    emu.run();
}
