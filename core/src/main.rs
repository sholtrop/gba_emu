mod alu;
mod cartridge;
mod cpu;
mod hardware_regs;
mod instruction;
mod memory;
mod registers;

use armv4t_decoder::decode_arm;
use cartridge::Cartridge;

fn main() {
    let cartridge = Cartridge::new().read_file("pokemon_emerald.gba");
    println!("{}", cartridge.game_title());
    println!("{}", decode_arm(cartridge.first_instr()));
}
