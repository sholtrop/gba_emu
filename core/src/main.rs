mod cartridge;
mod cpu;
mod hardware_regs;
mod instruction_decoder;
mod memory;
mod registers;

use cartridge::Cartridge;

fn main() {
    let cart = Cartridge::open("pokemon_emerald.gba");
    println!("{}", cart.game_title());
}
