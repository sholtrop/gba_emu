mod cpu;
mod hardware_regs;
mod instruction_decoder;
mod memory;
mod registers;

fn main() {
    let mem = memory::EmulatorMemory::new();
    println!("{}", mem.read(0));
}
