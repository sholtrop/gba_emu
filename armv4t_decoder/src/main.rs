use crate::thumb::ThumbInstruction;

use crate::arm::ArmInstruction;

mod arm;
mod common;
mod register;
mod thumb;

fn main() {
    let inst = std::env::args().nth(1).expect("No instruction given");
    println!("{}", inst);
    let inst = u16::from_str_radix(inst.trim_end().trim_start_matches("0x"), 16).unwrap();
    let inst = ThumbInstruction::decode(inst);
    println!("{:?}", inst);
    println!("{}", inst);
}
