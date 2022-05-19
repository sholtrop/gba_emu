use crate::arm::Instruction;

mod arm;
mod register;
mod thumb;

fn main() {
    println!("{:?}", Instruction::decode(0xFFFF));
}
