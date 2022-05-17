use crate::decode_arm::Instruction;

mod decode_arm;
mod decode_thumb;
mod register;

fn main() {
    println!("{:?}", Instruction::decode(0xFFFF));
}
