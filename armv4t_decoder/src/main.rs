use arm7tdmi_decoder::arm::{psr_transfer_msr, Operand12Bit};

use crate::arm::Instruction;

mod arm;
mod common;
mod register;
mod thumb;

fn main() {
    let inst = std::env::args().nth(1).unwrap();
    println!("{}", inst);
    let inst = u32::from_str_radix(inst.trim_end().trim_start_matches("0x"), 16).unwrap();
    let inst = Instruction::decode(inst);
    // println!("{:?}", );
    if let Instruction::PsrTransferMsrImm(op) = inst {
        println!("{}", op.src_operand().as_rot_imm().value());
    }
}
