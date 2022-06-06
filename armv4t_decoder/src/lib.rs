pub mod arm;
pub mod common;
pub mod register;
pub mod thumb;

pub fn decode_thumb(instr: u16) -> thumb::ThumbInstruction {
    thumb::ThumbInstruction::decode(instr)
}

pub fn decode_arm(instr: u32) -> arm::ArmInstruction {
    arm::ArmInstruction::decode(instr)
}
