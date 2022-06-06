use armv4t_decoder::{arm::ArmInstruction, thumb::ThumbInstruction};

#[derive(Debug)]
pub enum Armv4tInstruction {
    Arm(ArmInstruction),
    Thumb(ThumbInstruction),
}

const NOP: u32 = 0b11010101000000110010000000011111;

impl Armv4tInstruction {
    pub fn nop() -> Self {
        Self::Arm(ArmInstruction::decode(NOP))
    }
}
