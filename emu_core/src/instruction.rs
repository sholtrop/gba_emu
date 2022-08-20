use armv4t_decoder::{arm::ArmInstruction, thumb::ThumbInstruction};

#[derive(Debug, Clone, Copy)]
pub enum Armv4tInstruction {
    Arm(ArmInstruction),
    Thumb(ThumbInstruction),
}

/// = mov r0, r0
pub const NOP_ARM: u32 = 0xe1a00000;

/// = mov r8, r8
pub const NOP_THUMB: u16 = 0x46c0;

impl Armv4tInstruction {
    pub fn nop_arm() -> Self {
        Self::Arm(ArmInstruction::decode(NOP_ARM))
    }

    pub fn nop_thumb() -> Self {
        Self::Thumb(ThumbInstruction::decode(NOP_ARM as u16))
    }
}
