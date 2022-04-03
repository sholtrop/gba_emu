use crate::registers::CpuRegisters;

pub const CYCLES_PER_SECOND: usize = 1 << 24;

mod pipeline {
    #[derive(Debug)]
    pub enum Stage {
        Fetch,
        Decode,
        Execute,
    }
}

#[derive(Debug)]
enum InstructionSetState {
    // 16-bit special subset of instructions that map to 32-bit counterparts
    Thumb,

    // 32-bit ARM instruction set
    Arm,
}

#[derive(Debug)]
pub struct Cpu {
    current_instruction_set: InstructionSetState,
    current_stage: pipeline::Stage,
    registers: CpuRegisters,
}

impl Default for Cpu {
    fn default() -> Self {
        Self {
            current_instruction_set: InstructionSetState::Arm,
            current_stage: pipeline::Stage::Fetch,
            registers: CpuRegisters::new(),
        }
    }
}

impl Cpu {}
