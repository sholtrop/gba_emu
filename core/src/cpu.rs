use armv4t_decoder::{
    arm::ArmInstruction, common::RegisterName, decode_arm, decode_thumb, thumb::ThumbInstruction,
};

use crate::{
    alu::Alu,
    instruction::Armv4tInstruction,
    memory::{EmulatorMemory, MemRead, MemReadSize},
    registers::CpuRegisters,
};
use std::{cell::RefCell, rc::Rc};

pub const CYCLES_PER_SECOND: usize = 1 << 24;
pub const FRAMES_PER_SECOND: usize = 60;
pub const CYCLES_PER_FRAME: usize = CYCLES_PER_SECOND / FRAMES_PER_SECOND;

pub const PROGRAM_COUNTER: RegisterName = RegisterName::R15;

#[derive(Clone, Copy, Debug)]
struct CycleCost(u8);

#[derive(Debug)]
pub struct Cpu {
    instr_mode: InstructionMode,
    registers: CpuRegisters,
    memory: Rc<RefCell<EmulatorMemory>>,
    alu: Alu,
    cycle_count: u32,
}

impl Cpu {
    pub fn new(memory: Rc<RefCell<EmulatorMemory>>) -> Self {
        Self {
            instr_mode: InstructionMode::Arm,
            registers: CpuRegisters::new(),
            memory,
            alu: Alu {},
            cycle_count: 0,
        }
    }

    pub fn tick(&mut self) {
        let instr = self.fetch();
        let cycles = match instr {
            Armv4tInstruction::Arm(op) => self.exec_arm(op),
            Armv4tInstruction::Thumb(op) => self.exec_thumb(op),
        };
        self.cycle_count += cycles.0 as u32;
    }

    fn fetch(&mut self) -> Armv4tInstruction {
        let pc = self.registers.read(PROGRAM_COUNTER);
        let pc_offset;
        let instr = match self.instr_mode {
            InstructionMode::Thumb => match self.memory.borrow().read(pc, MemReadSize::Halfword) {
                MemRead::Halfword(instr) => {
                    pc_offset = 2;
                    Armv4tInstruction::Thumb(decode_thumb(instr))
                }
                _ => unreachable!(),
            },
            InstructionMode::Arm => match self.memory.borrow().read(pc, MemReadSize::Word) {
                MemRead::Word(instr) => {
                    pc_offset = 4;
                    Armv4tInstruction::Arm(decode_arm(instr))
                }
                _ => unreachable!(),
            },
        };
        self.pc_add_offset(pc_offset);
        instr
    }

    fn pc_add_offset(&mut self, offset: i32) {
        let pc = self.registers.read(PROGRAM_COUNTER);
        if offset < 0 {
            self.registers
                .write(PROGRAM_COUNTER, pc - offset.abs() as u32);
        } else {
            self.registers.write(PROGRAM_COUNTER, pc + offset as u32);
        }
    }

    fn exec_arm(&mut self, instr: ArmInstruction) -> CycleCost {
        use armv4t_decoder::arm::ArmInstruction::*;
        match instr {
            BranchAndBranchWithLink(op) => {}
            _ => todo!("Implement {:#?}", instr),
        }
        CycleCost(1)
    }

    fn exec_thumb(&mut self, instr: ThumbInstruction) -> CycleCost {
        use armv4t_decoder::thumb::ThumbInstruction::*;
        match instr {
            AddOffsetToStackPointer(op) => {}
            _ => todo!("Implement {:#?}", instr),
        }
        CycleCost(1)
    }
}

#[derive(Debug)]
enum InstructionMode {
    /// 16-bit special subset of instructions that map to 32-bit counterparts
    Thumb,

    /// 32-bit ARM instruction set
    Arm,
}

#[derive(Debug)]
enum CpuMode {
    /// Default mode
    User = 0b10000,

    /// Privileged mode for operating system
    System = 0b11111,

    /// Interrupt request mode
    Irq = 0b10010,

    /// Fast interrupt request mode
    Fiq = 0b10001,

    /// Supervisor mode. When calling the BIOS via SWI instructions.
    Supervisor = 0b10011,

    /// Abort mode. Entered after data or instruction prefetch abort.
    Abort = 0b10111,

    /// Undefined. Entered when an undefined instruction is executed.
    Undefined = 0b11011,
}
