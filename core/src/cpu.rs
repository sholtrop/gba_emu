use crate::{
    alu::Alu,
    instruction::Armv4tInstruction,
    memory::{CycleCost, EmulatorMemory, MemRead, MemSize, MemWrite},
    registers::{psr::InstructionMode, CpuRegisters},
};
use armv4t_decoder::{arm::*, common::*, thumb::ThumbInstruction};
use bitvec::{order::Lsb0, view::BitView};
use std::{cell::RefCell, rc::Rc};
use MemSize::*;

pub const CYCLES_PER_SECOND: usize = 1 << 24;
pub const FRAMES_PER_SECOND: usize = 60;
pub const CYCLES_PER_FRAME: usize = CYCLES_PER_SECOND / FRAMES_PER_SECOND;

pub const PROGRAM_COUNTER: RegisterName = RegisterName::R15;

#[derive(Debug)]
pub struct Cpu {
    registers: CpuRegisters,
    memory: Rc<RefCell<EmulatorMemory>>,
    alu: Alu,
    cycle_count: u32,
}

impl Cpu {
    pub fn new(memory: Rc<RefCell<EmulatorMemory>>) -> Self {
        Self {
            registers: CpuRegisters::new(),
            memory,
            alu: Alu {},
            cycle_count: 0,
        }
    }

    pub fn tick(&mut self) {
        let instr = self.fetch();
        let op = self.decode(instr);
        self.execute(op);
    }

    fn fetch(&mut self) -> u32 {
        let pc = self.registers.read(PROGRAM_COUNTER);
        let pc_offset;
        let (instr, cycles) = match self.registers.get_instr_mode() {
            InstructionMode::Thumb => {
                pc_offset = 2;
                self.memory.borrow().read(&MemRead {
                    address: pc,
                    size: Halfword,
                })
            }
            InstructionMode::Arm => {
                pc_offset = 4;
                self.memory.borrow().read(&MemRead {
                    address: pc,
                    size: Word,
                })
            }
        };
        self.pc_add_offset(pc_offset);
        self.cycle_count += cycles.0 as u32;
        instr
    }

    fn decode(&self, instr: u32) -> Armv4tInstruction {
        use Armv4tInstruction::*;
        match self.registers.get_instr_mode() {
            InstructionMode::Thumb => Thumb(armv4t_decoder::decode_thumb((instr & 0xFFFF) as u16)),
            InstructionMode::Arm => Arm(armv4t_decoder::decode_arm(instr)),
        }
    }

    fn execute(&mut self, op: Armv4tInstruction) {
        let cycles = match op {
            Armv4tInstruction::Arm(op) => self.exec_arm(op),
            Armv4tInstruction::Thumb(op) => self.exec_thumb(op),
        };
        self.cycle_count += cycles.0 as u32;
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
        let cond = instr.condition();
        if !self.arm_cond_satisfied(cond) {
            return CycleCost(1);
        }
        match instr {
            BranchAndExchange(op) => self.branch_and_exchange(op),
            BlockDataTransfer(op) => self.block_data_transfer(op),
            BranchAndBranchWithLink(op) => CycleCost(1),
            SoftwareInterrupt(op) => CycleCost(1),
            Undefined(op) => CycleCost(1),
            SingleDataTransfer(op) => CycleCost(1),
            SingleDataSwap(op) => CycleCost(1),
            HalfwordDataTransfer(op) => CycleCost(1),
            Multiply(op) => CycleCost(1),
            MultiplyLong(op) => CycleCost(1),
            PsrTransferMrs(op) => CycleCost(1),
            PsrTransferMsr(op) => CycleCost(1),
            PsrTransferMsrImm(op) => CycleCost(1),
            DataProcessing(op) => CycleCost(1),
        }
    }

    fn arm_cond_satisfied(&self, cond: Condition) -> bool {
        use Condition::*;
        let cpsr = self.registers.read_cpsr();
        match cond {
            Eq => cpsr.zero_condition(),
            Ne => !cpsr.zero_condition(),
            Cs => cpsr.cbe_condition(),
            Cc => !cpsr.cbe_condition(),
            Mi => cpsr.neg_condition(),
            Pl => !cpsr.neg_condition(),
            Vs => cpsr.overflow_condition(),
            Vc => !cpsr.overflow_condition(),
            Hi => cpsr.cbe_condition() && !cpsr.zero_condition(),
            Ls => !cpsr.cbe_condition() || cpsr.zero_condition(),
            Ge => cpsr.neg_condition() == cpsr.overflow_condition(),
            Lt => cpsr.neg_condition() != cpsr.overflow_condition(),
            Gt => !cpsr.zero_condition() && (cpsr.neg_condition() == cpsr.overflow_condition()),
            Le => cpsr.zero_condition() || (cpsr.neg_condition() != cpsr.overflow_condition()),
            Al => true,
        }
    }

    fn exec_thumb(&mut self, instr: ThumbInstruction) -> CycleCost {
        use armv4t_decoder::thumb::ThumbInstruction::*;
        match instr {
            AddOffsetToStackPointer(op) => {}
            SoftwareInterrupt(op) => {}
            UnconditionalBranch(op) => {}
            MultipleLoadstore(op) => {}
            LongBranchWithLink(op) => {}
            PushPopRegister(op) => {}
            LoadStoreHalfWord(op) => {}
            MoveShiftedRegister(op) => {}
            SpRelativeLoadStore(op) => {}
            LoadAddress(op) => {}
            ConditionalBranch(op) => {}
            LoadStoreImmOffset(op) => {}
            LoadStoreRegOffset(op) => {}
            LoadStoreSignExtHalfwordByte(op) => {}
            PcRelativeLoad(op) => {}
            HiRegOpsBranchExchange(op) => {}
            AluOps(op) => {}
            MoveCompAddSubtractImm(op) => {}
            AddSub(op) => {}
        }
        CycleCost(1)
    }

    fn branch_and_exchange(&mut self, op: branch_and_exchange::Op) -> CycleCost {
        let val = self.registers.read(op.rn());
        let switch_to_thumb = val & 1 == 1;
        self.registers.write(PROGRAM_COUNTER, val);
        if switch_to_thumb {
            self.registers.switch_instr_mode(InstructionMode::Thumb)
        }
        // TODO: Make cycle cost more accurate by looking at what memory region we would refill the pipeline from,
        // had this been a real GBA. Should take 2S + 1N cycles.
        CycleCost(3)
    }

    fn block_data_transfer(&mut self, op: block_data_transfer::Op) -> CycleCost {
        use LoadOrStore::*;
        use PreOrPostIndexing::*;
        use UpOrDown::*;

        let offset: i32 = match op.up_or_down() {
            Up => 4,
            Down => -4,
        };
        let base_reg = op.base_reg();
        let base_addr = self.registers.read(base_reg) as i32;
        let address = base_addr
            + match op.pre_post_indexing() {
                Pre => offset,
                Post => 0,
            };
        let mut address = address as u32;

        let mut mem = self.memory.borrow_mut();
        let mut cycle_cost = CycleCost(0);
        for reg in op.reg_list().to_vec() {
            cycle_cost += match op.load_store() {
                Store => mem.write(&MemWrite {
                    address,
                    size: MemSize::Word,
                    value: self.registers.read(reg),
                }),
                Load => {
                    let (val, cycles) = mem.read(&MemRead {
                        address,
                        size: MemSize::Word,
                    });
                    self.registers.write(reg, val);
                    cycles
                }
            };
            if offset < 0 {
                address -= offset.abs() as u32;
            } else {
                address += offset as u32;
            }
        }
        if op.psr_force_user() {
            todo!("S-bit should do something special")
        }
        if op.write_back() {
            let wb_addr = match op.pre_post_indexing() {
                Post => address,
                Pre => ((address as i32) - offset) as u32,
            };
            self.registers.write(base_reg, wb_addr);
        }
        // TODO: Cycle cost could be more accurate
        cycle_cost
    }
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
