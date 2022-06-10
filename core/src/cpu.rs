use crate::registers::psr::ProcessorMode::{self, *};
use crate::{
    alu::Alu,
    instruction::Armv4tInstruction,
    memory::{CycleCost, GbaMemory, BYTE, HALFWORD, WORD},
    registers::CpuRegisters,
};

use armv4t_decoder::{arm::*, common::*, thumb::ThumbInstruction};
use std::{cell::RefCell, rc::Rc};

pub const CYCLES_PER_SECOND: usize = 1 << 24;
pub const FRAMES_PER_SECOND: usize = 60;
pub const CYCLES_PER_FRAME: usize = CYCLES_PER_SECOND / FRAMES_PER_SECOND;

pub const PROGRAM_COUNTER: RegisterName = RegisterName::R15;
/// Value the PC is set to when entering supervisor mode via a SWI
pub const SUPERVISOR_PC: u32 = 0x08;

#[derive(Debug)]
pub struct Cpu {
    registers: CpuRegisters,
    memory: Rc<RefCell<GbaMemory>>,
    cycle_count: u32,
}

impl Cpu {
    pub fn new(memory: Rc<RefCell<GbaMemory>>) -> Self {
        Self {
            registers: CpuRegisters::new(),
            memory,
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

                let (val, cycles) = self.memory.borrow().read_le_halfword(pc);
                (val as u32, cycles)
            }
            InstructionMode::Arm => {
                pc_offset = 4;
                self.memory.borrow().read_le_word(pc)
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

    /// Add `offset` to the program counter. Returns the value of the old program counter, before the offset has been added.
    fn pc_add_offset(&mut self, offset: i32) -> u32 {
        let pc = self.registers.read(PROGRAM_COUNTER);
        if offset < 0 {
            self.registers
                .write(PROGRAM_COUNTER, pc - offset.abs() as u32);
        } else {
            self.registers.write(PROGRAM_COUNTER, pc + offset as u32);
        }
        pc
    }

    fn exec_arm(&mut self, instr: ArmInstruction) -> CycleCost {
        use armv4t_decoder::arm::ArmInstruction::*;
        let cond = instr.condition();
        if !self.arm_cond_satisfied(cond) {
            return CycleCost(1);
        }
        match instr {
            BranchAndExchange(op) => self.arm_branch_and_exchange(op),
            BlockDataTransfer(op) => self.arm_block_data_transfer(op),
            BranchAndBranchWithLink(op) => self.arm_branch_with_link(op),
            SoftwareInterrupt(op) => self.arm_software_interrupt(op),
            Undefined(op) => self.arm_undefined_op(op),
            SingleDataTransfer(op) => self.arm_single_data_transfer(op),
            SingleDataSwap(op) => self.arm_single_data_swap(op),
            HalfwordDataTransfer(op) => CycleCost(1),
            Multiply(op) => CycleCost(1),
            MultiplyLong(op) => CycleCost(1),
            PsrTransferMrs(op) => CycleCost(1),
            PsrTransferMsr(op) => CycleCost(1),
            PsrTransferMsrImm(op) => CycleCost(1),
            DataProcessing(op) => self.arm_data_processing(op),
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

    fn arm_branch_and_exchange(&mut self, op: branch_and_exchange::Op) -> CycleCost {
        let val = self.registers.read(op.rn());
        let switch_to_thumb = val & 1 == 1;
        self.registers.write(PROGRAM_COUNTER, val);
        if switch_to_thumb {
            self.registers.switch_instr_mode(InstructionMode::Thumb)
        }
        // TODO: Make cycle cost more accurate by looking at what memory region we would refill the pipeline from,
        // had this been a real GBA. Should take 2S + 1N cycles.
        CycleCost(12)
    }

    fn arm_block_data_transfer(&mut self, op: block_data_transfer::Op) -> CycleCost {
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
                Store => mem.write_le_word(address, self.registers.read(reg)),
                Load => {
                    let (val, cycles) = mem.read_le_word(address);
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
        // TODO: Make nS + 1N + 1I for LDM
        // (n+1)S + 2N + 1I for LDM PC
        // (n-1)S + 2N for STM
        // where n = amount of words transferred
        cycle_cost
    }

    fn arm_branch_with_link(&mut self, op: branch_and_link::Op) -> CycleCost {
        use RegisterName::*;
        let offset = op.offset_get();
        let old_pc = self.pc_add_offset(offset);
        if op.link() {
            // Link-reg contains address following the BL instruction
            self.registers.write(R14, old_pc + 4);
        }
        // TODO: make 2S + 1N
        CycleCost(12)
    }

    fn arm_software_interrupt(&mut self, op: software_interrupt::Op) -> CycleCost {
        let old_cpsr = self.registers.read_cpsr();
        self.registers.switch_cpu_mode(Supervisor);
        self.registers.write(PROGRAM_COUNTER, SUPERVISOR_PC);
        self.registers.write_spsr(old_cpsr);
        let bios_op = op.comment();
        todo!("Call relevant BIOS function");
        // TODO: Make 2S + 1N
        CycleCost(12)
    }

    fn arm_undefined_op(&mut self, op: undefined_instr::Op) -> CycleCost {
        self.registers.switch_cpu_mode(ProcessorMode::Undefined);
        // TODO: Set PC to und exception vector address
        // TODO: Make 2S + 1I + 1N
        CycleCost(12)
    }

    fn arm_single_data_transfer(&mut self, op: single_data_transfer::Op) -> CycleCost {
        use ByteOrWord::*;
        use LoadOrStore::*;
        use PreOrPostIndexing::*;
        use UpOrDown::*;

        let mut total_cycle_cost = CycleCost(1);

        let offset = if op.is_reg_offset() {
            let offset_reg = op.offset().as_shift_reg();
            let offset_amount = self.registers.read(offset_reg.reg());
            let shift_amount = if offset_reg.shift_amt_in_reg() {
                let sh_amt_reg = offset_reg.shift_reg();
                self.registers.read(sh_amt_reg) as u8
            } else {
                offset_reg.shift_amt()
            };
            let sh_type = offset_reg.shift_type();
            Alu::shift(offset_amount, sh_type, shift_amount)
        } else {
            op.offset().as_imm() as u32
        };
        let base_addr = self.registers.read(op.base_reg());
        let offset_addr = match op.up_or_down() {
            Up => base_addr + offset,
            Down => base_addr - offset,
        };
        let address = match op.pre_post_indexing() {
            Pre => offset_addr,
            Post => base_addr,
        };

        // TODO: half-word aligned addresses need to be rotated into the reg
        let cycle_cost = match op.load_store() {
            Load => {
                let dst = op.reg_dest();
                let mem = self.memory.borrow();
                let (value, cycle_cost) = match op.byte_or_word() {
                    Byte => {
                        let (val, cycles) = mem.read_byte(address);
                        (val as u32, cycles)
                    }
                    Word => mem.read_le_word(address),
                };
                self.registers.write(dst, value);
                cycle_cost
            }
            Store => {
                let value = self.registers.read(op.reg_dest());
                let mut mem = self.memory.borrow_mut();
                match op.byte_or_word() {
                    Byte => mem.write_byte(address, value as u8),
                    Word => mem.write_le_word(address, value),
                }
            }
        };
        total_cycle_cost += cycle_cost;

        if op.write_back() {
            self.registers.write(op.base_reg(), offset_addr);
        }
        // TODO: Make 1S + 1N + 1I for LDR, and 2N for STR
        total_cycle_cost
    }

    fn arm_single_data_swap(&mut self, op: single_data_swap::Op) -> CycleCost {
        let address = self.registers.read(op.base_reg());
        let new_val = self.registers.read(op.src_reg());

        match op.byte_or_word() {
            ByteOrWord::Byte => {
                let new_val = new_val as u8;
                let (old_val, cycles_read) = self.memory.borrow().read_byte(address);
                let cycles_write = self.memory.borrow_mut().write_byte(address, new_val);
                self.registers.write(op.dest_reg(), old_val as u32);
                cycles_read + cycles_write
            }
            ByteOrWord::Word => {
                let (old_val, cycles_read) = self.memory.borrow().read_le_word(address);
                let cycles_write = self.memory.borrow_mut().write_le_word(address, new_val);
                self.registers.write(op.dest_reg(), old_val);
                cycles_read + cycles_write
            }
        }
        // TODO: Cycles should be 1S + 2N + 1I
    }

    fn arm_data_processing(&mut self, op: data_processing::Op) -> CycleCost {
        // TODO for register specified shift amounts: If this byte is zero, the unchanged contents of Rm will be used as the second operand,
        // and the old value of the CPSR C flag will be passed on as the shifter carry output.

        // When Rd is R15 and the S flag is set the result of the operation is placed in R15 and
        // the SPSR corresponding to the current mode is moved to the CPSR. This allows state
        // changes which atomically restore both PC and CPSR. This form of instruction should
        // not be used in User mode

        CycleCost(0)
    }
}
