use crate::alu::AluResult;
use crate::bus::{Bus, WORD};
use crate::registers::psr::ProcessorMode::{self, *};
use crate::{
    alu::{Alu, AluSetCpsr},
    instruction::Armv4tInstruction,
    memcontroller::MemoryController,
    registers::CpuRegisters,
};
use armv4t_decoder::arm::data_processing::OpCode;
use armv4t_decoder::arm::halfword_data_transfer::ShType;
use armv4t_decoder::thumb;
use armv4t_decoder::{arm::*, common::*, thumb::ThumbInstruction};
use std::ops::{Add, AddAssign, Mul, Sub, SubAssign};
use std::{cell::RefCell, rc::Rc};

pub const STACK_POINTER: RegisterName = RegisterName::R13;
pub const LINK_REGISTER: RegisterName = RegisterName::R14;
pub const PROGRAM_COUNTER: RegisterName = RegisterName::R15;
/// Value the PC is set to when entering supervisor mode via a SWI
pub const SUPERVISOR_PC: u32 = 0x08;

pub enum HardwareInterrupt {
    Vblank = 1,
    Hblank = 1 << 1,
    VcounterMatch = 1 << 2,
    Timer0Overflow = 1 << 3,
    Timer1Overflow = 1 << 4,
    Timer2Overflow = 1 << 5,
    Timer3Overflow = 1 << 6,
    SerialComm = 1 << 7,
    Dma0 = 1 << 8,
    Dma1 = 1 << 9,
    Dma2 = 1 << 10,
    Dma3 = 1 << 11,
    Keypad = 1 << 12,
    GamePak = 1 << 13,
}

#[derive(Clone, Copy, Debug)]
pub struct Cycles(pub u8);

impl Add for Cycles {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Self(self.0 + rhs.0)
    }
}

impl AddAssign for Cycles {
    fn add_assign(&mut self, rhs: Self) {
        self.0 += rhs.0;
    }
}

impl Sub for Cycles {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        Self(self.0 - rhs.0)
    }
}

impl SubAssign for Cycles {
    fn sub_assign(&mut self, rhs: Self) {
        self.0 -= rhs.0
    }
}

impl Mul<u8> for Cycles {
    type Output = Self;

    fn mul(self, rhs: u8) -> Self::Output {
        Self(self.0 * rhs)
    }
}

pub struct Cpu {
    registers: CpuRegisters,
    memory: Rc<RefCell<MemoryController>>,
    current_tick_cycles: Cycles,
}

impl Cpu {
    pub const CYCLES_PER_SECOND: usize = 1 << 24;
    pub const FRAMES_PER_SECOND: usize = 60;

    pub fn new(memory: Rc<RefCell<MemoryController>>, pc: u32) -> Self {
        Self {
            registers: CpuRegisters::new(pc),
            memory,
            current_tick_cycles: Cycles(0),
        }
    }

    pub fn tick(&mut self) -> Cycles {
        self.current_tick_cycles = Cycles(0);
        let instr = self.fetch();
        let op = self.decode(instr);
        self.execute(op);
        self.current_tick_cycles
    }

    pub fn handle_interrupt(&mut self, interrupt: HardwareInterrupt) -> Cycles {
        // TODO: Check REG_IME (if != 1, interrupts are ignored)
        // TODO: Check REG_IE for specific bit of this interrupt
        // TODO: Set REG_IF for currently handled interrupt

        // TODO: Switch to IRQ mode and execute the following BIOS code:
        /*
            00000018  b      128h                ;IRQ vector: jump to actual BIOS handler
            00000128  stmfd  r13!,r0-r3,r12,r14  ;save registers to SP_irq
            0000012C  mov    r0,4000000h         ;ptr+4 to 03FFFFFC (mirror of 03007FFC)
            00000130  add    r14,r15,0h          ;retadr for USER handler $+8=138h
            00000134  ldr    r15,[r0,-4h]        ;jump to [03FFFFFC] USER handler
            00000138  ldmfd  r13!,r0-r3,r12,r14  ;restore registers from SP_irq
            0000013C  subs   r15,r14,4h          ;return from IRQ (PC=LR-4, CPSR=SPSR)
        */

        // TODO: More accurate cycle cost
        Cycles(0)
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
        self.current_tick_cycles += cycles;
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
        self.current_tick_cycles += cycles;
    }

    /// Add `offset` to the program counter. Returns the value of the old program counter, before the offset was added.
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

    fn exec_arm(&mut self, instr: ArmInstruction) -> Cycles {
        use armv4t_decoder::arm::ArmInstruction::*;
        log::debug!("{}", instr);
        let cond = instr.condition();
        if !self.arm_cond_satisfied(cond) {
            log::debug!("Instruction skipped - condition not satisfied");
            return Cycles(1);
        }
        match instr {
            BranchAndExchange(op) => self.arm_branch_and_exchange(op),
            BlockDataTransfer(op) => self.arm_block_data_transfer(op),
            BranchAndBranchWithLink(op) => self.arm_branch_with_link(op),
            SoftwareInterrupt(op) => self.arm_software_interrupt(op),
            Undefined(op) => self.arm_undefined_op(op),
            SingleDataTransfer(op) => self.arm_single_data_transfer(op),
            SingleDataSwap(op) => self.arm_single_data_swap(op),
            HalfwordDataTransfer(op) => self.arm_halfword_data_transfer(op),
            Multiply(op) => self.arm_multiply(op),
            MultiplyLong(op) => self.arm_multiply_long(op),
            PsrTransferMrs(op) => self.arm_psr_transfer_mrs(op),
            PsrTransferMsr(op) => self.arm_psr_transfer_msr(op),
            PsrTransferMsrImm(op) => self.arm_psr_transfer_imm(op),
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

    fn exec_thumb(&mut self, instr: ThumbInstruction) -> Cycles {
        use armv4t_decoder::thumb::ThumbInstruction::*;

        match instr {
            AddOffsetToStackPointer(op) => self.thumb_add_offset_sp(op),
            SoftwareInterrupt(op) => {
                todo!()
            }
            UnconditionalBranch(op) => {
                todo!()
            }
            MultipleLoadstore(op) => {
                todo!()
            }
            LongBranchWithLink(op) => {
                todo!()
            }
            PushPopRegister(op) => {
                todo!()
            }
            LoadStoreHalfWord(op) => {
                todo!()
            }
            MoveShiftedRegister(op) => {
                todo!()
            }
            SpRelativeLoadStore(op) => {
                todo!()
            }
            LoadAddress(op) => {
                todo!()
            }
            ConditionalBranch(op) => {
                todo!()
            }
            LoadStoreImmOffset(op) => {
                todo!()
            }
            LoadStoreRegOffset(op) => {
                todo!()
            }
            LoadStoreSignExtHalfwordByte(op) => {
                todo!()
            }
            PcRelativeLoad(op) => {
                todo!()
            }
            HiRegOpsBranchExchange(op) => {
                todo!()
            }
            AluOps(op) => {
                todo!()
            }
            MoveCompAddSubtractImm(op) => {
                todo!()
            }
            AddSub(op) => {
                todo!()
            }
        }
    }

    fn thumb_add_offset_sp(&mut self, op: thumb::add_offset_to_stack_pointer::Op) -> Cycles {
        let cpsr = self.registers.get_cpsr();
        let offset = op.get_imm() as i32;
        let sp = self.registers.read(STACK_POINTER);
        let op = if offset < 0 { OpCode::Sub } else { OpCode::Add };
        let AluResult { value, .. } =
            Alu::exec_op(op, sp, offset as u32, AluSetCpsr::Preserve(cpsr));
        self.registers.write(STACK_POINTER, value);
        // TODO: Cycles should be same as arm::data_processing of the equivalent instruction
        Cycles(1)
    }

    fn arm_branch_and_exchange(&mut self, op: branch_and_exchange::Op) -> Cycles {
        let val = self.registers.read(op.rn());
        let switch_to_thumb = val & 1 == 1;
        self.registers.write(PROGRAM_COUNTER, val);
        if switch_to_thumb {
            self.registers.switch_instr_mode(InstructionMode::Thumb)
        }
        // TODO: Make cycle cost more accurate by looking at what memory region we would refill the pipeline from,
        // had this been a real GBA. Should take 2S + 1N cycles.
        Cycles(12)
    }

    fn arm_block_data_transfer(&mut self, op: block_data_transfer::Op) -> Cycles {
        use LoadOrStore::*;
        use PreOrPostIndexing::*;
        use UpOrDown::*;

        let offset: i32 = match op.up_or_down() {
            Up => 4,
            Down => -4,
        };
        let base_reg = op.base_reg();
        let base_addr = self.registers.read(base_reg) as i32;
        let mut reg_list = op.reg_list().to_vec();
        let mut address = match op.pre_post_indexing() {
            Pre => base_addr + offset,
            Post => base_addr,
        } as i32;

        let reg_iter = match op.up_or_down() {
            Up => reg_list.into_iter(),
            Down => {
                reg_list.reverse();
                reg_list.into_iter()
            }
        };

        let mut mem = self.memory.borrow_mut();
        let mut cycle_cost = Cycles(0);
        for reg in reg_iter {
            cycle_cost += match op.load_store() {
                Store => mem.write_le_word(address as u32, self.registers.read(reg)),
                Load => {
                    let (val, cycles) = mem.read_le_word(address as u32);
                    self.registers.write(reg, val);
                    cycles
                }
            };
            address += offset;
        }
        if op.psr_force_user() {
            todo!("S-bit should do something special")
        }
        if op.write_back() {
            let wb_addr = match op.pre_post_indexing() {
                Post => address,
                Pre => address - offset,
            };
            self.registers.write(base_reg, wb_addr as u32);
        }
        // TODO: Make nS + 1N + 1I for LDM
        // (n+1)S + 2N + 1I for LDM PC
        // (n-1)S + 2N for STM
        // where n = amount of words transferred
        cycle_cost
    }

    fn arm_branch_with_link(&mut self, op: branch_and_link::Op) -> Cycles {
        use RegisterName::*;
        // `offset` will be encoded with -8 (two instructions behind)
        // but since we don't implement pipelining, CPU will only be one instruction ahead.
        // Correct this by adding one instruction (= 4) to the branch offset.
        const INSTR_SIZE: i32 = WORD as i32;
        let offset = op.offset_get() + INSTR_SIZE;
        let old_pc = self.pc_add_offset(offset);
        if op.link() {
            // Link-reg contains address following the BL instruction
            self.registers.write(R14, old_pc);
        }
        // TODO: make 2S + 1N
        Cycles(12)
    }

    fn arm_software_interrupt(&mut self, op: software_interrupt::Op) -> Cycles {
        let old_cpsr = self.registers.read_cpsr();
        self.registers.switch_cpu_mode(Supervisor);
        self.registers.write(PROGRAM_COUNTER, SUPERVISOR_PC);
        self.registers.write_spsr(old_cpsr);
        let bios_op = op.comment();
        todo!("Call relevant BIOS function");
        // TODO: Make 2S + 1N
        Cycles(12)
    }

    fn arm_undefined_op(&mut self, op: undefined_instr::Op) -> Cycles {
        self.registers.switch_cpu_mode(ProcessorMode::Undefined);
        // TODO: Set PC to und exception vector address
        // TODO: Make 2S + 1I + 1N
        Cycles(12)
    }

    fn arm_single_data_transfer(&mut self, op: single_data_transfer::Op) -> Cycles {
        use ByteOrWord::*;
        use LoadOrStore::*;
        use PreOrPostIndexing::*;
        use UpOrDown::*;

        let mut total_cycle_cost = Cycles(1);

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
            let cpsr = self.registers.get_cpsr_mut();
            Alu::exec_shift(
                offset_amount,
                sh_type,
                shift_amount,
                AluSetCpsr::Alter(cpsr),
            )
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

    fn arm_single_data_swap(&mut self, op: single_data_swap::Op) -> Cycles {
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

    fn arm_data_processing(&mut self, op: data_processing::Op) -> Cycles {
        let opcode = op.op();
        let lhs = self.registers.read(op.operand1());
        let rhs = if op.is_imm_operand() {
            op.operand2().as_rot_imm().value()
        } else {
            let sh_reg = op.operand2().as_shift_reg();
            self.read_shift_reg(sh_reg)
        };

        let result = if op.set_cond() {
            let cpsr = self.registers.get_cpsr_mut();
            Alu::exec_op(opcode, lhs, rhs, AluSetCpsr::Alter(cpsr))
        } else {
            let cpsr = self.registers.get_cpsr();
            Alu::exec_op(opcode, lhs, rhs, AluSetCpsr::Preserve(cpsr))
        };

        if result.write_back {
            let dest = op.dest_reg();

            // When Rd is R15 and the S flag is set the result of the operation is placed in R15 and
            // the SPSR corresponding to the current mode is moved to the CPSR. This allows state
            // changes which atomically restore both PC and CPSR. This form of instruction should
            // not be used in User mode.
            if dest == RegisterName::R15 {
                let spsr = self.registers.read_spsr();
                self.registers.write_cpsr(spsr);
            }
            self.registers.write(dest, result.value);
        }

        // TODO: Cycles should be
        // Normal: 1S
        // Reg shift: 1S + 1I
        // PC written: 2S + 1N
        // Reg shift + PC written: 2S + 1n + 1I
        Cycles(12)
    }

    fn arm_psr_transfer_msr(&mut self, op: psr_transfer_msr::Op) -> Cycles {
        let val = self.registers.read(op.rm()) >> 28;
        let dest = match op.dest_psr() {
            PsrLocation::Cpsr => self.registers.get_cpsr_mut(),
            PsrLocation::Spsr => self.registers.get_spsr_mut(),
        };

        let (new_neg, new_zero, new_carry, new_overflow) = (
            val >> 3 & 1 == 1,
            val >> 2 & 1 == 1,
            val >> 1 & 1 == 1,
            val & 1 == 1,
        );

        dest.set_neg_condition(new_neg);
        dest.set_zero_condition(new_zero);
        dest.set_cbe_condition(new_carry);
        dest.set_overflow_condition(new_overflow);

        // TODO: Cycles should be 1S
        Cycles(6)
    }

    fn arm_psr_transfer_mrs(&mut self, op: psr_transfer_mrs::Op) -> Cycles {
        let src = match op.src_psr() {
            PsrLocation::Cpsr => self.registers.read_cpsr().into_bytes(),
            PsrLocation::Spsr => self.registers.read_spsr().into_bytes(),
        };
        let dst = op.reg_dest();
        // Bytes are BE
        self.registers.write(dst, u32::from_be_bytes(src));
        // TODO: Cycles should be 1S
        Cycles(6)
    }

    fn arm_psr_transfer_imm(&mut self, op: psr_transfer_msr::OpImm) -> Cycles {
        let val = if op.is_imm_operand() {
            op.src_operand().as_rot_imm().value()
        } else {
            let reg = op.src_operand().as_reg();
            self.registers.read(reg)
        } >> 28;
        let dest = match op.dest_psr() {
            PsrLocation::Cpsr => self.registers.get_cpsr_mut(),
            PsrLocation::Spsr => self.registers.get_spsr_mut(),
        };

        let (new_neg, new_zero, new_carry, new_overflow) = (
            val >> 3 & 1 == 1,
            val >> 2 & 1 == 1,
            val >> 1 & 1 == 1,
            val & 1 == 1,
        );

        dest.set_neg_condition(new_neg);
        dest.set_zero_condition(new_zero);
        dest.set_cbe_condition(new_carry);
        dest.set_overflow_condition(new_overflow);

        // TODO: Cycles should be 1S
        Cycles(6)
    }

    fn arm_multiply(&mut self, op: multiply::Op) -> Cycles {
        use AccumulateType::*;
        let lhs = self.registers.read(op.rs());
        let rhs = self.registers.read(op.rm());
        let acc = if op.accumulate() == MultiplyAndAccumulate {
            self.registers.read(op.rn())
        } else {
            0
        };
        let cpsr = if op.set_cond() {
            AluSetCpsr::Alter(self.registers.get_cpsr_mut())
        } else {
            AluSetCpsr::Preserve(self.registers.get_cpsr())
        };

        let result = Alu::exec_mul(lhs, rhs, acc, cpsr);
        let dst = op.reg_dest();
        self.registers.write(dst, result);

        // TODO: Cycles should be MUL: 1S + mI and MLA: 1S + (m+1)I
        // where m is:
        // 1 if bits [32:8] of the multiplier operand are all zero or all one.
        // 2 if bits [32:16] of the multiplier operand are all zero or all one.
        // 3 if bits [32:24] of the multiplier operand are all zero or all one.
        // 4 in all other cases.
        // Do we want to be this precise though?
        Cycles(20)
    }

    fn arm_multiply_long(&mut self, op: multiply_long::Op) -> Cycles {
        use AccumulateType::*;
        let lhs = self.registers.read(op.rs());
        let rhs = self.registers.read(op.rm());
        let dst_lo = op.reg_dest_lo();
        let dst_hi = op.reg_dest_hi();

        let acc = if op.accumulate() == MultiplyAndAccumulate {
            let hi = self.registers.read(dst_lo) as u64;
            let lo = self.registers.read(dst_hi) as u64;
            (hi << 32) | lo
        } else {
            0
        } as u64;
        let sign = op.signedness();

        let cpsr = if op.set_cond() {
            AluSetCpsr::Alter(self.registers.get_cpsr_mut())
        } else {
            AluSetCpsr::Preserve(self.registers.get_cpsr())
        };

        let (res_hi, res_lo) = Alu::exec_mul_long(lhs, rhs, acc, cpsr, sign);

        self.registers.write(dst_hi, res_hi);
        self.registers.write(dst_lo, res_lo);

        // TODO: Cycles should be MULL: 1S + (m+1)I, and MLAL: 1S + (m+2)I
        // where `m` depends on how many bits are zero (look at docs)
        // Do we want to be this precise though?
        Cycles(20)
    }

    fn arm_halfword_data_transfer(&mut self, op: halfword_data_transfer::Op) -> Cycles {
        use LoadOrStore::*;
        use PreOrPostIndexing::*;
        use ShType::*;
        use UpOrDown::*;

        let mut offset = if op.is_imm_offset() {
            op.imm_offset() as i32
        } else {
            self.registers.read(op.reg_offset()) as i32
        };

        let mut addr = self.registers.read(op.base_reg()) as i32;

        if op.up_or_down() == Down {
            offset = -offset;
        }
        if op.pre_post_indexing() == Pre {
            addr += offset;
        }
        let mut addr = addr as u32;

        let cycles = match (op.load_store(), op.sh_type()) {
            (Store, _) => {
                let value = self.registers.read(op.reg_dest()) as u16;
                self.memory.borrow_mut().write_le_halfword(addr, value)
            }
            (Load, SignedByte) => {
                let (val, cycles) = self.memory.borrow().read_byte(addr);
                let val = val as i8 as i32;
                self.registers.write(op.reg_dest(), val as u32);
                cycles
            }
            (Load, SignedHalfwords) => {
                let (val, cycles) = self.memory.borrow().read_le_halfword(addr);
                let val = val as i16 as i32;
                self.registers.write(op.reg_dest(), val as u32);
                cycles
            }
            (Load, UnsignedHalfwords) => {
                let (val, cycles) = self.memory.borrow().read_le_halfword(addr);
                self.registers.write(op.reg_dest(), val as u32);
                cycles
            }
            _ => unreachable!(),
        };
        if op.pre_post_indexing() == Post {
            addr += offset as u32;
        }

        if op.write_back() {
            self.registers.write(op.base_reg(), addr);
        }

        // TODO: Cycles should be
        // Normal LDR: 1S + 1N + 1I
        // LDR PC: 2S + 2N + 1I
        // STRH: 2N
        cycles
    }

    fn read_shift_reg(&mut self, sh_reg: ShiftRegister) -> u32 {
        // TODO LSL #0 is a special case, where the shifter carry out is the old value of the CPSR C
        // flag. The contents of Rm are used directly as the second operand
        let offset_amount = self.registers.read(sh_reg.reg());
        let shift_amount = if sh_reg.shift_amt_in_reg() {
            // TODO
            // Only the least significant byte of the contents of Rs is used to determine the shift
            // amount. Rs can be any general register other than R15.
            // If this byte is zero, the unchanged contents of Rm will be used as the second operand,
            // and the old value of the CPSR C flag will be passed on as the shifter carry output.

            let sh_amt_reg = sh_reg.shift_reg();
            self.registers.read(sh_amt_reg) as u8
        } else {
            sh_reg.shift_amt()
        };
        let sh_type = sh_reg.shift_type();
        let cpsr = self.registers.get_cpsr_mut();
        Alu::exec_shift(
            offset_amount,
            sh_type,
            shift_amount,
            AluSetCpsr::Alter(cpsr),
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        cartridge::Cartridge, memcontroller::CART_ROM_START,
        registers::psr::DEFAULT_STACKPOINTER_USER,
    };
    use byteorder::{LittleEndian, WriteBytesExt};
    use RegisterName::*;

    fn vecu32_to_vecu8(vec32: Vec<u32>) -> Vec<u8> {
        let mut vec8: Vec<u8> = vec![];
        for i in vec32 {
            vec8.write_u32::<LittleEndian>(i).unwrap();
        }
        vec8
    }

    fn setup_cpu(start_memory: Vec<u8>) -> Cpu {
        let memory = Rc::new(RefCell::new(MemoryController::new()));
        memory
            .borrow_mut()
            .insert_cartridge(Cartridge::new_with_contents(start_memory));
        Cpu::new(memory, CART_ROM_START as u32)
    }

    /// `expected_mem` is a vector of (address, value)
    fn assert_memory_state_eq(mem: &Rc<RefCell<MemoryController>>, expected_mem: &[(u32, u32)]) {
        let mem = mem.borrow();
        for &(addr, expected_val) in expected_mem {
            let (actual_val, _) = mem.read_le_word(addr);
            assert_eq!(
                actual_val, expected_val,
                "\n{:#X} - Expected {} ({:#X}), but actual was {} ({:#X})",
                addr, expected_val, expected_val, actual_val, actual_val
            );
        }
    }

    fn assert_cpu_state_eq(cpu: &Cpu, expected_regs: &[(RegisterName, u32)]) {
        for &(reg, expected_val) in expected_regs {
            let actual_val = cpu.registers.read(reg);
            assert_eq!(
                actual_val, expected_val,
                "\n{} - Expected {} ({:#X}), but actual was {} ({:#X})",
                reg, expected_val, expected_val, actual_val, actual_val
            );
        }
    }

    #[test]
    fn test_ldm_stm() {
        let mut cpu = setup_cpu(vecu32_to_vecu8(vec![
            0xe3a00001, // mov r0, #1
            0xe3a01002, // mov r1, #2
            0xe92d8003, // stmfd sp!, {r0-r1, pc}
            0xe3a00003, // mov r0, #3
            0xe3a01004, // mov r1, #4
            0xe8bd8003, // ldmfd sp!, {r0-r1, pc}
        ]));

        // mov r0, #1
        cpu.tick();
        assert_cpu_state_eq(&cpu, &[(R0, 1)]);

        // mov r1, #2
        cpu.tick();
        assert_cpu_state_eq(&cpu, &[(R1, 2)]);

        // stmfd sp!, {r0-r1, pc}
        cpu.tick();
        let pc = cpu.registers.read(PROGRAM_COUNTER);
        assert_memory_state_eq(
            &cpu.memory,
            &[
                (DEFAULT_STACKPOINTER_USER - 4, pc),
                (DEFAULT_STACKPOINTER_USER - 8, 2),
                (DEFAULT_STACKPOINTER_USER - 12, 1),
            ],
        );
        assert_cpu_state_eq(&cpu, &[(STACK_POINTER, DEFAULT_STACKPOINTER_USER - 12)]);

        // mov r0, #3
        cpu.tick();
        assert_cpu_state_eq(&cpu, &[(R0, 3)]);

        // mov r1, #4
        cpu.tick();
        assert_cpu_state_eq(&cpu, &[(R1, 4)]);

        // ldmfd sp!, {r0-r1, pc}
        cpu.tick();
        let pc = cpu.registers.read(PROGRAM_COUNTER);
        assert_memory_state_eq(
            &cpu.memory,
            &[
                (DEFAULT_STACKPOINTER_USER - 4, pc),
                (DEFAULT_STACKPOINTER_USER - 8, 2),
                (DEFAULT_STACKPOINTER_USER - 12, 1),
            ],
        );
        assert_cpu_state_eq(
            &cpu,
            &[(STACK_POINTER, DEFAULT_STACKPOINTER_USER), (R0, 1), (R1, 2)],
        );
    }

    #[test]
    fn test_i32_u32_conv() {
        // Sanity check
        let signed = -1_i32;
        let unsigned = signed as u32;
        let signed_again = unsigned as i32;
        assert_eq!(signed, signed_again);
    }
}
