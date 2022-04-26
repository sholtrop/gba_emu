use modular_bitfield::{bitfield, specifiers::*, BitfieldSpecifier};
use std::fmt::Display;

/*
package gba

// 27-26: 00
func IsArmALU(inst uint32) bool {
    return inst&0b0000_1100_0000_0000_0000_0000_0000_0000 == 0
}

// 27-24: 1011
func IsArmBL(inst uint32) bool {
    return inst&0b0000_1111_0000_0000_0000_0000_0000_0000 == 0b0000_1011_0000_0000_0000_0000_0000_0000
}

// 27-24: 1010
func IsArmB(inst uint32) bool {
    return inst&0b0000_1111_0000_0000_0000_0000_0000_0000 == 0b0000_1010_0000_0000_0000_0000_0000_0000
}

// 27-8: 0001_0010_1111_1111_1111 && 7-4: 0001
func IsArmBX(inst uint32) bool {
    return inst&0b0000_1111_1111_1111_1111_1111_1111_0000 == 0b0000_0001_0010_1111_1111_1111_0001_0000
}

// 27-24: 1111
func IsArmSWI(inst uint32) bool {
    return inst&0b0000_1111_0000_0000_0000_0000_0000_0000 == 0b0000_1111_0000_0000_0000_0000_0000_0000
}

// 27-25: 011
func IsArmUND(inst uint32) bool {
    return inst&0b0000_1110_0000_0000_0000_0000_0000_0000 == 0b0000_0110_0000_0000_0000_0000_0000_0000
}

// multiply

// 27-25: 000 & 7-4: 1001
func IsArmMPY(inst uint32) bool {
    return inst&0b0000_1110_0000_0000_0000_0000_1111_0000 == 0b0000_0000_0000_0000_0000_0000_1001_0000
}

// 27-25: 000 & 20: 0 & 7: 1 & 4: 0
func IsArmMPY16(inst uint32) bool {
    return inst&0b0000_1110_0001_0000_0000_0000_1001_0000 == 0b0000_0000_0000_0000_0000_0000_1000_0000
}

// loadstore

// 27-26: 01
func IsArmLDR(inst uint32) bool {
    return inst&0b0000_1100_0001_0000_0000_0000_0000_0000 == 0b0000_0100_0001_0000_0000_0000_0000_0000
}

// 27-26: 01
func IsArmSTR(inst uint32) bool {
    return inst&0b0000_1100_0001_0000_0000_0000_0000_0000 == 0b0000_0100_0000_0000_0000_0000_0000_0000
}

// 27-25: 000 & 20: 1 & 7-4: 1011
func IsArmLDRH(inst uint32) bool {
    return inst&0b0000_1110_0001_0000_0000_0000_1111_0000 == 0b0000_0000_0001_0000_0000_0000_1011_0000
}

// 27-25: 000 & 20: 1 & 7-4: 1101
func IsArmLDRSB(inst uint32) bool {
    return inst&0b0000_1110_0001_0000_0000_0000_1111_0000 == 0b0000_0000_0001_0000_0000_0000_1101_0000
}

// 27-25: 000 & 20: 1 & 7-4: 1111
func IsArmLDRSH(inst uint32) bool {
    return inst&0b0000_1110_0001_0000_0000_0000_1111_0000 == 0b0000_0000_0001_0000_0000_0000_1111_0000
}

// 27-25: 000 & 20: 0 & 7-4: 1011
func IsArmSTRH(inst uint32) bool {
    return inst&0b0000_1110_0001_0000_0000_0000_1111_0000 == 0b0000_0000_0000_0000_0000_0000_1011_0000
}

// 27-25: 100 & 20: 1
func IsArmLDM(inst uint32) bool {
    return inst&0b0000_1110_0001_0000_0000_0000_0000_0000 == 0b0000_1000_0001_0000_0000_0000_0000_0000
}

// 27-25: 100 & 20: 0
func IsArmSTM(inst uint32) bool {
    return inst&0b0000_1110_0001_0000_0000_0000_0000_0000 == 0b0000_1000_0000_0000_0000_0000_0000_0000
}

// 27-23: 0001_0 & 21-20: 00 & 11-4: 0000_1001
func IsArmSWP(inst uint32) bool {
    return inst&0b0000_1111_1011_0000_0000_1111_1111_0000 == 0b0000_0001_0000_0000_0000_0000_1001_0000
}

// 27-23: 0001_0 & 21-16: 00_1111 & 11-0: 0000_0000_0000
func IsArmMRS(inst uint32) bool {
    return inst&0b0000_1111_1011_1111_0000_1111_1111_1111 == 0b0000_0001_0000_1111_0000_0000_0000_0000
}

// 27-26: 00 & 24-23: 10 & 21-20: 10 & 15-12: 1111
func IsArmMSR(inst uint32) bool {
    return inst&0b0000_1101_1011_0000_1111_0000_0000_0000 == 0b0000_0001_0010_0000_1111_0000_0000_0000
}


*/
use modular_bitfield::Specifier;

const INSTR_SIZE: u8 = 32;
const COND_SIZE: u8 = 4;

#[derive(BitfieldSpecifier, Debug, Clone, Copy, PartialEq, Eq)]
#[bits = 4]
pub enum Condition {
    Eq = 0b0000, // Z set
    Ne = 0b0001, // Z clear
    Cs = 0b0010, // C set
    Cc = 0b0011, // C clear
    Mi = 0b0100, // N set
    Pl = 0b0101, // N clear
    Vs = 0b0110, // V set
    Vc = 0b0111, // V clear
    Hi = 0b1000, // C set && Z clear
    Ls = 0b1001, // C clear || Z set
    Ge = 0b1010, // N equals V
    Lt = 0b1011, // N not equal V
    Gt = 0b1100, // Z clear && (V equals V)
    Le = 0b1101, // Z set || (N not equal V)
    Al = 0b1110, // Always - Instruction is always executed
}

impl Display for Condition {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match &self {
                Condition::Eq => "EQ",
                Condition::Ne => "NE",
                Condition::Cs => "CS",
                Condition::Cc => "CC",
                Condition::Mi => "MI",
                Condition::Pl => "PL",
                Condition::Vs => "VS",
                Condition::Vc => "VC",
                Condition::Hi => "HI",
                Condition::Ls => "LS",
                Condition::Ge => "GE",
                Condition::Lt => "LT",
                Condition::Gt => "GT",
                Condition::Le => "LE",
                Condition::Al => "AL",
            }
        )
    }
}

#[derive(BitfieldSpecifier, Clone, Copy, PartialEq, Eq, Debug)]
#[bits = 2]
pub enum ShiftType {
    LogicalLeft = 0b00,
    LogicalRight = 0b01,
    ArithmeticRight = 0b10,
    RotateRight = 0b11,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum ShiftSource {
    Amount(u8),
    Register(RegisterName),
}

impl From<u16> for ShiftSource {
    fn from(n: u16) -> Self {
        if ((n >> 4) & 1) == 1 {
            Self::Register(RegisterName::from((n >> 8) & 0b1111))
        } else {
            Self::Amount(((n >> 7) & 0b11111) as u8)
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct ShiftRegister {
    name: RegisterName,
    shift: (ShiftSource, ShiftType),
}

impl Display for ShiftRegister {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} [{:#?}]", self.name, self.shift)
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum RegisterName {
    R0,
    R1,
    R2,
    R3,
    R4,
    R5,
    R6,
    R7,
    R8,
    R9,
    R10,
    R11,
    R12,
    R13,
    R14,
}

use RegisterName::*;

impl From<u16> for RegisterName {
    fn from(n: u16) -> Self {
        match n {
            0 => R0,
            1 => R1,
            2 => R2,
            3 => R3,
            4 => R4,
            5 => R5,
            6 => R6,
            7 => R7,
            8 => R8,
            9 => R9,
            10 => R10,
            11 => R11,
            12 => R12,
            13 => R13,
            14 => R14,
            _ => panic!("Invalid register number {}", n),
        }
    }
}

impl Display for RegisterName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "R{}",
            match &self {
                RegisterName::R0 => "0",
                RegisterName::R1 => "1",
                RegisterName::R2 => "2",
                RegisterName::R3 => "3",
                RegisterName::R4 => "4",
                RegisterName::R5 => "5",
                RegisterName::R6 => "6",
                RegisterName::R7 => "7",
                RegisterName::R8 => "8",
                RegisterName::R9 => "9",
                RegisterName::R10 => "10",
                RegisterName::R11 => "11",
                RegisterName::R12 => "12",
                RegisterName::R13 => "13",
                RegisterName::R14 => "14",
            }
        )
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Signedness {
    Signed,
    Unsigned,
}

use Signedness::*;

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Accumulate {
    MultiplyOnly,
    MultiplyAndAccumulate,
}

use Accumulate::*;

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum SetConditionCodes {
    Set,
    DontSet,
}

impl Display for SetConditionCodes {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Set => "S",
                Self::DontSet => "",
            }
        )
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum ByteOrWord {
    Byte,
    Word,
}

impl Display for ByteOrWord {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Byte => "B",
                Self::Word => "",
            }
        )
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct DataOperands {
    op1: Option<RegisterName>,
    op2: DataOperand,
    dst: RegisterName,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum DataOpcode {
    And, // Logical AND
    Eor, // Exclusive OR
    Sub, // Subtract
    Rsb, // Reverse subtract
    Add, // Add
    Adc, // Add with carry
    Sbc, // Subtract with carry
    Rsc, // Reverse subtract with carry
    Tst, // Test bits
    Teq, // Test bitwise equality
    Cmp, // Compare
    Cmn, // Compare negative
    Orr, // Or
    Mov, // Move register or constant
    Bic, // Bit clear
    Mvn, // Move negative register
}

use DataOpcode::*;

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct DataOp {
    opcode: DataOpcode,
    operands: DataOperands,
}

impl DataOp {
    pub fn get_operands(&self) -> (String, Option<String>, String) {
        (
            self.operands.dst.to_string(),
            self.operands.op1.and_then(|o| Some(o.to_string())),
            self.operands.op2.to_string(),
        )
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Op {
    Data(DataOp), // All Data Processing ops
    B,            // Branch
    Bl,           // Branch with link
    Bx,           // Branch and Exchange
    Cdp,          // Coprocessor Data Processing
    Ldc,          // Load coprocessor from memory
    Ldm,          // Load multiple regisers
    Ldr,          // Load register from memory
    Mcr,          // Move CPU register to coprocessor register
    Mrc,          // Move from coprocessor register to CPU register
    Mrs,          // Move PSR status/flags to register
    Msr,          // Move register to PSR status/flags
    // Multiply
    Mul {
        accumulate: Accumulate,
        set_condition: SetConditionCodes,
        op1: RegisterName,
        op2: RegisterName,
    },
    // Multiply long
    Mull {
        sign: Signedness,
        accumulate: Accumulate,
        set_condition: SetConditionCodes,
        dest_low: RegisterName,
        dest_high: RegisterName,
        op1: RegisterName,
        op2: RegisterName,
    },
    Stc, // Store coprocessor register to memory
    Stm, // Store multiple
    Str, // Store register to memory
    // Software interrupt
    Swi {
        comment_field: Immediate,
    },
    // Swap register with memory
    Swp {
        src: RegisterName,
        dst: RegisterName,
        base: RegisterName,
        size: ByteOrWord,
    },
    Unknown,
}

impl Display for Op {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match &self {
                Op::Data(DataOp { opcode, .. }) => match opcode {
                    Adc => "ADC",
                    Add => "ADD",
                    And => "AND",
                    Cmn => "CMN",
                    Cmp => "CMP",
                    Eor => "EOR",
                    Mov => "MOV",
                    Rsb => "RSB",
                    Rsc => "RSC",
                    Sbc => "SBC",
                    Sub => "SUB",
                    Orr => "ORR",
                    Teq => "TEQ",
                    Tst => "TST",
                    Bic => "BIC",
                    Mvn => "MVN",
                },
                Op::B => "B",
                Op::Bl => "BL",
                Op::Bx => "BX",
                Op::Cdp => "CDP",
                Op::Ldc => "LDC",
                Op::Ldm => "LDM",
                Op::Ldr => "LDR",
                Op::Mcr => "MCR",
                Op::Mrc => "MRC",
                Op::Mrs => "MRS",
                Op::Msr => "MSR",
                Op::Mul { accumulate, .. } => {
                    match accumulate {
                        MultiplyOnly => "MUL",
                        MultiplyAndAccumulate => "MULA",
                    }
                }
                Op::Mull {
                    sign, accumulate, ..
                } => {
                    match (sign, accumulate) {
                        (Signed, MultiplyOnly) => "SMULL",
                        (Unsigned, MultiplyOnly) => "UMULL",
                        (Signed, MultiplyAndAccumulate) => "SMLAL",
                        (Unsigned, MultiplyAndAccumulate) => "UMLAL",
                    }
                }

                Op::Stc => "STC",
                Op::Stm => "STM",
                Op::Str => "STR",
                Op::Swi { .. } => "SWI",
                Op::Swp { .. } => "SWP",
                Op::Unknown => "UNKNOWN",
            }
        )
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Immediate(u32);

impl Display for Immediate {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl From<u32> for Immediate {
    fn from(n: u32) -> Self {
        Self(n)
    }
}

#[derive(Clone, Copy)]
#[bitfield(bits = 12)]
pub struct RotatedImmediate {
    value: B8,  // To be zero-extended to 32 bits
    rotate: B4, // `value` is right-rotated by twice this amount
}

impl RotatedImmediate {
    pub const fn new_imm(value: u8, rotate: u8) -> Self {
        Self {
            bytes: [value, rotate],
        }
    }

    pub fn imm_value(&self) -> u32 {
        let value = self.value() as u32;
        value.rotate_right(self.rotate() as u32 * 2)
    }
}

impl core::fmt::Debug for RotatedImmediate {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} rot:{:b}", self.value(), self.rotate())
    }
}

impl PartialEq for RotatedImmediate {
    fn eq(&self, other: &Self) -> bool {
        self.value() == other.value() && self.rotate() == other.rotate()
    }
}

impl Eq for RotatedImmediate {}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DataOperand {
    RotatedImmediate(RotatedImmediate),
    ShiftRegister(ShiftRegister),
}

impl Display for DataOperand {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match &self {
                DataOperand::ShiftRegister(ShiftRegister { name, .. }) => format!("R{}", name),
                DataOperand::RotatedImmediate(ri) => format!("#{}", ri.imm_value()),
            }
        )
    }
}

impl From<RotatedImmediate> for DataOperand {
    fn from(ri: RotatedImmediate) -> Self {
        DataOperand::RotatedImmediate(ri)
    }
}

impl From<ShiftRegister> for DataOperand {
    fn from(sr: ShiftRegister) -> Self {
        Self::ShiftRegister(sr)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Operand {
    Register(RegisterName),
    Immediate(Immediate),
}

impl Display for Operand {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match &self {
                Self::Register(r) => format!("R{}", r),
                Self::Immediate(i) => format!("#{}", i),
            }
        )
    }
}

impl From<RegisterName> for Operand {
    fn from(name: RegisterName) -> Self {
        Self::Register(name)
    }
}

impl From<Immediate> for Operand {
    fn from(i: Immediate) -> Self {
        Self::Immediate(i)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Instruction {
    condition: Condition,
    op: Op,
}

impl Instruction {
    pub fn get_operands(
        &self,
    ) -> (
        Option<String>,
        Option<String>,
        Option<String>,
        Option<String>,
    ) {
        match self.op {
            Op::Data(op) => {
                let (dst, op1, op2) = op.get_operands();
                (Some(dst), op1, Some(op2), None)
            }
            Op::Mull {
                dest_low,
                dest_high,
                op1,
                op2,
                ..
            } => (
                Some(dest_low.to_string()),
                Some(dest_high.to_string()),
                Some(op1.to_string()),
                Some(op2.to_string()),
            ),
            Op::Swp { src, dst, base, .. } => (
                Some(dst.to_string()),
                Some(src.to_string()),
                Some(base.to_string()),
                None,
            ),
            Op::Swi { comment_field } => (Some(comment_field.to_string()), None, None, None),
            _ => (None, None, None, None),
        }
    }
}

impl Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // Certain instructions can add an extra letter to the mnemonic.
        // E.g. MUL can become MULEQS with condition code EQ and set_condition=true
        let extra_letter = match self.op {
            Op::Swp { size, .. } => size.to_string(),
            Op::Mul { set_condition, .. } | Op::Mull { set_condition, .. } => {
                set_condition.to_string()
            }
            _ => "".into(),
        };

        match self.get_operands() {
            (None, None, None, None) => {
                write!(f, "{}{}{}", self.op, self.condition, extra_letter)
            }
            (Some(op1), None, None, None) => {
                write!(f, "{}{}{} {}", self.op, self.condition, extra_letter, op1)
            }
            (Some(op1), Some(op2), None, None) => {
                write!(
                    f,
                    "{}{}{} {},{}",
                    self.op, self.condition, extra_letter, op1, op2
                )
            }
            (Some(op1), Some(op2), Some(op3), None) => {
                write!(
                    f,
                    "{}{}{} {},{},{}",
                    self.op, self.condition, extra_letter, op1, op2, op3
                )
            }
            (Some(op1), Some(op2), Some(op3), Some(op4)) => {
                write!(
                    f,
                    "{}{}{} {},{},{},{}",
                    self.op, self.condition, extra_letter, op1, op2, op3, op4
                )
            }
            _ => unreachable!("A previous Operand was `None`"),
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum OpFormat {
    BranchAndBranchExchange,
    BlockDataTransfer,
    BranchAndBranchWithLink,
    SoftwareInterrupt,
    Undefined,
    SingleDataTransfer,
    SingleDataSwap,
    HalfwordDataTransferRegister,
    HalfwordDataTransferImmediate,
    Multiply,
    PsrTransferMrs,
    PsrTransferMsr,
    DataProcessing, // ALU instructions
    Unimplemented,
}

pub struct Decoder {}

impl Decoder {
    pub fn decode(instr: u32) -> Instruction {
        let format = Decoder::get_op_format(instr);
        Decoder::get_instr(format, instr)
    }

    fn get_op_format(instr: u32) -> OpFormat {
        if Decoder::is_branch_and_branch_exchange(instr) {
            OpFormat::BranchAndBranchExchange
        } else if Decoder::is_block_data_transfer(instr) {
            OpFormat::BlockDataTransfer
        } else if Decoder::is_branch_and_branch_with_link(instr) {
            OpFormat::BranchAndBranchWithLink
        } else if Decoder::is_software_interrupt(instr) {
            OpFormat::SoftwareInterrupt
        } else if Decoder::is_undefined(instr) {
            OpFormat::Undefined
        } else if Decoder::is_single_data_transfer(instr) {
            OpFormat::SingleDataTransfer
        } else if Decoder::is_single_data_swap(instr) {
            OpFormat::SingleDataSwap
        } else if Decoder::is_halfword_data_transfer_register(instr) {
            OpFormat::HalfwordDataTransferRegister
        } else if Decoder::is_halfword_data_transfer_immediate(instr) {
            OpFormat::HalfwordDataTransferImmediate
        } else if Decoder::is_multiply(instr) {
            OpFormat::Multiply
        } else if Decoder::is_psr_transfer_mrs(instr) {
            OpFormat::PsrTransferMrs
        } else if Decoder::is_psr_transfer_msr(instr) {
            OpFormat::PsrTransferMsr
        } else if Decoder::is_data_processing(instr) {
            OpFormat::DataProcessing
        } else {
            OpFormat::Unimplemented
        }
    }

    fn get_instr(format: OpFormat, instr: u32) -> Instruction {
        let condition = Decoder::get_condition(instr);
        match format {
            OpFormat::BranchAndBranchExchange => {
                let link_bit = instr >> 24;
                let op = if link_bit == 1 { Op::Bl } else { Op::B };
                Instruction { condition, op }
            }
            OpFormat::SoftwareInterrupt => {
                let comment_field = (instr & ((1 << 24) - 1)).into();
                Instruction {
                    condition,
                    op: Op::Swi { comment_field },
                }
            }
            OpFormat::DataProcessing => {
                let alu_op = Decoder::get_dp_op(instr);
                Instruction {
                    condition,
                    op: Op::Data(alu_op),
                }
            }
            _ => todo!("{:?}", format),
        }
    }

    fn get_dp_op(instr: u32) -> DataOp {
        let opcode_bits = (instr >> 21) & 0b1111;
        let opcode = match opcode_bits {
            0b0000 => And,
            0b0001 => Eor,
            0b0010 => Sub,
            0b0011 => Rsb,
            0b0100 => Add,
            0b0101 => Adc,
            0b0110 => Sbc,
            0b0111 => Rsc,
            0b1000 => Tst,
            0b1001 => Teq,
            0b1010 => Cmp,
            0b1011 => Cmn,
            0b1100 => Orr,
            0b1101 => Mov,
            0b1110 => Bic,
            0b1111 => Mvn,
            _ => panic!("Opcode {opcode_bits} invalid for data processing format"),
        };
        let operands = {
            let op1 = if opcode == DataOpcode::Mov {
                None
            } else {
                let bits = ((instr >> 16) & 0b1111) as u16;
                Some(RegisterName::from(bits))
            };
            let op2 = (instr & ((1 << 12) - 1)) as u16;
            let is_immediate = ((instr >> 25) & 1) == 1;
            let op2 = if is_immediate {
                let rot_imm = RotatedImmediate::from_bytes(op2.to_ne_bytes());
                DataOperand::RotatedImmediate(rot_imm)
            } else {
                let shift_type = ShiftType::from_bytes(((op2 >> 5) & 0b11) as u8).unwrap();
                let shift_source = ShiftSource::from(op2);
                let reg = op2 & 0b1111;
                let op2 = ShiftRegister {
                    name: reg.into(),
                    shift: (shift_source, shift_type),
                };
                DataOperand::ShiftRegister(op2)
            };

            let dst = ((instr >> 12) & 0b1111) as u16;
            let dst = RegisterName::from(dst);
            DataOperands { dst, op1, op2 }
        };
        DataOp { opcode, operands }
    }

    /// Get the condition field of the ARM instruction
    fn get_condition(instr: u32) -> Condition {
        let cond_bits: u8 = (instr >> (INSTR_SIZE - COND_SIZE)).try_into().unwrap();
        Condition::from_bytes(cond_bits)
            .unwrap_or_else(|e| panic!("Invalid bit pattern for ARM condition: {e}"))
    }

    fn is_branch_and_branch_exchange(instr: u32) -> bool {
        let branch_and_exchange_format = 0b0000_0001_0010_1111_1111_1111_0001_0000;

        let format_mask = 0b0000_1111_1111_1111_1111_1111_1111_0000;

        let extracted_format = instr & format_mask;

        extracted_format == branch_and_exchange_format
    }

    fn is_block_data_transfer(instr: u32) -> bool {
        let block_data_transfer_format = 0b0000_1000_0000_0000_0000_0000_0000_0000;

        let format_mask = 0b0000_1110_0000_0000_0000_0000_0000_0000;

        let extracted_format = instr & format_mask;

        extracted_format == block_data_transfer_format
    }

    fn is_branch_and_branch_with_link(instr: u32) -> bool {
        let branch_format = 0b0000_1010_0000_0000_0000_0000_0000_0000;
        let branch_with_link_format = 0b0000_1011_0000_0000_0000_0000_0000_0000;

        let format_mask = 0b0000_1111_0000_0000_0000_0000_0000_0000;

        let extracted_format = instr & format_mask;

        extracted_format == branch_format || extracted_format == branch_with_link_format
    }

    fn is_software_interrupt(instr: u32) -> bool {
        let software_interrupt_format = 0b0000_1111_0000_0000_0000_0000_0000_0000;

        let format_mask = 0b0000_1111_0000_0000_0000_0000_0000_0000;

        let extracted_format = instr & format_mask;

        extracted_format == software_interrupt_format
    }

    fn is_undefined(instr: u32) -> bool {
        let undefined_format = 0b0000_0110_0000_0000_0000_0000_0001_0000;

        let format_mask = 0b0000_1110_0000_0000_0000_0000_0001_0000;

        let extracted_format = instr & format_mask;

        extracted_format == undefined_format
    }

    fn is_single_data_transfer(instr: u32) -> bool {
        let single_data_transfer_format = 0b0000_0100_0000_0000_0000_0000_0000_0000;

        let format_mask = 0b0000_1100_0000_0000_0000_0000_0000_0000;

        let extracted_format = instr & format_mask;

        extracted_format == single_data_transfer_format
    }

    fn is_single_data_swap(instr: u32) -> bool {
        let single_data_swap_format = 0b0000_0001_0000_0000_0000_0000_1001_0000;

        let format_mask = 0b0000_1111_1000_0000_0000_1111_1111_0000;

        let extracted_format = instr & format_mask;

        extracted_format == single_data_swap_format
    }

    fn is_halfword_data_transfer_register(instr: u32) -> bool {
        let halfword_data_transfer_register_format = 0b0000_0000_0000_0000_0000_0000_1001_0000;

        let format_mask = 0b0000_1110_0100_0000_0000_1111_1001_0000;

        let extracted_format = instr & format_mask;

        extracted_format == halfword_data_transfer_register_format
    }

    fn is_halfword_data_transfer_immediate(instr: u32) -> bool {
        let halfword_data_transfer_immediate_format = 0b0000_0000_0100_0000_0000_0000_1001_0000;

        let format_mask = 0b0000_1110_0100_0000_0000_0000_1001_0000;

        let extracted_format = instr & format_mask;

        extracted_format == halfword_data_transfer_immediate_format
    }

    fn is_multiply(instr: u32) -> bool {
        let multiply_format = 0b0000_0000_0000_0000_0000_0000_1001_0000;
        let multiply_long_format = 0b0000_0000_1000_0000_0000_0000_1001_0000;

        let format_mask = 0b0000_1111_1000_0000_0000_0000_1111_0000;

        let extracted_format = instr & format_mask;

        extracted_format == multiply_format || extracted_format == multiply_long_format
    }

    fn is_psr_transfer_mrs(instr: u32) -> bool {
        let mrs_format = 0b0000_0001_0000_1111_0000_0000_0000_0000;

        let format_mask = 0b0000_1111_1011_1111_0000_0000_0000_0000;

        let extracted_format = instr & format_mask;

        extracted_format == mrs_format
    }

    fn is_psr_transfer_msr(instr: u32) -> bool {
        let msr_format = 0b0000_0001_0010_1001_1111_0000_0000_0000;
        let msr_flag_format = 0b0000_0001_0010_1000_1111_0000_0000_0000;

        let format_mask = 0b0000_1101_1011_1111_1111_0000_0000_0000;

        let extracted_format = instr & format_mask;

        extracted_format == msr_format || extracted_format == msr_flag_format
    }

    fn is_data_processing(instr: u32) -> bool {
        let data_processing_format = 0b0000_0000_0000_0000_0000_0000_0000_0000;

        let format_mask = 0b0000_1100_0000_0000_0000_0000_0000_0000;

        let extracted_format = instr & format_mask;

        extracted_format == data_processing_format
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // Instruction hex -> expectation mapping
    const TEST_INSTRUCTIONS: [(u32, Instruction); 4] = [
        (
            0xe2833001, // 	add	r3, r3, #1
            Instruction {
                condition: Condition::Al,
                op: Op::Data(DataOp {
                    opcode: Add,
                    operands: DataOperands {
                        op1: Some(R3),
                        op2: DataOperand::RotatedImmediate(RotatedImmediate::new_imm(1, 0)),
                        dst: R3,
                    },
                }),
            },
        ),
        (
            0xe0823003, // add r3, r2, r3
            Instruction {
                condition: Condition::Al,
                op: Op::Data(DataOp {
                    opcode: Add,
                    operands: DataOperands {
                        op1: Some(R2),
                        op2: DataOperand::ShiftRegister(ShiftRegister {
                            name: R3,
                            shift: (ShiftSource::Amount(0), ShiftType::LogicalLeft),
                        }),
                        dst: R3,
                    },
                }),
            },
        ),
        (
            0xe24dd01c, // sub sp, sp, #28
            Instruction {
                condition: Condition::Al,
                op: Op::Data(DataOp {
                    opcode: Sub,
                    // R13 = stack pointer
                    operands: DataOperands {
                        op1: Some(RegisterName::R13),
                        op2: DataOperand::RotatedImmediate(RotatedImmediate::new_imm(28, 0)),
                        dst: R13,
                    },
                }),
            },
        ),
        (
            0xe3a09a01, // mov r9, #4096
            Instruction {
                condition: Condition::Al,
                op: Op::Data(DataOp {
                    opcode: Mov,
                    operands: DataOperands {
                        op1: None,
                        op2: DataOperand::RotatedImmediate(RotatedImmediate::new_imm(1, 0b1010)),
                        dst: R9,
                    },
                }),
            },
        ),
    ];

    #[test]
    fn rotated_imm_eq_test() {
        let rot_imm1 = RotatedImmediate::new_imm(0, 0);
        let rot_imm2 = RotatedImmediate::new_imm(0, 1);
        let rot_imm3 = RotatedImmediate::new_imm(1, 0);
        let rot_imm4 = RotatedImmediate::new_imm(1, 1);
        assert_ne!(rot_imm1, rot_imm2);
        assert_ne!(rot_imm1, rot_imm3);
        assert_ne!(rot_imm1, rot_imm4);

        assert_ne!(rot_imm2, rot_imm3);
        assert_ne!(rot_imm2, rot_imm4);

        assert_ne!(rot_imm3, rot_imm4);
    }

    #[test]
    fn rotated_imm_value_test() {
        assert_eq!(RotatedImmediate::new_imm(1, 0b1010).imm_value(), 4096)
    }

    #[test]
    fn decode_instructions_test() {
        for (instr, expect_decoded) in TEST_INSTRUCTIONS.into_iter() {
            let actual_decoded = Decoder::decode(instr);
            assert_eq!(
                expect_decoded, actual_decoded,
                "\nEXPECTED:\n{:#?}\n\nACTUAL:\n{:#?}",
                expect_decoded, actual_decoded
            );
        }
    }
}
