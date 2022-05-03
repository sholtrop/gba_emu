use bitvec::array::BitArray;
use bitvec::order::Lsb0;
use bitvec::{bitarr, BitArr};
use modular_bitfield::Specifier;
use modular_bitfield::{bitfield, specifiers::*, BitfieldSpecifier};
use std::fmt::{Debug, Display};

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
                Condition::Eq => "eq",
                Condition::Ne => "ne",
                Condition::Cs => "cs",
                Condition::Cc => "cc",
                Condition::Mi => "mi",
                Condition::Pl => "pl",
                Condition::Vs => "vs",
                Condition::Vc => "vc",
                Condition::Hi => "hi",
                Condition::Ls => "ls",
                Condition::Ge => "ge",
                Condition::Lt => "lt",
                Condition::Gt => "gt",
                Condition::Le => "le",
                Condition::Al => "",
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
    R15,
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
            15 => R15,
            _ => panic!("Invalid register number {}", n),
        }
    }
}

impl Display for RegisterName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "r{}",
            match &self {
                R0 => "0",
                R1 => "1",
                R2 => "2",
                R3 => "3",
                R4 => "4",
                R5 => "5",
                R6 => "6",
                R7 => "7",
                R8 => "8",
                R9 => "9",
                R10 => "10",
                R11 => "11",
                R12 => "12",
                R13 => "13",
                R14 => "14",
                R15 => "15",
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
                Self::Set => "s",
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
                Self::Byte => "b",
                Self::Word => "",
            }
        )
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct DataOperands {
    op1: Option<RegisterName>,
    op2: DataOperand,
    dst: Option<RegisterName>,
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
    pub fn get_operands(&self) -> (Option<String>, Option<String>, String) {
        (
            self.operands.dst.map(|r| r.to_string()),
            self.operands.op1.map(|o| o.to_string()),
            self.operands.op2.to_string(),
        )
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct Offset(i32);
impl Offset {
    /// Decode a signed 2's complement 24 bit offset.
    /// The offset is shifted left 2 bits and sign-extended to 32 bits.
    /// Then 8 is added, as the program counter is always 2 instructions (=8 bytes) behind.
    pub const fn from_24bits(n: u32) -> Self {
        let signed = ((n >> 23) & 1) == 1;
        let shifted = n << 2;

        if signed {
            Self((shifted | (0b111111 << 26)) as i32 + 8)
        } else {
            Self(shifted as i32 + 8)
        }
    }
}

impl Display for Offset {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Clone, Copy)]
#[bitfield(bits = 4)]
pub struct BlockDataFlags {
    write_back: B1,          // W
    psr_force_user_mode: B1, // S
    up_down: B1,             // U
    pre_post_indexing: B1,   // P
}

impl Debug for BlockDataFlags {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}{}{}{}",
            self.pre_post_indexing(),
            self.up_down(),
            self.psr_force_user_mode(),
            self.write_back()
        )
    }
}

impl PartialEq for BlockDataFlags {
    fn eq(&self, other: &Self) -> bool {
        self.into_bytes() == other.into_bytes()
    }
}

impl Eq for BlockDataFlags {}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Op {
    Data(DataOp), // All Data Processing ops
    // Branch
    B {
        offset: Offset,
    },
    // Branch with link
    Bl {
        offset: Offset,
    },
    Bx {
        reg: RegisterName,
    }, // Branch and Exchange
    Cdp, // Coprocessor Data Processing
    Ldc, // Load coprocessor from memory
    // Load multiple regisers
    BlockData(BlockDataOp),
    Ldr, // Load register from memory
    Mcr, // Move CPU register to coprocessor register
    Mrc, // Move from coprocessor register to CPU register
    Mrs, // Move PSR status/flags to register
    Msr, // Move register to PSR status/flags
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
    // Stm, // Store multiple
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

use Op::*;

impl Display for Op {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match &self {
                Data(DataOp { opcode, .. }) => match opcode {
                    Adc => "adc".into(),
                    Add => "add".into(),
                    And => "and".into(),
                    Cmn => "cmn".into(),
                    Cmp => "cmp".into(),
                    Eor => "eor".into(),
                    Mov => "mov".into(),
                    Rsb => "rsb".into(),
                    Rsc => "rsc".into(),
                    Sbc => "sbc".into(),
                    Sub => "sub".into(),
                    Orr => "orr".into(),
                    Teq => "teq".into(),
                    Tst => "tst".into(),
                    Bic => "bic".into(),
                    Mvn => "mvn".into(),
                },
                B { .. } => "b".into(),
                Bl { .. } => "bl".into(),
                Bx { .. } => "bx".into(),
                Cdp => "cdp".into(),
                Ldc => "ldc".into(),
                BlockData(op) => op.to_string(),
                Ldr => "ldr".into(),
                Mcr => "mcr".into(),
                Mrc => "mrc".into(),
                Mrs => "mrs".into(),
                Msr => "msr".into(),
                Mul { accumulate, .. } => {
                    match accumulate {
                        MultiplyOnly => "mul".into(),
                        MultiplyAndAccumulate => "mula".into(),
                    }
                }
                Mull {
                    sign, accumulate, ..
                } => {
                    match (sign, accumulate) {
                        (Signed, MultiplyOnly) => "smull".into(),
                        (Unsigned, MultiplyOnly) => "umull".into(),
                        (Signed, MultiplyAndAccumulate) => "smlal".into(),
                        (Unsigned, MultiplyAndAccumulate) => "umlal".into(),
                    }
                }

                Stc => "stc".into(),
                Str => "str".into(),
                Swi { .. } => "swi".into(),
                Swp { .. } => "swp".into(),
                Unknown => "unknown".into(),
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

impl Debug for RotatedImmediate {
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

impl DataOperand {
    pub const fn make_shift_reg(name: RegisterName, shift: (ShiftSource, ShiftType)) -> Self {
        Self::ShiftRegister(ShiftRegister { name, shift })
    }

    /// Create a [ShiftedRegister] without a shift (= shift 0, logical left)
    pub const fn make_shift_reg_noshift(name: RegisterName) -> Self {
        Self::ShiftRegister(ShiftRegister {
            name,
            shift: (ShiftSource::Amount(0), ShiftType::LogicalLeft),
        })
    }

    pub const fn make_rot_imm(value: u8, rotate: u8) -> Self {
        Self::RotatedImmediate(RotatedImmediate::new_imm(value, rotate))
    }
}

impl Display for DataOperand {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match &self {
                DataOperand::ShiftRegister(ShiftRegister { name, .. }) => format!("{}", name),
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
                Self::Register(r) => format!("{}", r),
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
pub enum BlockDataOp {
    Ldm(BlockDataOperands),
    Stm(BlockDataOperands),
}

use BlockDataOp::*;

impl Display for BlockDataOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let (l, p, u) = match &self {
            Ldm(BlockDataOperands { flags, .. }) => (1, flags.pre_post_indexing(), flags.up_down()),
            Stm(BlockDataOperands { flags, .. }) => (0, flags.pre_post_indexing(), flags.up_down()),
        };
        write!(
            f,
            "{}",
            match (l, p, u) {
                (1, 1, 1) => "ldmib",
                (1, 0, 1) => "ldmia",
                (1, 1, 0) => "ldmdb",
                (1, 0, 0) => "ldmda",
                (0, 1, 1) => "stmib",
                (0, 0, 1) => "stmia",
                (0, 1, 0) => "stmdb",
                (0, 0, 0) => "stmda",
                _ => unreachable!(),
            }
        )
        // write!(f, "")
    }
}

impl BlockDataOp {
    pub fn get_operands(&self) -> (String, String) {
        let (write_back, set_psr, base_reg, reg_list) = match &self {
            Ldm(BlockDataOperands {
                base_reg,
                flags,
                reg_list,
            })
            | Stm(BlockDataOperands {
                base_reg,
                flags,
                reg_list,
            }) => {
                let write_back = flags.write_back();
                let set_psr = flags.psr_force_user_mode();
                let reg_list = format!(
                    "{{{}}}",
                    reg_list
                        .iter()
                        .enumerate()
                        .filter_map(|(reg, reg_bit)| {
                            if reg_bit == true {
                                Some(RegisterName::from(reg as u16).to_string())
                            } else {
                                None
                            }
                        })
                        .collect::<Vec<String>>()
                        .join(", ")
                );
                (write_back, set_psr, base_reg, reg_list)
            }
        };
        let op1 = format!("{}{}", base_reg, if write_back == 1 { "!" } else { "" });
        let op2 = format!("{}{}", reg_list, if set_psr == 1 { "^" } else { "" });
        (op1, op2)
    }
}

type RegisterBitArr = BitArr!(for 16, in u16, Lsb0);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct BlockDataOperands {
    flags: BlockDataFlags,
    base_reg: RegisterName,
    reg_list: RegisterBitArr, // Each bit corresponds to a register
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
        Option<String>, // dest (dest-lo)
        Option<String>, // op1 or dest-hi
        Option<String>, // op2 or op1
        Option<String>, // op3 or op2
    ) {
        match self.op {
            Data(op) => {
                let (dst, op1, op2) = op.get_operands();
                (dst, op1, Some(op2), None)
            }
            Mull {
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
            Swp { src, dst, base, .. } => (
                Some(dst.to_string()),
                Some(src.to_string()),
                Some(base.to_string()),
                None,
            ),
            BlockData(op) => {
                let (op1, op2) = op.get_operands();
                (None, Some(op1), Some(op2), None)
            }
            B { offset } | Bl { offset } => (Some(offset.0.to_string()), None, None, None),
            Bx { reg } => (Some(reg.to_string()), None, None, None),
            Swi { comment_field } => (Some(comment_field.to_string()), None, None, None),
            _ => todo!("{}", self.op),
        }
    }
}

impl Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // Certain instructions can add an extra letter to the mnemonic.
        // E.g. MUL can become MULEQS with condition code EQ and set_condition=true
        let extra_letter = match self.op {
            Swp { size, .. } => size.to_string(),
            Mul { set_condition, .. } | Mull { set_condition, .. } => set_condition.to_string(),
            _ => "".into(),
        };
        match self.get_operands() {
            (None, None, None, None) => {
                write!(f, "{}{}{}", self.op, self.condition, extra_letter)
            }
            (None, Some(op1), Some(op2), None) => {
                write!(
                    f,
                    "{}{}{} {}, {}",
                    self.op, self.condition, extra_letter, op1, op2
                )
            }
            (Some(dest), None, None, None) => {
                write!(f, "{}{}{} {}", self.op, self.condition, extra_letter, dest)
            }
            // op1=None and op2=Some can happen, but not vice-versa
            (Some(dest), None, Some(op2), None) => {
                write!(
                    f,
                    "{}{}{} {}, {}",
                    self.op, self.condition, extra_letter, dest, op2,
                )
            }
            (Some(dest), Some(op1), Some(op2), None) => {
                write!(
                    f,
                    "{}{}{} {}, {}, {}",
                    self.op, self.condition, extra_letter, dest, op1, op2
                )
            }
            (Some(dest), Some(op1), Some(op2), Some(op3)) => {
                write!(
                    f,
                    "{}{}{} {}, {}, {}, {}",
                    self.op, self.condition, extra_letter, dest, op1, op2, op3
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
                let reg = RegisterName::from((instr & 0b1111) as u16);
                Instruction {
                    condition,
                    op: Bx { reg },
                }
            }
            OpFormat::BranchAndBranchWithLink => {
                let link_bit = instr >> 24 & 1;
                let offset = instr & ((1 << 24) - 1);
                let offset = Offset::from_24bits(offset);

                let op = if link_bit == 1 {
                    Bl { offset }
                } else {
                    B { offset }
                };
                Instruction { condition, op }
            }
            OpFormat::SoftwareInterrupt => {
                let comment_field = (instr & ((1 << 24) - 1)).into();
                Instruction {
                    condition,
                    op: Swi { comment_field },
                }
            }
            OpFormat::DataProcessing => {
                let data_op = Decoder::get_data_op(instr);
                Instruction {
                    condition,
                    op: Data(data_op),
                }
            }
            OpFormat::BlockDataTransfer => {
                let op = Decoder::get_block_data_op(instr);
                Instruction {
                    condition,
                    op: BlockData(op),
                }
            }
            _ => todo!("{:?}", format),
        }
    }

    fn get_block_data_op(instr: u32) -> BlockDataOp {
        let base_reg = RegisterName::from((instr >> 16 & 0b1111) as u16);
        let flags = BlockDataFlags::from_bytes([(instr >> 21 & 0b1111) as u8]);
        let reg_list = RegisterBitArr::new([(instr & ((1 << 16) - 1)) as u16]);
        let operands = BlockDataOperands {
            base_reg,
            flags,
            reg_list,
        };
        let is_load = instr >> 20 & 1 == 1;
        if is_load {
            BlockDataOp::Ldm(operands)
        } else {
            BlockDataOp::Stm(operands)
        }
    }

    fn get_data_op(instr: u32) -> DataOp {
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

            let op2 = {
                let op2_bits = (instr & ((1 << 12) - 1)) as u16;
                let is_immediate = ((instr >> 25) & 1) == 1;
                if is_immediate {
                    let rot_imm = RotatedImmediate::from_bytes(op2_bits.to_ne_bytes());
                    DataOperand::RotatedImmediate(rot_imm)
                } else {
                    let shift_type = ShiftType::from_bytes(((op2_bits >> 5) & 0b11) as u8).unwrap();
                    let shift_source = ShiftSource::from(op2_bits);
                    let reg = op2_bits & 0b1111;
                    let op2 = ShiftRegister {
                        name: reg.into(),
                        shift: (shift_source, shift_type),
                    };
                    DataOperand::ShiftRegister(op2)
                }
            };
            let dst = if !(matches!(opcode, Cmp | Cmn | Teq | Tst)) {
                let dst_bits = ((instr >> 12) & 0b1111) as u16;
                Some(RegisterName::from(dst_bits))
            } else {
                None
            };
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
    use bitvec::bitarr;

    use super::*;

    // Instruction hex, assembly string, expected decoded instruction
    const TEST_INSTRUCTIONS: [(u32, &str, Instruction); 17] = [
        (
            0xe2833001,
            "add r3, r3, #1",
            Instruction {
                condition: Condition::Al,
                op: Data(DataOp {
                    opcode: Add,
                    operands: DataOperands {
                        op1: Some(R3),
                        op2: DataOperand::make_rot_imm(1, 0),
                        dst: Some(R3),
                    },
                }),
            },
        ),
        (
            0xe0823003,
            "add r3, r2, r3",
            Instruction {
                condition: Condition::Al,
                op: Data(DataOp {
                    opcode: Add,
                    operands: DataOperands {
                        op1: Some(R2),
                        op2: DataOperand::make_shift_reg_noshift(R3),
                        dst: Some(R3),
                    },
                }),
            },
        ),
        (
            0xe24dd01c,
            "sub r13, r13, #28",
            Instruction {
                condition: Condition::Al,
                op: Data(DataOp {
                    opcode: Sub,
                    operands: DataOperands {
                        op1: Some(RegisterName::R13),
                        op2: DataOperand::make_rot_imm(28, 0),
                        dst: Some(R13),
                    },
                }),
            },
        ),
        (
            0xe3a09a01,
            "mov r9, #4096",
            Instruction {
                condition: Condition::Al,
                op: Data(DataOp {
                    opcode: Mov,
                    operands: DataOperands {
                        op1: None,
                        op2: DataOperand::make_rot_imm(1, 0b1010),
                        dst: Some(R9),
                    },
                }),
            },
        ),
        (
            0xe2a5400a,
            "adc r4, r5, #10",
            Instruction {
                condition: Condition::Al,
                op: Data(DataOp {
                    opcode: Adc,
                    operands: DataOperands {
                        op1: Some(R5),
                        op2: DataOperand::make_rot_imm(10, 0),
                        dst: Some(R4),
                    },
                }),
            },
        ),
        (
            0xe0076008,
            "and r6, r7, r8",
            Instruction {
                condition: Condition::Al,
                op: Data(DataOp {
                    opcode: And,
                    operands: DataOperands {
                        op1: Some(R7),
                        op2: DataOperand::make_shift_reg_noshift(R8),
                        dst: Some(R6),
                    },
                }),
            },
        ),
        (
            0xeafffffe,
            "b 0",
            Instruction {
                condition: Condition::Al,
                op: B {
                    offset: Offset::from_24bits(0xfffffe),
                },
            },
        ),
        (
            0xebfffffe,
            "bl 0",
            Instruction {
                condition: Condition::Al,
                op: Bl {
                    offset: Offset::from_24bits(0xfffffe),
                },
            },
        ),
        (
            0xe3c920aa,
            "bic r2, r9, #170",
            Instruction {
                condition: Condition::Al,
                op: Data(DataOp {
                    opcode: Bic,
                    operands: DataOperands {
                        op1: Some(R9),
                        op2: DataOperand::make_rot_imm(170, 0),
                        dst: Some(R2),
                    },
                }),
            },
        ),
        (
            0x112fff12,
            "bxne r2",
            Instruction {
                condition: Condition::Ne,
                op: Bx { reg: R2 },
            },
        ),
        (
            0x03720010,
            "cmneq r2, #16",
            Instruction {
                condition: Condition::Eq,
                op: Data(DataOp {
                    opcode: Cmn,
                    operands: DataOperands {
                        dst: None,
                        op1: Some(R2),
                        op2: DataOperand::make_rot_imm(16, 0),
                    },
                }),
            },
        ),
        (
            0x93540000,
            "cmpls r4, #0",
            Instruction {
                condition: Condition::Ls,
                op: Data(DataOp {
                    opcode: Cmp,
                    operands: DataOperands {
                        dst: None,
                        op1: Some(R4),
                        op2: DataOperand::make_rot_imm(0, 0),
                    },
                }),
            },
        ),
        (
            0x42221012,
            "eormi r1, r2, #18",
            Instruction {
                condition: Condition::Mi,
                op: Data(DataOp {
                    opcode: Eor,
                    operands: DataOperands {
                        dst: Some(R1),
                        op1: Some(R2),
                        op2: DataOperand::make_rot_imm(18, 0),
                    },
                }),
            },
        ),
        (
            0x08417c00,
            "stmdaeq r1, {r10, r11, r12, r13, r14}^",
            Instruction {
                condition: Condition::Eq,
                op: BlockData(Stm(BlockDataOperands {
                    flags: BlockDataFlags::from_bytes([0b0010]),
                    base_reg: R1,
                    reg_list: bitarr![
                        const u16, Lsb0; 0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,0
                    ],
                })),
            },
        ),
        (
            0xe9100002,
            "ldmdb r0, {r1}",
            Instruction {
                condition: Condition::Al,
                op: BlockData(Ldm(BlockDataOperands {
                    flags: BlockDataFlags::from_bytes([0b1000]),
                    base_reg: R0,
                    reg_list: bitarr![
                        const u16, Lsb0; 0, 1
                    ],
                })),
            },
        ),
        (
            0xc9f0fffe,
            "ldmibgt r0!, {r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15}^",
            Instruction {
                condition: Condition::Gt,
                op: BlockData(Ldm(BlockDataOperands {
                    flags: BlockDataFlags::from_bytes([0b1111]),
                    base_reg: R0,
                    reg_list: bitarr![
                        const u16, Lsb0; 0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1
                    ],
                })),
            },
        ),
        (
            0xe80800aa,
            "stmda r8, {r1, r3, r5, r7}",
            Instruction {
                condition: Condition::Al,
                op: BlockData(Stm(BlockDataOperands {
                    flags: BlockDataFlags::from_bytes([0b0000]),
                    base_reg: R8,
                    reg_list: bitarr![
                        const u16, Lsb0; 0, 1, 0, 1, 0, 1, 0, 1
                    ],
                })),
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
        for (instr, _, expect_decoded) in TEST_INSTRUCTIONS.into_iter() {
            let actual_decoded = Decoder::decode(instr);
            assert_eq!(
                expect_decoded, actual_decoded,
                "\nEXPECTED:\n{:#?}\n\nACTUAL:\n{:#?}",
                expect_decoded, actual_decoded
            );
        }
    }

    #[test]
    fn stringify_instructions_test() {
        for (instr, expected_str, _) in TEST_INSTRUCTIONS.into_iter() {
            let actual_str = Decoder::decode(instr).to_string();
            assert_eq!(
                expected_str, actual_str,
                "\nEXPECTED:\n{:#?}\n\nACTUAL:\n{:#?}",
                expected_str, actual_str
            );
        }
    }
}
