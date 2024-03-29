use crate::common::{ByteOrWord, Condition, LoadOrStore, RegisterName, Signedness};
use modular_bitfield::{bitfield, specifiers::*, BitfieldSpecifier, Specifier};
use std::fmt::{Debug, Display};
use RegisterName::*;
use Signedness::*;

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum ArmInstruction {
    BranchAndExchange(branch_and_exchange::Op),
    BlockDataTransfer(block_data_transfer::Op),
    BranchAndBranchWithLink(branch_and_link::Op),
    SoftwareInterrupt(software_interrupt::Op),
    Undefined(undefined_instr::Op),
    SingleDataTransfer(single_data_transfer::Op),
    SingleDataSwap(single_data_swap::Op),
    HalfwordDataTransfer(halfword_data_transfer::Op),
    Multiply(multiply::Op),
    MultiplyLong(multiply_long::Op),
    PsrTransferMrs(psr_transfer_mrs::Op),
    PsrTransferMsr(psr_transfer_msr::Op),
    PsrTransferMsrImm(psr_transfer_msr::OpImm),
    DataProcessing(data_processing::Op), // ALU instructions
}

use ArmInstruction::*;

impl ArmInstruction {
    pub fn decode(instr: u32) -> Self {
        if branch_and_exchange::is_branch_and_exchange(instr) {
            BranchAndExchange(branch_and_exchange::parse(instr))
        } else if block_data_transfer::is_block_data_transfer(instr) {
            BlockDataTransfer(block_data_transfer::parse(instr))
        } else if branch_and_link::is_branch_and_link(instr) {
            BranchAndBranchWithLink(branch_and_link::parse(instr))
        } else if software_interrupt::is_swi(instr) {
            SoftwareInterrupt(software_interrupt::parse(instr))
        } else if undefined_instr::is_undefined(instr) {
            Undefined(undefined_instr::parse(instr))
        } else if single_data_transfer::is_single_data_transfer(instr) {
            SingleDataTransfer(single_data_transfer::parse(instr))
        } else if single_data_swap::is_single_data_swap(instr) {
            SingleDataSwap(single_data_swap::parse(instr))
        } else if multiply::is_multiply(instr) {
            Multiply(multiply::parse(instr))
        } else if multiply_long::is_multiply_long(instr) {
            MultiplyLong(multiply_long::parse(instr))
        } else if halfword_data_transfer::is_halfword_data_transfer(instr) {
            HalfwordDataTransfer(halfword_data_transfer::parse(instr))
        } else if psr_transfer_mrs::is_psr_transfer_mrs(instr) {
            PsrTransferMrs(psr_transfer_mrs::parse(instr))
        } else if psr_transfer_msr::is_psr_transfer_msr(instr) {
            PsrTransferMsr(psr_transfer_msr::parse(instr))
        } else if psr_transfer_msr::is_psr_transfer_msr_imm(instr) {
            PsrTransferMsrImm(psr_transfer_msr::parse_imm(instr))
        } else if data_processing::is_data_processing(instr) {
            DataProcessing(data_processing::parse(instr))
        } else {
            panic!("Unknown instruction format: {:x}", instr);
        }
    }

    pub fn get_operands(
        &self,
    ) -> (
        Option<String>,
        Option<String>,
        Option<String>,
        Option<String>,
    ) {
        use OpCode::*;

        match &self {
            DataProcessing(op) => {
                let operand2 = if op.is_imm_operand() {
                    Some(op.operand2().as_rot_imm().to_string())
                } else {
                    Some(op.operand2().as_shift_reg().to_string())
                };
                match op.op() {
                    Mov | Mvn => (Some(op.dest_reg().to_string()), None, operand2, None),
                    Cmp | Cmn | Teq | Tst => {
                        (None, Some(op.operand1().to_string()), operand2, None)
                    }
                    _ => (
                        Some(op.dest_reg().to_string()),
                        Some(op.operand1().to_string()),
                        operand2,
                        None,
                    ),
                }
            }
            BranchAndExchange(op) => {
                let rn = Some(op.rn().to_string());
                (rn, None, None, None)
            }
            BranchAndBranchWithLink(op) => {
                // Add 8 to match the original offset. Assembler will encode it -8 because
                // the CPU will always be two instructions ahead.
                let offset = Some((op.offset_get() + 8).to_string());
                (offset, None, None, None)
            }
            SoftwareInterrupt(op) => {
                let comment = Some(format!("{:#x}", op.comment()));
                (comment, None, None, None)
            }
            BlockDataTransfer(op) => {
                let write_back = op.write_back();
                let set_psr = op.psr_force_user();
                let base_reg = op.base_reg().to_string();
                let reg_list = op.reg_list();

                let op1 = format!("{}{}", base_reg, if write_back { "!" } else { "" });
                let op2 = format!("{}{}", reg_list, if set_psr { "^" } else { "" });
                (None, Some(op1), Some(op2), None)
            }
            Undefined(_) => (None, None, None, None),
            PsrTransferMrs(op) => {
                let psr = Some(op.src_psr().to_string());
                let rd = Some(op.reg_dest().to_string());
                (rd, None, psr, None)
            }
            PsrTransferMsr(op) => {
                let psr = Some(op.dest_psr().to_string());
                let rm = Some(op.rm().to_string());
                (psr, None, rm, None)
            }
            PsrTransferMsrImm(op) => {
                let psr = Some(op.dest_psr().to_string());
                if op.is_imm_operand() {
                    let imm = op.src_operand().as_rot_imm().to_string();
                    (psr, None, Some(imm), None)
                } else {
                    let reg = op.src_operand().as_reg().to_string();
                    (psr, None, Some(reg), None)
                }
            }
            SingleDataTransfer(op) => {
                use PreOrPostIndexing::*;
                use UpOrDown::*;

                let rd = Some(op.dest_reg().to_string());
                let rn = op.base_reg();
                let wb = if op.write_back() { "!" } else { "" };
                let imm = op.offset().as_imm();
                let shift_reg = op.offset().as_shift_reg();
                let no_shift = !shift_reg.shift_amt_in_reg() && shift_reg.shift_amt() == 0;
                let sign = match (op.up_or_down(), no_shift) {
                    (_, true) => "",
                    (Up, false) => "+",
                    (Down, false) => "-",
                };

                let address = match (op.pre_post_indexing(), !op.is_reg_offset()) {
                    (Pre, true) => {
                        if imm == 0 {
                            format!("[{}]", rn)
                        } else {
                            format!("[{},{}]{}", rn, imm, wb)
                        }
                    }
                    (Pre, false) => {
                        format!("[{},{}{}]{}", rn, sign, shift_reg, wb)
                    }
                    (Post, true) => {
                        format!("[{}],{}", rn, imm)
                    }
                    (Post, false) => {
                        format!("[{}],{}{}", rn, sign, shift_reg)
                    }
                };
                (rd, None, Some(address), None)
            }
            SingleDataSwap(op) => {
                let dst = op.dest_reg().to_string();
                let src = op.src_reg().to_string();
                let rn = Some(format!("[{}]", op.base_reg()));
                (Some(dst), Some(src), rn, None)
            }
            HalfwordDataTransfer(op) => {
                let dst = Some(op.reg_dest().to_string());
                let base_reg = op.base_reg();
                let wb = if op.write_back() { "!" } else { "" };
                let up_down = match op.up_or_down() {
                    UpOrDown::Up => "",
                    UpOrDown::Down => "-",
                };
                let operand2 = match (op.is_imm_offset(), op.pre_post_indexing()) {
                    (true, PreOrPostIndexing::Pre) => {
                        let offset = op.imm_offset();
                        if base_reg == R15 {
                            format!("#{}", offset)
                        } else {
                            format!("[{}, {}]{}", base_reg, offset, wb)
                        }
                    }
                    (true, PreOrPostIndexing::Post) => {
                        let offset = op.imm_offset();
                        format!("[{}],{}", base_reg, offset)
                    }
                    (false, PreOrPostIndexing::Pre) => {
                        let offset_reg = op.reg_offset();
                        format!("[{}, {}{}]{}", base_reg, up_down, offset_reg, wb)
                    }
                    (false, PreOrPostIndexing::Post) => {
                        let offset_reg = op.reg_offset();
                        format!("[{}],{}{}", base_reg, up_down, offset_reg)
                    }
                };
                (dst, None, Some(operand2), None)
            }
            Multiply(op) => {
                use AccumulateType::*;
                let rd = Some(op.reg_dest().to_string());
                let rm = Some(op.rm().to_string());
                let rs = Some(op.rs().to_string());
                let rn = match op.accumulate() {
                    MultiplyOnly => None,
                    MultiplyAndAccumulate => Some(op.rn().to_string()),
                };
                (rd, rm, rs, rn)
            }
            MultiplyLong(op) => {
                let rdlo = Some(op.reg_dest_lo().to_string());
                let rdhi = Some(op.reg_dest_hi().to_string());
                let rm = Some(op.rm().to_string());
                let rs = Some(op.rs().to_string());
                (rdlo, rdhi, rm, rs)
            }
        }
    }

    pub fn op_mnemonic(&self) -> String {
        use halfword_data_transfer::ShType::*;
        use AccumulateType::*;
        use LoadOrStore::*;
        use PreOrPostIndexing::*;
        use UpOrDown::*;

        match self {
            BranchAndExchange(_) => "bx".into(),
            BranchAndBranchWithLink(op) => match op.link() {
                true => "bl".into(),
                false => "b".into(),
            },
            Multiply(op) => match op.accumulate() {
                MultiplyAndAccumulate => "mla".into(),
                MultiplyOnly => "mul".into(),
            },
            MultiplyLong(op) => match (op.signedness(), op.accumulate()) {
                (Unsigned, MultiplyOnly) => "umull".into(),
                (Unsigned, MultiplyAndAccumulate) => "umlal".into(),
                (Signed, MultiplyOnly) => "smull".into(),
                (Signed, MultiplyAndAccumulate) => "smlal".into(),
            },
            DataProcessing(op) => op.op().to_string(),
            SoftwareInterrupt(_) => "swi".into(),
            BlockDataTransfer(op) => match op.load_store() {
                Load => match (op.pre_post_indexing(), op.up_or_down()) {
                    (Pre, Up) => "ldmib",
                    (Post, Up) => "ldmia",
                    (Pre, Down) => "ldmdb",
                    (Post, Down) => "ldmda",
                }
                .into(),
                Store => match (op.pre_post_indexing(), op.up_or_down()) {
                    (Pre, Up) => "stmib",
                    (Post, Up) => "stmia",
                    (Pre, Down) => "stmdb",
                    (Post, Down) => "stmda",
                }
                .into(),
            },
            PsrTransferMrs(_) => "mrs".into(),
            PsrTransferMsr(_) | PsrTransferMsrImm(_) => "msr".into(),
            SingleDataTransfer(op) => match op.load_store() {
                Load => "ldr".into(),
                Store => "str".into(),
            },
            Undefined(_) => "undefined".into(),
            SingleDataSwap(_) => "swp".into(),
            HalfwordDataTransfer(op) => match (op.load_store(), op.sh_type()) {
                (Load, UnsignedHalfword) => "ldrh".into(),
                (Load, SignedHalfword) => "ldrsh".into(),
                (Load, SignedByte) => "ldrsb".into(),
                (Store, UnsignedHalfword) => "strh".into(),
                _ => unreachable!(),
            },
        }
    }

    pub fn condition(&self) -> Condition {
        match self {
            BranchAndExchange(op) => op.condition(),
            BlockDataTransfer(op) => op.condition(),
            BranchAndBranchWithLink(op) => op.condition(),
            SoftwareInterrupt(op) => op.condition(),
            Undefined(op) => op.condition(),
            SingleDataTransfer(op) => op.condition(),
            Multiply(op) => op.condition(),
            MultiplyLong(op) => op.condition(),
            PsrTransferMrs(op) => op.condition(),
            PsrTransferMsr(op) => op.condition(),
            PsrTransferMsrImm(op) => op.condition(),
            DataProcessing(op) => op.condition(),
            SingleDataSwap(op) => op.condition(),
            HalfwordDataTransfer(op) => op.condition(),
        }
    }
}

impl Display for ArmInstruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // Certain instructions can add an extra letter to the mnemonic.
        // E.g. MUL can become MULEQS with condition code EQ and set_condition=true
        let extra_letter = match &self {
            Multiply(op) => {
                if op.set_cond() {
                    "s".into()
                } else {
                    "".into()
                }
            }
            MultiplyLong(op) => {
                if op.set_cond() {
                    "s".into()
                } else {
                    "".into()
                }
            }
            DataProcessing(op) => {
                use data_processing::OpCode::*;
                if op.set_cond() && !matches!(op.op(), Cmp | Cmn | Teq | Tst) {
                    "s".into()
                } else {
                    "".into()
                }
            }
            SingleDataTransfer(op) => {
                format!("{}{}", op.byte_or_word(), op.pre_post_indexing())
            }

            SingleDataSwap(op) => op.byte_or_word().to_string(),
            _ => "".into(),
        };
        let op = self.op_mnemonic();
        let condition = self.condition();
        match self.get_operands() {
            (None, None, None, None) => {
                write!(f, "{}{}{}", op, extra_letter, condition,)
            }
            (None, Some(op1), Some(op2), None) => {
                write!(f, "{}{}{} {}, {}", op, extra_letter, condition, op1, op2)
            }
            (Some(dest), None, None, None) => {
                write!(f, "{}{}{} {}", op, extra_letter, condition, dest)
            }
            (Some(dest), None, Some(op2), None) => {
                write!(f, "{}{}{} {}, {}", op, extra_letter, condition, dest, op2,)
            }
            (Some(dest), Some(op1), Some(op2), None) => {
                write!(
                    f,
                    "{}{}{} {}, {}, {}",
                    op, extra_letter, condition, dest, op1, op2
                )
            }
            (Some(dest), Some(op1), Some(op2), Some(op3)) => {
                write!(
                    f,
                    "{}{}{} {}, {}, {}, {}",
                    op, extra_letter, condition, dest, op1, op2, op3
                )
            }
            _ => unreachable!("Invalid combination of operands"),
        }
    }
}

use self::data_processing::OpCode;

impl From<RegisterName> for u8 {
    fn from(reg: RegisterName) -> Self {
        match reg {
            R0 => 0,
            R1 => 1,
            R2 => 2,
            R3 => 3,
            R4 => 4,
            R5 => 5,
            R6 => 6,
            R7 => 7,
            R8 => 8,
            R9 => 9,
            R10 => 10,
            R11 => 11,
            R12 => 12,
            R13 => 13,
            R14 => 14,
            R15 => 15,
        }
    }
}

impl From<u8> for RegisterName {
    fn from(n: u8) -> Self {
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

#[derive(Clone, Copy, PartialEq, Eq, Debug, BitfieldSpecifier)]
#[bits = 1]
pub enum AccumulateType {
    MultiplyOnly,
    MultiplyAndAccumulate,
}

#[bitfield(bits = 12)]
#[derive(Clone, Copy, PartialEq, Eq, BitfieldSpecifier)]
pub struct Operand12Bit {
    data: B12,
}

impl From<u16> for Operand12Bit {
    fn from(imm: u16) -> Self {
        Self::from_bytes(imm.to_le_bytes())
    }
}

impl From<RotatedImmediate> for Operand12Bit {
    fn from(imm: RotatedImmediate) -> Self {
        Self::from_bytes([imm.val(), imm.rotation()])
    }
}

impl From<ShiftRegister> for Operand12Bit {
    fn from(sr: ShiftRegister) -> Self {
        Self::from_bytes(sr.into_bytes())
    }
}

impl From<RegisterName> for Operand12Bit {
    fn from(rn: RegisterName) -> Self {
        Self::from_bytes([RegisterName::into_bytes(rn).unwrap(), 0])
    }
}

impl Debug for Operand12Bit {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:#b}", self.data())
    }
}

impl Operand12Bit {
    pub fn as_imm(self) -> u16 {
        self.data()
    }

    pub fn as_rot_imm(self) -> RotatedImmediate {
        RotatedImmediate::from_bytes(self.into_bytes())
    }

    pub fn as_shift_reg(self) -> ShiftRegister {
        ShiftRegister::from_bytes(self.into_bytes())
    }

    pub fn as_reg(self) -> RegisterName {
        RegisterName::from(self.into_bytes()[0])
    }
}

#[bitfield(bits = 12)]
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct ShiftRegister {
    pub reg: RegisterName,
    pub shift_amt_in_reg: bool,
    #[bits = 2]
    pub shift_type: ShiftType,
    shift_src: B5,
}

impl ShiftRegister {
    pub fn shift_amt(&self) -> u8 {
        self.shift_src()
    }

    pub fn shift_reg(&self) -> RegisterName {
        (self.shift_src() >> 1).into()
    }

    pub fn with_shift_reg(self, reg: RegisterName) -> Self {
        let reg_num = u8::from(reg) << 1;
        self.with_shift_src(reg_num)
    }

    pub fn with_shift_amt(self, amt: u8) -> Self {
        self.with_shift_src(amt)
    }
}

impl Display for ShiftRegister {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.shift_amt_in_reg() {
            write!(
                f,
                "{}, {} {}",
                self.reg(),
                self.shift_type(),
                self.shift_reg(),
            )
        } else if self.shift_src() != 0 {
            write!(
                f,
                "{}, {} #{}",
                self.reg(),
                self.shift_type(),
                self.shift_amt()
            )
        } else {
            write!(f, "{}", self.reg())
        }
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

impl Display for ShiftType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                ShiftType::LogicalLeft => "lsl",
                ShiftType::LogicalRight => "lsr",
                ShiftType::ArithmeticRight => "asr",
                ShiftType::RotateRight => "ror",
            }
        )
    }
}

#[bitfield(bits = 5)]
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct ShiftAmount {
    amount: B5,
}

#[bitfield(bits = 5)]
#[derive(Clone, Copy, PartialEq, Eq, Debug)]

pub struct ShiftSource {
    data: B5,
}

impl ShiftSource {
    pub fn as_shift_amount(self) -> ShiftAmount {
        ShiftAmount::from_bytes(self.into_bytes())
    }

    pub fn as_reg(self) -> RegisterName {
        RegisterName::from(self.data() as u8)
    }
}

#[bitfield(bits = 12)]
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct RotatedImmediate {
    val: B8,      // To be zero-extended to 32 bits
    rotation: B4, // `val` is right-rotated by twice this amount
}

impl RotatedImmediate {
    /// The calculated value of this immediate, after applying the rotation.
    pub fn value(&self) -> u32 {
        let value = self.val() as u32;
        value.rotate_right(self.rotation() as u32 * 2)
    }

    pub fn new_imm(value: u8, rotate: u8) -> Self {
        Self {
            bytes: [value, rotate],
        }
    }
}

impl Display for RotatedImmediate {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "#{}", self.value())
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug, BitfieldSpecifier)]
#[bits = 1]
pub enum PsrLocation {
    Cpsr,
    Spsr,
}

impl Display for PsrLocation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Cpsr => "CPSR",
                Self::Spsr => "SPSR",
            }
        )
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, BitfieldSpecifier)]
#[bits = 1]
pub enum UpOrDown {
    Down, // Subtract offset from base
    Up,   // Add offset to base
}
#[derive(Clone, Copy, Debug, PartialEq, Eq, BitfieldSpecifier)]
#[bits = 1]
pub enum PreOrPostIndexing {
    Post, // Add offset after transfer
    Pre,  // Add offset before transfer
}

impl Display for PreOrPostIndexing {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use PreOrPostIndexing::*;
        write!(
            f,
            "{}",
            match self {
                Post => "t",
                Pre => "",
            }
        )
    }
}

pub mod data_processing {
    use super::*;

    pub fn is_data_processing(instr: u32) -> bool {
        let data_processing_format = 0b0000_0000_0000_0000_0000_0000_0000_0000;

        let format_mask = 0b0000_1100_0000_0000_0000_0000_0000_0000;

        let extracted_format = instr & format_mask;

        extracted_format == data_processing_format
    }

    pub fn parse(instr: u32) -> Op {
        Op::from_bytes(instr.to_le_bytes())
    }

    #[bitfield(bits = 32)]
    #[derive(Clone, Copy, Debug, PartialEq, Eq)]
    pub struct Op {
        /// `operand2` is either a [ShiftRegister] or [RotatedImmediate]
        pub operand2: Operand12Bit,
        pub dest_reg: RegisterName,
        pub operand1: RegisterName,
        pub set_cond: bool,
        pub op: OpCode,
        pub is_imm_operand: bool,
        #[skip]
        ignored: B2,
        pub condition: Condition,
    }

    #[derive(Clone, Copy, PartialEq, Eq, Debug, BitfieldSpecifier)]
    #[bits = 4]
    pub enum OpCode {
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

    impl Display for OpCode {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            use OpCode::*;
            write!(
                f,
                "{}",
                match self {
                    And => "and",
                    Eor => "eor",
                    Sub => "sub",
                    Rsb => "rsb",
                    Add => "add",
                    Adc => "adc",
                    Sbc => "sbc",
                    Rsc => "rsc",
                    Tst => "tst",
                    Teq => "teq",
                    Cmp => "cmp",
                    Cmn => "cmn",
                    Orr => "orr",
                    Mov => "mov",
                    Bic => "bic",
                    Mvn => "mvn",
                }
            )
        }
    }
}

pub mod multiply {
    use super::*;

    pub fn is_multiply(instr: u32) -> bool {
        let multiply_format = 0b0000_0000_0000_0000_0000_0000_1001_0000;
        let format_mask = 0b0000_1111_1000_0000_0000_0000_1111_0000;
        let extracted_format = instr & format_mask;
        extracted_format == multiply_format
    }

    pub fn parse(instr: u32) -> multiply::Op {
        Op::from_bytes(instr.to_le_bytes())
    }

    #[bitfield(bits = 32)]
    #[derive(Clone, Copy, Debug, Eq)]
    pub struct Op {
        pub rm: RegisterName,
        #[skip]
        ignored: B4, // 1001
        pub rs: RegisterName,
        pub rn: RegisterName,
        pub reg_dest: RegisterName,
        pub set_cond: bool,
        pub accumulate: AccumulateType,
        #[skip]
        ignored: B6, // 000000
        pub condition: Condition,
    }

    impl PartialEq for Op {
        fn eq(&self, other: &Self) -> bool {
            self.rs() == other.rs()
                && self.rn() == other.rn()
                && self.rm() == other.rm()
                && self.rs() == other.rs()
                && self.reg_dest() == other.reg_dest()
                && self.set_cond() == other.set_cond()
                && self.condition() == other.condition()
                && self.accumulate() == other.accumulate()
        }
    }
}

pub mod multiply_long {
    use super::*;

    pub fn is_multiply_long(instr: u32) -> bool {
        let multiply_long_format = 0b0000_0000_1000_0000_0000_0000_1001_0000;
        let format_mask = 0b0000_1111_1000_0000_0000_0000_1111_0000;
        let extracted_format = instr & format_mask;
        extracted_format == multiply_long_format
    }

    pub fn parse(instr: u32) -> Op {
        Op::from_bytes(instr.to_le_bytes())
    }

    #[bitfield(bits = 32)]
    #[derive(Clone, Copy, Debug, Eq)]
    pub struct Op {
        pub rm: RegisterName,
        #[skip]
        ignored: B4, // 1001
        pub rs: RegisterName,
        pub reg_dest_lo: RegisterName,
        pub reg_dest_hi: RegisterName,
        pub set_cond: bool,
        pub accumulate: AccumulateType,
        pub signedness: Signedness,
        #[skip]
        ignored: B5,
        pub condition: Condition,
    }

    impl PartialEq for Op {
        fn eq(&self, other: &Self) -> bool {
            self.rm() == other.rm()
                && self.rs() == other.rs()
                && self.reg_dest_lo() == other.reg_dest_lo()
                && self.reg_dest_hi() == other.reg_dest_hi()
                && self.set_cond() == other.set_cond()
                && self.condition() == other.condition()
                && self.accumulate() == other.accumulate()
                && self.signedness() == other.signedness()
        }
    }
}

pub mod branch_and_exchange {
    use crate::common::InstructionMode;

    use super::*;

    pub fn is_branch_and_exchange(instr: u32) -> bool {
        let branch_and_exchange_format = 0b0000_0001_0010_1111_1111_1111_0001_0000;
        let format_mask = 0b0000_1111_1111_1111_1111_1111_1111_0000;
        let extracted_format = instr & format_mask;
        extracted_format == branch_and_exchange_format
    }

    pub fn parse(instr: u32) -> Op {
        Op::from_bytes(instr.to_le_bytes())
    }

    #[bitfield(bits = 32)]
    #[derive(Clone, Copy, Debug, Eq)]
    pub struct Op {
        pub rn: RegisterName,
        #[skip]
        ignored: B24,
        pub condition: Condition,
    }

    impl PartialEq for Op {
        fn eq(&self, other: &Self) -> bool {
            self.rn() == other.rn() && self.condition() == other.condition()
        }
    }

    impl Op {
        pub fn switch_mode_to(&self) -> InstructionMode {
            use InstructionMode::*;
            if u8::from(self.rn()) & 1 == 1 {
                Thumb
            } else {
                Arm
            }
        }
    }
}

pub mod branch_and_link {
    use super::*;

    pub fn is_branch_and_link(instr: u32) -> bool {
        let branch_format = 0b0000_1010_0000_0000_0000_0000_0000_0000;
        let branch_with_link_format = 0b0000_1011_0000_0000_0000_0000_0000_0000;
        let format_mask = 0b0000_1111_0000_0000_0000_0000_0000_0000;
        let extracted_format = instr & format_mask;
        extracted_format == branch_format || extracted_format == branch_with_link_format
    }

    pub fn parse(instr: u32) -> Op {
        Op::from_bytes(instr.to_le_bytes())
    }

    #[bitfield(bits = 32)]
    #[derive(Clone, Copy, Debug, Eq)]
    pub struct Op {
        offset: B24,
        pub link: bool,
        #[skip]
        ignored: B3,
        pub condition: Condition,
    }

    impl PartialEq for Op {
        fn eq(&self, other: &Self) -> bool {
            self.offset() == other.offset()
                && self.link() == other.link()
                && self.condition() == other.condition()
        }
    }

    impl Op {
        /// Set a 24-bit integer.
        pub fn offset_set(self, n: u32) -> Self {
            self.with_offset(n)
        }

        /// Compute the 32-bit 2's complement sign-extended integer
        /// from the stored 24 bits. Does NOT subtract 2 instructions (= 8 bytes).
        pub fn offset_get(&self) -> i32 {
            let n = self.offset();
            let signed = ((n >> 23) & 1) == 1;
            let shifted = (n << 2) as i32;

            if signed {
                shifted | (0b111111 << 26)
            } else {
                shifted as i32
            }
        }
    }
}

pub mod software_interrupt {
    use super::*;

    pub fn is_swi(instr: u32) -> bool {
        let software_interrupt_format = 0b0000_1111_0000_0000_0000_0000_0000_0000;
        let format_mask = 0b0000_1111_0000_0000_0000_0000_0000_0000;
        let extracted_format = instr & format_mask;
        extracted_format == software_interrupt_format
    }

    pub fn parse(instr: u32) -> Op {
        Op::from_bytes(instr.to_le_bytes())
    }

    #[bitfield(bits = 32)]
    #[derive(Clone, Copy, Debug, Eq)]
    pub struct Op {
        pub comment: B24,
        #[skip]
        ignored: B4,
        pub condition: Condition,
    }

    impl PartialEq for Op {
        fn eq(&self, other: &Self) -> bool {
            self.comment() == other.comment() && self.condition() == other.condition()
        }
    }
}

pub mod block_data_transfer {
    use bitvec::{order::Lsb0, view::BitView};

    use super::*;

    pub fn is_block_data_transfer(instr: u32) -> bool {
        let block_data_transfer_format = 0b0000_1000_0000_0000_0000_0000_0000_0000;
        let format_mask = 0b0000_1110_0000_0000_0000_0000_0000_0000;
        let extracted_format = instr & format_mask;
        extracted_format == block_data_transfer_format
    }

    pub fn parse(instr: u32) -> Op {
        Op::from_bytes(instr.to_le_bytes())
    }

    #[derive(Clone, Copy, Debug, PartialEq, Eq)]
    pub struct RegisterList(u16);

    impl RegisterList {
        pub fn new(list: u16) -> Self {
            Self(list)
        }

        pub fn to_vec(self) -> Vec<RegisterName> {
            self.0
                .view_bits()
                .iter()
                .enumerate()
                .filter_map(
                    |(reg, reg_bit): (usize, bitvec::ptr::BitRef<'_, _, _, Lsb0>)| {
                        if *reg_bit {
                            Some(RegisterName::from(reg as u8))
                        } else {
                            None
                        }
                    },
                )
                .collect()
        }
    }

    impl Display for RegisterList {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(
                f,
                "{{{}}}",
                self.to_vec()
                    .into_iter()
                    .map(|r| r.to_string())
                    .collect::<Vec<String>>()
                    .join(", ")
            )
        }
    }

    #[bitfield(bits = 32)]
    #[derive(Clone, Copy, Debug, Eq)]
    pub struct Op {
        pub regs: u16,
        pub base_reg: RegisterName,
        pub load_store: LoadOrStore,
        pub write_back: bool,
        pub psr_force_user: bool,
        pub up_or_down: UpOrDown,
        pub pre_post_indexing: PreOrPostIndexing,
        #[skip]
        ignored: B3,
        pub condition: Condition,
    }

    impl Op {
        pub fn reg_list(&self) -> RegisterList {
            let regs = self.regs();
            RegisterList(regs)
        }
    }

    impl PartialEq for Op {
        fn eq(&self, other: &Self) -> bool {
            self.regs() == other.regs()
                && self.base_reg() == other.base_reg()
                && self.load_store() == other.load_store()
                && self.write_back() == other.write_back()
                && self.psr_force_user() == other.psr_force_user()
                && self.up_or_down() == other.up_or_down()
                && self.pre_post_indexing() == other.pre_post_indexing()
                && self.condition() == other.condition()
        }
    }
}

pub mod undefined_instr {
    use super::*;

    pub fn is_undefined(instr: u32) -> bool {
        let undefined_format = 0b0000_0110_0000_0000_0000_0000_0001_0000;
        let format_mask = 0b0000_1110_0000_0000_0000_0000_0001_0000;
        let extracted_format = instr & format_mask;
        extracted_format == undefined_format
    }

    pub fn parse(instr: u32) -> Op {
        Op::from_bytes(instr.to_le_bytes())
    }

    #[bitfield(bits = 32)]
    #[derive(Clone, Copy, Debug, PartialEq, Eq)]
    pub struct Op {
        #[skip]
        ignored: B28,
        pub condition: Condition,
    }
}

pub mod psr_transfer_mrs {
    use super::*;

    pub fn is_psr_transfer_mrs(instr: u32) -> bool {
        let mrs_format = 0b0000_0001_0000_1111_0000_0000_0000_0000;
        let format_mask = 0b0000_1111_1011_1111_0000_0000_0000_0000;
        let extracted_format = instr & format_mask;
        extracted_format == mrs_format
    }

    pub fn parse(instr: u32) -> Op {
        Op::from_bytes(instr.to_le_bytes())
    }

    #[bitfield(bits = 32)]
    #[derive(Clone, Copy, Debug, Eq)]
    pub struct Op {
        #[skip]
        ignored: B12,
        pub reg_dest: RegisterName,
        #[skip]
        ignored: B6,
        pub src_psr: PsrLocation,
        #[skip]
        ignored: B5,
        pub condition: Condition,
    }

    impl PartialEq for Op {
        fn eq(&self, other: &Self) -> bool {
            self.reg_dest() == other.reg_dest()
                && self.src_psr() == other.src_psr()
                && self.condition() == other.condition()
        }
    }
}

pub mod psr_transfer_msr {
    use super::*;

    pub fn is_psr_transfer_msr(instr: u32) -> bool {
        let msr_format = 0b0000_0001_0010_1001_1111_0000_0000_0000;
        let format_mask = 0b0000_1101_1011_1111_1111_0000_0000_0000;
        let extracted_format = instr & format_mask;
        extracted_format == msr_format
    }

    pub fn is_psr_transfer_msr_imm(instr: u32) -> bool {
        let msr_flag_format = 0b0000_0001_0010_1000_1111_0000_0000_0000;
        let format_mask = 0b0000_1101_1011_1111_1111_0000_0000_0000;
        let extracted_format = instr & format_mask;
        extracted_format == msr_flag_format
    }

    pub fn parse(instr: u32) -> Op {
        Op::from_bytes(instr.to_le_bytes())
    }

    pub fn parse_imm(instr: u32) -> OpImm {
        OpImm::from_bytes(instr.to_le_bytes())
    }

    #[bitfield(bits = 32)]
    #[derive(Clone, Copy, Debug, PartialEq, Eq)]
    pub struct Op {
        pub rm: RegisterName,
        #[skip]
        ignored: B18,
        pub dest_psr: PsrLocation,
        #[skip]
        ignored: B5,
        pub condition: Condition,
    }

    #[bitfield(bits = 32)]
    #[derive(Clone, Copy, Debug, Eq)]
    pub struct OpImm {
        /// `src_operand` is either a [RegisterName] or [RotatedImmediate]
        pub src_operand: Operand12Bit,
        #[skip]
        ignored: B10,
        pub dest_psr: PsrLocation,
        #[skip]
        ignored: B2,
        pub is_imm_operand: bool,
        #[skip]
        ignored: B2,
        pub condition: Condition,
    }

    impl PartialEq for OpImm {
        fn eq(&self, other: &Self) -> bool {
            self.src_operand() == other.src_operand()
                && self.dest_psr() == other.dest_psr()
                && self.is_imm_operand() == other.is_imm_operand()
                && self.condition() == other.condition()
        }
    }
}

pub mod single_data_transfer {
    use crate::common::ByteOrWord;

    use super::*;

    pub fn is_single_data_transfer(instr: u32) -> bool {
        let single_data_transfer_format = 0b0000_0100_0000_0000_0000_0000_0000_0000;
        let format_mask = 0b0000_1100_0000_0000_0000_0000_0000_0000;
        let extracted_format = instr & format_mask;
        extracted_format == single_data_transfer_format
    }

    pub fn parse(instr: u32) -> Op {
        Op::from_bytes(instr.to_le_bytes())
    }

    #[bitfield(bits = 32)]
    #[derive(Clone, Copy, Debug, Eq)]
    pub struct Op {
        /// `offset` is either a [ShiftRegister] or [u16] (12-bit immediate)
        pub offset: Operand12Bit,
        pub dest_reg: RegisterName,
        pub base_reg: RegisterName,
        pub load_store: LoadOrStore,
        pub write_back: bool,
        pub byte_or_word: ByteOrWord,
        pub up_or_down: UpOrDown,
        pub pre_post_indexing: PreOrPostIndexing,
        pub is_reg_offset: bool,
        #[skip]
        ignored: B2,
        pub condition: Condition,
    }

    impl PartialEq for Op {
        fn eq(&self, other: &Self) -> bool {
            self.offset() == other.offset()
                && self.dest_reg() == other.dest_reg()
                && self.base_reg() == other.base_reg()
                && self.load_store() == other.load_store()
                && self.write_back() == other.write_back()
                && self.byte_or_word() == other.byte_or_word()
                && self.up_or_down() == other.up_or_down()
                && self.pre_post_indexing() == other.pre_post_indexing()
                && self.is_reg_offset() == other.is_reg_offset()
                && self.condition() == other.condition()
        }
    }
}

pub mod single_data_swap {
    use super::*;

    pub fn is_single_data_swap(instr: u32) -> bool {
        let single_data_swap_format = 0b0000_0001_0000_0000_0000_0000_1001_0000;
        let format_mask = 0b0000_1111_1000_0000_0000_1111_1111_0000;
        let extracted_format = instr & format_mask;
        extracted_format == single_data_swap_format
    }

    pub fn parse(instr: u32) -> Op {
        Op::from_bytes(instr.to_le_bytes())
    }

    #[bitfield(bits = 32)]
    #[derive(Clone, Copy, Debug, Eq)]
    pub struct Op {
        /// `offset` is either a [ShiftRegister] or [u16] (12-bit immediate)
        pub src_reg: RegisterName,
        #[skip]
        ignored: B8,
        pub dest_reg: RegisterName,
        pub base_reg: RegisterName,
        #[skip]
        ignored: B2,
        pub byte_or_word: ByteOrWord,
        #[skip]
        ignored: B5,
        pub condition: Condition,
    }

    impl PartialEq for Op {
        fn eq(&self, other: &Self) -> bool {
            self.src_reg() == other.src_reg()
                && self.dest_reg() == other.dest_reg()
                && self.base_reg() == other.base_reg()
                && self.byte_or_word() == other.byte_or_word()
                && self.condition() == other.condition()
        }
    }
}

pub mod halfword_data_transfer {
    use super::*;

    pub fn is_halfword_data_transfer(instr: u32) -> bool {
        let halfword_data_transfer_register_format = 0b0000_0000_0000_0000_0000_0000_1001_0000;
        let format_mask = 0b0000_1110_0000_0000_0000_1111_1001_0000;
        let extracted_format = instr & format_mask;
        extracted_format == halfword_data_transfer_register_format
    }

    pub fn parse(instr: u32) -> Op {
        Op::from_bytes(instr.to_le_bytes())
    }

    #[derive(Clone, Copy, PartialEq, Eq, Debug, BitfieldSpecifier)]
    #[bits = 2]
    pub enum ShType {
        Swp,
        UnsignedHalfword,
        SignedByte,
        SignedHalfword,
    }

    #[bitfield(bits = 32)]
    #[derive(Clone, Copy, Debug, Eq)]
    pub struct Op {
        /// Is [RegisterName] when `is_imm` is 0, otherwise an immediate formed by OR'ing with `offset_hi`
        offset_lo_or_reg: B4,
        #[skip]
        ignored: B1,
        pub sh_type: ShType,
        #[skip]
        ignored: B1,
        offset_hi: B4,
        pub reg_dest: RegisterName,
        pub base_reg: RegisterName,
        pub load_store: LoadOrStore,
        pub write_back: bool,
        pub is_imm_offset: bool,
        pub up_or_down: UpOrDown,
        pub pre_post_indexing: PreOrPostIndexing,
        #[skip]
        ignored: B3,
        pub condition: Condition,
    }

    impl Op {
        pub fn imm_offset(&self) -> u8 {
            if !self.is_imm_offset() {
                panic!("This Op is not an immediate")
            }
            (self.offset_hi() << 4) | self.offset_lo_or_reg()
        }

        pub fn with_imm_offset(self, offset: u8) -> Self {
            if !self.is_imm_offset() {
                panic!("This Op is not an immediate")
            }
            let first = self.with_offset_hi(offset >> 4);
            first.with_offset_lo_or_reg(offset & 0b1111)
        }

        pub fn reg_offset(&self) -> RegisterName {
            if self.is_imm_offset() {
                panic!("This Op does not have a register offset")
            }
            self.offset_lo_or_reg().into()
        }

        pub fn with_reg_offset(self, reg: RegisterName) -> Self {
            if self.is_imm_offset() {
                panic!("This Op does not have a register offset")
            }
            self.with_offset_lo_or_reg(reg.into())
        }
    }

    impl PartialEq for Op {
        fn eq(&self, other: &Self) -> bool {
            self.offset_lo_or_reg() == other.offset_lo_or_reg()
                && self.offset_hi() == other.offset_hi()
                && self.sh_type() == other.sh_type()
                && self.reg_dest() == other.reg_dest()
                && self.base_reg() == other.base_reg()
                && self.load_store() == other.load_store()
                && self.write_back() == other.write_back()
                && self.up_or_down() == other.up_or_down()
                && self.pre_post_indexing() == other.pre_post_indexing()
                && self.condition() == other.condition()
                && self.is_imm_offset() == other.is_imm_offset()
        }
    }
}

#[cfg(test)]
mod tests {
    use super::halfword_data_transfer::ShType::*;
    use super::AccumulateType::*;
    use super::ByteOrWord;
    use super::Condition::*;
    use super::LoadOrStore::*;
    use super::OpCode::*;
    use super::ShiftType::*;
    use super::UpOrDown::*;
    use super::*;
    use lazy_static::lazy_static;

    lazy_static! {
    // Instruction hex, assembly string, expected decoded instruction
    static ref TEST_INSTRUCTIONS: [(u32, &'static str, ArmInstruction); 37] = [
        (
            0xe2833001,
            "add r3, r3, #1",
            ArmInstruction::DataProcessing(
                data_processing::Op::new()
                    .with_condition(Al)
                    .with_op(Add)
                    .with_dest_reg(R3)
                    .with_operand1(R3)
                    .with_operand2(RotatedImmediate::new_imm(1, 0).into())
                    .with_is_imm_operand(true)
            )
        ),
        (
            0xe0823003,
            "add r3, r2, r3",
            ArmInstruction::DataProcessing(data_processing::Op::new()
                .with_condition(Al)
                .with_op(Add)
                .with_dest_reg(R3)
                .with_operand1(R2)
                .with_operand2(ShiftRegister::new().with_reg(R3).into())
            )
        ),
        (
            0xe24dd01c,
            "sub r13, r13, #28",
            ArmInstruction::DataProcessing(data_processing::Op::new()
                .with_condition(Al)
                .with_op(Sub)
                .with_dest_reg(R13)
                .with_operand1(R13)
                .with_is_imm_operand(true)
                .with_operand2(RotatedImmediate::new_imm(28, 0).into())
            )
        ),
        (
            0xe3a09a01,
            "mov r9, #4096",
            ArmInstruction::DataProcessing(data_processing::Op::new()
                .with_condition(Al)
                .with_op(Mov)
                .with_dest_reg(R9)
                .with_is_imm_operand(true)
                .with_operand2(RotatedImmediate::new_imm(1, 0b1010).into())
            )
        ),
        (
            0xe2a5400a,
            "adc r4, r5, #10",
            ArmInstruction::DataProcessing(data_processing::Op::new()
                .with_condition(Al)
                .with_op(Adc)
                .with_dest_reg(R4)
                .with_operand1(R5)
                .with_is_imm_operand(true)
                .with_operand2(RotatedImmediate::new_imm(10, 0).into())
            )
        ),
        (
            0xe0076008,
            "and r6, r7, r8",
            ArmInstruction::DataProcessing(data_processing::Op::new()
                .with_condition(Al)
                .with_op(And)
                .with_dest_reg(R6)
                .with_operand1(R7)
                .with_operand2(ShiftRegister::new().with_reg(R8).into())

            )
        ),
        (
            0xeafffffe,
            "b 0",
            ArmInstruction::BranchAndBranchWithLink(branch_and_link::Op::new()
                .with_condition(Al)
                .offset_set(0xfffffe)
            )
        ),
        (
            0xebfffffe,
            "bl 0",
            ArmInstruction::BranchAndBranchWithLink(branch_and_link::Op::new()
                .with_condition(Al)
                .with_link(true)
                .offset_set(0xfffffe)

            )
        ),
        (
            0xe3c920aa,
            "bic r2, r9, #170",
            ArmInstruction::DataProcessing(data_processing::Op::new()
                .with_condition(Al)
                .with_op(Bic)
                .with_operand1(R9)
                .with_is_imm_operand(true)
                .with_operand2(RotatedImmediate::new_imm(170, 0).into())
                .with_dest_reg(R2)
            )
        ),
        (
            0x112fff12,
            "bxne r2",
            ArmInstruction::BranchAndExchange(branch_and_exchange::Op::new()
                .with_condition(Ne)
                .with_rn(R2)
            )
        ),
        (
            0x03720010,
            "cmneq r2, #16",
            ArmInstruction::DataProcessing(data_processing::Op::new()
                .with_condition(Eq)
                .with_set_cond(true)
                .with_op(Cmn)
                .with_operand1(R2)
                .with_is_imm_operand(true)
                .with_operand2(RotatedImmediate::new_imm(16, 0).into())
            )
        ),
        (
            0x93540000,
            "cmpls r4, #0",
            ArmInstruction::DataProcessing(data_processing::Op::new()
                .with_condition(Ls)
                .with_op(Cmp)
                .with_operand1(R4)
                .with_is_imm_operand(true)
                .with_operand2(RotatedImmediate::new_imm(0, 0).into())
                .with_set_cond(true)
            )
        ),
        (
            0x42221012,
            "eormi r1, r2, #18",
            ArmInstruction::DataProcessing(data_processing::Op::new()
                .with_condition(Mi)
                .with_op(Eor)
                .with_dest_reg(R1)
                .with_operand1(R2)
                .with_is_imm_operand(true)
                .with_operand2(RotatedImmediate::new_imm(18, 0).into())
            )
        ),
        (
            0x08417c00,
            "stmdaeq r1, {r10, r11, r12, r13, r14}^",
            ArmInstruction::BlockDataTransfer(block_data_transfer::Op::new()
                .with_condition(Eq)
                .with_load_store(Store)
                .with_base_reg(R1)
                .with_psr_force_user(true)
                .with_up_or_down(UpOrDown::Down)
                .with_pre_post_indexing(PreOrPostIndexing::Post)
                .with_regs(0b0111_1100_0000_0000)
            )
        ),
        (
            0xe9100002,
            "ldmdb r0, {r1}",
            ArmInstruction::BlockDataTransfer(block_data_transfer::Op::new()
                .with_condition(Al)
                .with_load_store(Load)
                .with_base_reg(R0)
                .with_up_or_down(UpOrDown::Down)
                .with_pre_post_indexing(PreOrPostIndexing::Pre)
                .with_regs(0b0000_0000_0000_0010)
            )
        ),
        (
            0xc9f0fffe,
            "ldmibgt r0!, {r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15}^",
            ArmInstruction::BlockDataTransfer(block_data_transfer::Op::new()
                .with_condition(Gt)
                .with_load_store(Load)
                .with_base_reg(R0)
                .with_up_or_down(UpOrDown::Up)
                .with_write_back(true)
                .with_psr_force_user(true)
                .with_pre_post_indexing(PreOrPostIndexing::Pre)
                .with_regs(0b1111_1111_1111_1110)
            )
        ),
        (
            0xe80800aa,
            "stmda r8, {r1, r3, r5, r7}",
            ArmInstruction::BlockDataTransfer(block_data_transfer::Op::new()
                .with_condition(Al)
                .with_load_store(Store)
                .with_base_reg(R8)
                .with_up_or_down(UpOrDown::Down)
                .with_pre_post_indexing(PreOrPostIndexing::Post)
                .with_regs(0b0000_0000_1010_1010)
            )
        ),
        (
            0x00314392,
            "mlaseq r1, r2, r3, r4",
            ArmInstruction::Multiply(multiply::Op::new()
                .with_condition(Eq)
                .with_accumulate(MultiplyAndAccumulate)
                .with_reg_dest(R1)
                .with_rm(R2)
                .with_rs(R3)
                .with_rn(R4)
                .with_set_cond(true)
            )
        ),
        (
            0xe3100001,
            "tst r0, #1",
            ArmInstruction::DataProcessing(data_processing::Op::new()
                .with_condition(Al)
                .with_op(Tst)
                .with_operand1(R0)
                .with_is_imm_operand(true)
                .with_operand2(RotatedImmediate::new_imm(1, 0).into())
                .with_set_cond(true)
            )
        ),
        (
            0xe3300b01,
            "teq r0, #1024",
            ArmInstruction::DataProcessing(data_processing::Op::new()
                .with_condition(Al)
                .with_op(Teq)
                .with_operand1(R0)
                .with_is_imm_operand(true)
                .with_operand2(RotatedImmediate::new_imm(1, 11).into())
                .with_set_cond(true)
            )
        ),
        (
            0x1f000000,
            "swine 0x0",
            ArmInstruction::SoftwareInterrupt(software_interrupt::Op::new()
                .with_condition(Ne)
                .with_comment(0x0)
            )
        ),
        (
            0xe0010392,
            "mul r1, r2, r3",
            ArmInstruction::Multiply(multiply::Op::new()
                .with_condition(Al)
                .with_accumulate(MultiplyOnly)
                .with_reg_dest(R1)
                .with_rm(R2)
                .with_rs(R3)
                .with_set_cond(false)
            )
        ),
        (
            0x01f0c10b,
            "mvnseq r12, r11, lsl #2",
            ArmInstruction::DataProcessing(data_processing::Op::new()
                .with_condition(Eq)
                .with_op(Mvn)
                .with_dest_reg(R12)
                .with_operand2(ShiftRegister::new()
                    .with_reg(R11)
                    .with_shift_amt_in_reg(false)
                    .with_shift_type(LogicalLeft)
                    .with_shift_amt(2)
                    .into()
                )
                .with_set_cond(true)
            )
        ),
        (
            0x53811001,
            "orrpl r1, r1, #1",
            ArmInstruction::DataProcessing(data_processing::Op::new()
                .with_condition(Pl)
                .with_op(Orr)
                .with_dest_reg(R1)
                .with_operand1(R1)
                .with_is_imm_operand(true)
                .with_operand2(RotatedImmediate::new_imm(1, 0).into())
                .with_set_cond(false)
            )
        ),
        (
            0xe2754000,
            "rsbs r4, r5, #0",
            ArmInstruction::DataProcessing(data_processing::Op::new()
                .with_condition(Al)
                .with_op(Rsb)
                .with_dest_reg(R4)
                .with_operand1(R5)
                .with_is_imm_operand(true)
                .with_operand2(RotatedImmediate::new_imm(0, 0).into())
                .with_set_cond(true)
            )
        ),
        (
            0xe0e21233,
            "rsc r1, r2, r3, lsr r2",
            ArmInstruction::DataProcessing(data_processing::Op::new()
                .with_condition(Al)
                .with_op(Rsc)
                .with_dest_reg(R1)
                .with_operand1(R2)
                .with_operand2(ShiftRegister::new()
                    .with_reg(R3)
                    .with_shift_amt_in_reg(true)
                    .with_shift_type(LogicalRight)
                    .with_shift_reg(R2)
                    .into()
                )
                .with_set_cond(false)
            )
        ),
        (
            0x10d33003,
            "sbcsne r3, r3, r3",
            ArmInstruction::DataProcessing(data_processing::Op::new()
                .with_condition(Ne)
                .with_op(Sbc)
                .with_dest_reg(R3)
                .with_operand1(R3)
                .with_operand2(ShiftRegister::new()
                    .with_reg(R3)
                    .into()
                )
                .with_set_cond(true)
            )
        ),
        (
            0x46610102,
            "strbtmi r0, [r1],-r2, lsl #2",
            ArmInstruction::SingleDataTransfer(single_data_transfer::Op::new()
                .with_condition(Mi)
                .with_load_store(Store)
                .with_up_or_down(Down)
                .with_write_back(true)
                .with_pre_post_indexing(PreOrPostIndexing::Post)
                .with_byte_or_word(ByteOrWord::Byte)
                .with_dest_reg(R0)
                .with_base_reg(R1)
                .with_is_reg_offset(true)
                .with_offset(ShiftRegister::new()
                    .with_reg(R2)
                    .with_shift_amt(2)
                    .with_shift_type(LogicalLeft)
                    .into()
                )
            )
        ),
        (
            0xe1442093,
            "swpb r2, r3, [r4]",
            ArmInstruction::SingleDataSwap(single_data_swap::Op::new()
                .with_condition(Al)
                .with_byte_or_word(ByteOrWord::Byte)
                .with_dest_reg(R2)
                .with_src_reg(R3)
                .with_base_reg(R4)
            )
        ),
        (
            0xe0841392,
            "umull r1, r4, r2, r3",
            ArmInstruction::MultiplyLong(multiply_long::Op::new()
                .with_condition(Al)
                .with_accumulate(MultiplyOnly)
                .with_reg_dest_lo(R1)
                .with_reg_dest_hi(R4)
                .with_rm(R2)
                .with_rs(R3)
                .with_signedness(Unsigned)
                .with_set_cond(false)
            )
        ),
        (
            0xe0f51392,
            "smlals r1, r5, r2, r3",
            ArmInstruction::MultiplyLong(multiply_long::Op::new()
                .with_condition(Al)
                .with_accumulate(MultiplyAndAccumulate)
                .with_reg_dest_lo(R1)
                .with_reg_dest_hi(R5)
                .with_rm(R2)
                .with_rs(R3)
                .with_signedness(Signed)
                .with_set_cond(true)
            )
        ),
        (
            0xe1d210b0,
            "ldrh r1, [r2, 0]",
            ArmInstruction::HalfwordDataTransfer(halfword_data_transfer::Op::new()
                .with_condition(Al)
                .with_load_store(Load)
                .with_up_or_down(Up)
                .with_write_back(false)
                .with_pre_post_indexing(PreOrPostIndexing::Pre)
                .with_sh_type(UnsignedHalfword)
                .with_reg_dest(R1)
                .with_base_reg(R2)
                .with_is_imm_offset(true)
                .with_imm_offset(0)
            )
        ),
        (
            0xe10230b4,
            "strh r3, [r2, -r4]",
            ArmInstruction::HalfwordDataTransfer(halfword_data_transfer::Op::new()
                .with_condition(Al)
                .with_load_store(Store)
                .with_up_or_down(Down)
                .with_write_back(false)
                .with_pre_post_indexing(PreOrPostIndexing::Pre)
                .with_sh_type(UnsignedHalfword)
                .with_reg_dest(R3)
                .with_base_reg(R2)
                .with_is_imm_offset(false)
                .with_reg_offset(R4)
            )
        ),
        (
            0x814f1000,
            "mrshi r1, SPSR",
            ArmInstruction::PsrTransferMrs(psr_transfer_mrs::Op::new()
                .with_condition(Hi)
                .with_reg_dest(R1)
                .with_src_psr(PsrLocation::Spsr)
            )
        ),
        (
            0xe128f001,
            "msr CPSR, r1",
            ArmInstruction::PsrTransferMsrImm(psr_transfer_msr::OpImm::new()
                .with_condition(Al)
                .with_dest_psr(PsrLocation::Cpsr)
                .with_is_imm_operand(false)
                .with_src_operand(R1.into())
            )
        ),
        (
            0xe368f201,
            "msr SPSR, #268435456",
            ArmInstruction::PsrTransferMsrImm(psr_transfer_msr::OpImm::new()
                .with_condition(Al)
                .with_dest_psr(PsrLocation::Spsr)
                .with_is_imm_operand(true)
                .with_src_operand(RotatedImmediate::new_imm(1, 2).into())
            )
        ),
        (
            0xEA00002E,
            "b 192",
            ArmInstruction::BranchAndBranchWithLink(branch_and_link::Op::new()
                .with_condition(Al)
                .with_link(false)
                .offset_set(0x2E)
            )
        )
    ];
    }

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
        assert_eq!(RotatedImmediate::new_imm(1, 0b1010).value(), 4096)
    }

    #[test]
    fn decode_instructions_test() {
        for (instr, _, expect_decoded) in TEST_INSTRUCTIONS.into_iter() {
            let actual_decoded = ArmInstruction::decode(instr);
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
            let actual_str = ArmInstruction::decode(instr).to_string();
            assert_eq!(
                expected_str, actual_str,
                "\nEXPECTED:\n{:#?}\n\nACTUAL:\n{:#?}",
                expected_str, actual_str
            );
        }
    }
}
