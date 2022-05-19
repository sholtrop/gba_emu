use modular_bitfield::{bitfield, specifiers::*, BitfieldSpecifier, Specifier};
use std::fmt::{Debug, Display};

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Instruction {
    BranchAndExchange(branch_and_exchange::Op),
    BlockDataTransfer(block_data_transfer::Op),
    BranchAndBranchWithLink(branch_and_link::Op),
    SoftwareInterrupt(software_interrupt::Op),
    Undefined(undefined_instr::Op),
    SingleDataTransfer(single_data_transfer::Op),
    SingleDataSwap(u32),
    HalfwordDataTransferRegister(u32),
    HalfwordDataTransferImmediate(u32),
    Multiply(multiply::Op),
    MultiplyLong(multiply_long::Op),
    PsrTransferMrs(psr_transfer_mrs::Op),
    PsrTransferMsr(psr_transfer_msr::Op),
    PsrTransferMsrImm(psr_transfer_msr::OpImm),
    DataProcessing(data_processing::Op), // ALU instructions
}

use Instruction::*;

impl Instruction {
    pub fn decode(instr: u32) -> Self {
        //x if Decoder::is_branch_and_branch_exchange(instr) {
        //     OpFormat::BranchAndBranchExchange
        //x } else if Decoder::is_block_data_transfer(instr) {
        //     OpFormat::BlockDataTransfer
        //x } else if Decoder::is_branch_and_branch_with_link(instr) {
        //     OpFormat::BranchAndBranchWithLink
        //x } else if Decoder::is_software_interrupt(instr) {
        //     OpFormat::SoftwareInterrupt
        //x } else if Decoder::is_undefined(instr) {
        //     OpFormat::Undefined
        //x } else if Decoder::is_single_data_transfer(instr) {
        //     OpFormat::SingleDataTransfer
        // x} else if Decoder::is_single_data_swap(instr) {
        //     OpFormat::SingleDataSwap
        // x} else if Decoder::is_halfword_data_transfer_register(instr) {
        //     OpFormat::HalfwordDataTransferRegister
        // x} else if Decoder::is_halfword_data_transfer_immediate(instr) {
        //     OpFormat::HalfwordDataTransferImmediate
        // x} else if Decoder::is_multiply(instr) {
        //     OpFormat::Multiply
        // x} else if Decoder::is_psr_transfer_mrs(instr) {
        //     OpFormat::PsrTransferMrs
        //x } else if Decoder::is_psr_transfer_msr(instr) {
        //     OpFormat::PsrTransferMsr
        // x} else if Decoder::is_data_processing(instr) {

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
            todo!("SingleDataSwap")
        } else if halfword_data_reg::is_halfword_data_transfer_reg(instr) {
            todo!("HalfwordDataTransferRegister")
        } else if halfword_data_imm::is_halfword_data_transfer_imm(instr) {
            todo!("HalfwordDataTransferImmediate")
        } else if multiply::is_multiply(instr) {
            Multiply(multiply::parse(instr))
        } else if psr_transfer_mrs::is_psr_transfer_mrs(instr) {
            PsrTransferMrs(psr_transfer_mrs::parse(instr))
        } else if psr_transfer_msr::is_psr_transfer_msr(instr) {
            PsrTransferMsr(psr_transfer_msr::parse(instr))
        } else if psr_transfer_msr::is_psr_transfer_msr_imm(instr) {
            PsrTransferMsrImm(psr_transfer_msr::parse_imm(instr))
        } else if data_processing::is_data_processing(instr) {
            DataProcessing(data_processing::parse(instr))
        } else {
            todo!("other instructions")
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
                let offset = Some(op.offset_get().to_string());
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
                (psr, rd, None, None)
            }
            PsrTransferMsr(op) => {
                let psr = Some(op.dest_psr().to_string());
                let rm = Some(op.rm().to_string());
                (psr, rm, None, None)
            }
            PsrTransferMsrImm(op) => {
                let psr = Some(op.dest_psr().to_string());
                if op.is_imm_operand() {
                    let imm = op.src_operand().as_rot_imm().to_string();
                    (psr, Some(imm), None, None)
                } else {
                    let reg = op.src_operand().as_reg().to_string();
                    (psr, Some(reg), None, None)
                }
            }
            SingleDataTransfer(op) => {
                use PreOrPostIndexing::*;
                use UpOrDown::*;

                let rd = Some(op.reg_dest().to_string());
                let rn = op.base_reg();
                let wb = if op.write_back() { "!" } else { "" };
                let imm = op.offset().as_imm();
                let shift_reg = op.offset().as_shift_reg();
                let no_shift = !shift_reg.shift_amt_in_reg() && shift_reg.shift_amt() == 0;
                // let shift = if ps_reg.shift() > 0 {
                //     format!(",{}", ps_reg.shift())
                // } else {
                //     "".into()
                // };
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
            SingleDataSwap(_) => todo!(),
            HalfwordDataTransferRegister(_) => todo!(),
            HalfwordDataTransferImmediate(_) => todo!(),
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
        use multiply_long::Signedness::*;
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
            SingleDataSwap(_) => todo!(),
            HalfwordDataTransferImmediate(_) => todo!(),
            HalfwordDataTransferRegister(_) => todo!(),
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
            SingleDataSwap(_) => todo!(),
            HalfwordDataTransferRegister(_) => todo!(),
            HalfwordDataTransferImmediate(_) => todo!(),
        }
    }
}

impl Display for Instruction {
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

            // Swp { size, .. } => size.to_string(),
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
            _ => unreachable!("A previous Operand was `None`"),
        }
    }
}

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

#[derive(Clone, Copy, PartialEq, Eq, Debug, BitfieldSpecifier)]
#[bits = 4]
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
        let bytes = [0, <RegisterName as Specifier>::into_bytes(rn).unwrap()];
        Self::from_bytes(bytes)
    }
}

// impl From<PureShiftRegister> for Operand12Bit {
//     fn from(pure_sr: PureShiftRegister) -> Self {
//         Self::from_bytes(pure_sr.into_bytes())
//     }
// }

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
        RegisterName::from(self.into_bytes()[1])
    }

    // pub fn as_pure_shift_reg(self) -> PureShiftRegister {
    //     PureShiftRegister::from_bytes(self.into_bytes())
    // }
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

/// Differs from [ShiftRegister], since the shift is simply applied to the offset.
// #[bitfield(bits = 12)]
// #[derive(Clone, Copy, Debug, PartialEq, Eq)]
// pub struct PureShiftRegister {
//     pub reg: RegisterName,
//     pub shift: B8,
// }

// impl Display for PureShiftRegister {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         let shift = if self.shift() > 0 {
//             format!(",{}", self.shift())
//         } else {
//             "".into()
//         };
//         write!(f, "{}{}", self.reg(), shift)
//     }
// }

#[bitfield(bits = 12)]
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct RotatedImmediate {
    val: B8,      // To be zero-extended to 32 bits
    rotation: B4, // `val` is right-rotated by twice this amount
}

impl RotatedImmediate {
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
pub enum LoadOrStore {
    Store,
    Load,
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
        ignored2: B6, // 000000
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

    #[derive(Clone, Copy, PartialEq, Eq, Debug, BitfieldSpecifier)]
    #[bits = 1]
    pub enum Signedness {
        Unsigned,
        Signed,
    }

    #[bitfield(bits = 32)]
    #[derive(Clone, Copy, Debug, PartialEq, Eq)]
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
        ignored2: B5,
        pub condition: Condition,
    }
}

pub mod branch_and_exchange {
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

    pub enum CpuMode {
        Arm,
        Thumb,
    }

    impl Op {
        pub fn switch_mode_to(&self) -> CpuMode {
            use CpuMode::*;
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
        /// from the stored 24 bits, add 8 because the CPU will be 2 instructions ahead,
        /// and then return it.
        pub fn offset_get(&self) -> i32 {
            let n = self.offset();
            let signed = ((n >> 23) & 1) == 1;
            let shifted = (n << 2) as i32;

            if signed {
                (shifted | (0b111111 << 26)) + 8
            } else {
                shifted as i32 + 8
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

    impl Display for RegisterList {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(
                f,
                "{{{}}}",
                self.0
                    .view_bits()
                    .iter()
                    .enumerate()
                    .filter_map(
                        |(reg, reg_bit): (usize, bitvec::ptr::BitRef<'_, _, _, Lsb0>)| {
                            if *reg_bit {
                                Some(RegisterName::from(reg as u8).to_string())
                            } else {
                                None
                            }
                        }
                    )
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
    #[derive(Clone, Copy, Debug, PartialEq, Eq)]
    pub struct Op {
        #[skip]
        ignored: B12,
        pub reg_dest: RegisterName,
        #[skip]
        ignored2: B6,
        pub src_psr: PsrLocation,
        #[skip]
        ignored3: B5,
        pub condition: Condition,
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
        ignored2: B5,
        pub condition: Condition,
    }

    #[bitfield(bits = 32)]
    #[derive(Clone, Copy, Debug, PartialEq, Eq)]
    pub struct OpImm {
        /// `src_operand` is either a [RegisterName] or [RotatedImmediate]
        pub src_operand: Operand12Bit,
        #[skip]
        ignored: B10,
        pub dest_psr: PsrLocation,
        #[skip]
        ignored2: B2,
        pub is_imm_operand: bool,
        #[skip]
        ignored3: B2,
        pub condition: Condition,
    }
}

pub mod single_data_transfer {
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

    #[derive(Clone, Copy, PartialEq, Eq, Debug, BitfieldSpecifier)]
    pub enum ByteOrWord {
        Word,
        Byte,
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

    #[bitfield(bits = 32)]
    #[derive(Clone, Copy, Debug, Eq)]
    pub struct Op {
        /// `offset` is either a [ShiftRegister] or [u16] (12-bit immediate)
        pub offset: Operand12Bit,
        pub reg_dest: RegisterName,
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
                && self.reg_dest() == other.reg_dest()
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
    pub fn is_single_data_swap(instr: u32) -> bool {
        let single_data_swap_format = 0b0000_0001_0000_0000_0000_0000_1001_0000;
        let format_mask = 0b0000_1111_1000_0000_0000_1111_1111_0000;
        let extracted_format = instr & format_mask;
        extracted_format == single_data_swap_format
    }
}

pub mod halfword_data_reg {
    pub fn is_halfword_data_transfer_reg(instr: u32) -> bool {
        let halfword_data_transfer_register_format = 0b0000_0000_0000_0000_0000_0000_1001_0000;
        let format_mask = 0b0000_1110_0100_0000_0000_1111_1001_0000;
        let extracted_format = instr & format_mask;
        extracted_format == halfword_data_transfer_register_format
    }
}

pub mod halfword_data_imm {
    pub fn is_halfword_data_transfer_imm(instr: u32) -> bool {
        let halfword_data_transfer_immediate_format = 0b0000_0000_0100_0000_0000_0000_1001_0000;
        let format_mask = 0b0000_1110_0100_0000_0000_0000_1001_0000;
        let extracted_format = instr & format_mask;
        extracted_format == halfword_data_transfer_immediate_format
    }
}

#[cfg(test)]
mod tests {
    use super::single_data_transfer::ByteOrWord;
    use super::AccumulateType::*;
    use super::Condition::*;
    use super::LoadOrStore::*;
    use super::OpCode::*;
    use super::ShiftType::*;
    use super::UpOrDown::*;
    use super::*;
    use lazy_static::lazy_static;

    lazy_static! {
    // Instruction hex, assembly string, expected decoded instruction
    static ref TEST_INSTRUCTIONS: [(u32, &'static str, Instruction); 28] = [
        (
            0xe2833001,
            "add r3, r3, #1",
            Instruction::DataProcessing(
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
            Instruction::DataProcessing(data_processing::Op::new()
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
            Instruction::DataProcessing(data_processing::Op::new()
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
            Instruction::DataProcessing(data_processing::Op::new()
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
            Instruction::DataProcessing(data_processing::Op::new()
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
            Instruction::DataProcessing(data_processing::Op::new()
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
            Instruction::BranchAndBranchWithLink(branch_and_link::Op::new()
                .with_condition(Al)
                .offset_set(0xfffffe)
            )
        ),
        (
            0xebfffffe,
            "bl 0",
            Instruction::BranchAndBranchWithLink(branch_and_link::Op::new()
                .with_condition(Al)
                .with_link(true)
                .offset_set(0xfffffe)

            )
        ),
        (
            0xe3c920aa,
            "bic r2, r9, #170",
            Instruction::DataProcessing(data_processing::Op::new()
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
            Instruction::BranchAndExchange(branch_and_exchange::Op::new()
                .with_condition(Ne)
                .with_rn(R2)
            )
        ),
        (
            0x03720010,
            "cmneq r2, #16",
            Instruction::DataProcessing(data_processing::Op::new()
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
            Instruction::DataProcessing(data_processing::Op::new()
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
            Instruction::DataProcessing(data_processing::Op::new()
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
            Instruction::BlockDataTransfer(block_data_transfer::Op::new()
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
            Instruction::BlockDataTransfer(block_data_transfer::Op::new()
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
            Instruction::BlockDataTransfer(block_data_transfer::Op::new()
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
            Instruction::BlockDataTransfer(block_data_transfer::Op::new()
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
            Instruction::Multiply(multiply::Op::new()
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
            Instruction::DataProcessing(data_processing::Op::new()
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
            Instruction::DataProcessing(data_processing::Op::new()
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
            Instruction::SoftwareInterrupt(software_interrupt::Op::new()
                .with_condition(Ne)
                .with_comment(0x0)
            )
        ),
        (
            0xe0010392,
            "mul r1, r2, r3",
            Instruction::Multiply(multiply::Op::new()
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
            Instruction::DataProcessing(data_processing::Op::new()
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
            Instruction::DataProcessing(data_processing::Op::new()
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
            Instruction::DataProcessing(data_processing::Op::new()
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
            Instruction::DataProcessing(data_processing::Op::new()
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
            Instruction::DataProcessing(data_processing::Op::new()
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
            Instruction::SingleDataTransfer(single_data_transfer::Op::new()
                .with_condition(Mi)
                .with_load_store(Store)
                .with_up_or_down(Down)
                .with_write_back(true)
                .with_pre_post_indexing(PreOrPostIndexing::Post)
                .with_byte_or_word(ByteOrWord::Byte)
                .with_reg_dest(R0)
                .with_base_reg(R1)
                .with_is_reg_offset(true)
                .with_offset(ShiftRegister::new()
                    .with_reg(R2)
                    .with_shift_amt(2)
                    .with_shift_type(LogicalLeft)
                    .into()
                )
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
            let actual_decoded = Instruction::decode(instr);
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
            let actual_str = Instruction::decode(instr).to_string();
            assert_eq!(
                expected_str, actual_str,
                "\nEXPECTED:\n{:#?}\n\nACTUAL:\n{:#?}",
                expected_str, actual_str
            );
        }
    }
}
