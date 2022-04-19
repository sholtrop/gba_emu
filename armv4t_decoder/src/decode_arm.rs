use modular_bitfield::BitfieldSpecifier;
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

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Register {
    R0,
}

impl Display for Register {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match &self {
                Register::R0 => "R0",
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
pub enum Op {
    // Add with carry
    Adc {
        op1: Register,
        op2: Operand,
        dst: Register,
    },
    // Add
    Add {
        op1: Register,
        op2: Operand,
        dst: Register,
    },
    // Logical AND
    And,
    B,   // Branch
    Bic, // Bit clear
    Bl,  // Branch with link
    Bx,  // Branch and Exchange
    Cdp, // Coprocessor Data Processing
    Cmn, // Compare negative
    Cmp, // Compare
    Eor, // Exclusive OR
    Ldc, // Load coprocessor from memory
    Ldm, // Load multiple regisers
    Ldr, // Load register from memory
    Mcr, // Move CPU register to coprocessor register
    Mov, // Move register or constant
    Mrc, // Move from coprocessor register to CPU register
    Mrs, // Move PSR status/flags to register
    Msr, // Move register to PSR status/flags

    // Multiply
    Mul {
        accumulate: Accumulate,
        set_condition: SetConditionCodes,
        op1: Register,
        op2: Register,
    },
    // Multiply long
    Mull {
        sign: Signedness,
        accumulate: Accumulate,
        set_condition: SetConditionCodes,
        dest_low: Register,
        dest_high: Register,
        op1: Register,
        op2: Register,
    },
    Mvn, // Move negative register
    Orr, // Or
    Rsb, // Reverse subtract
    Rsc, // Reverse subtract with carry
    Sbc, // Subtract with carry
    Stc, // Store coprocessor register to memory
    Stm, // Store multiple
    Str, // Store register to memory
    Sub, // Subtract

    // Software interrupt
    Swi {
        comment_field: Immediate,
    },
    // Swap register with memory
    Swp {
        src: Register,
        dst: Register,
        base: Register,
        size: ByteOrWord,
    },
    Teq, // Test bitwise equality
    Tst, // Test bits
    Unknown,
}

impl Display for Op {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match &self {
                Op::Adc { .. } => "ADC",
                Op::Add { .. } => "ADD",
                Op::And => "AND",
                Op::B => "B",
                Op::Bic => "BIC",
                Op::Bl => "BL",
                Op::Bx => "BX",
                Op::Cdp => "CDP",
                Op::Cmn => "CMN",
                Op::Cmp => "CMP",
                Op::Eor => "EOR",
                Op::Ldc => "LDC",
                Op::Ldm => "LDM",
                Op::Ldr => "LDR",
                Op::Mcr => "MCR",
                Op::Mov => "MOV",
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
                Op::Mvn => "MVN",
                Op::Orr => "ORR",
                Op::Rsb => "RSB",
                Op::Rsc => "RSC",
                Op::Sbc => "SBC",
                Op::Stc => "STC",
                Op::Stm => "STM",
                Op::Str => "STR",
                Op::Sub => "SUB",
                Op::Swi { .. } => "SWI",
                Op::Swp { .. } => "SWP",
                Op::Teq => "TEQ",
                Op::Tst => "TST",
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Operand {
    Register(Register),
    Immediate(Immediate),
}

impl Display for Operand {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match &self {
                Operand::Register(r) => format!("R{}", r),
                Operand::Immediate(i) => format!("#{}", i),
            }
        )
    }
}

impl From<Register> for Operand {
    fn from(r: Register) -> Self {
        Self::Register(r)
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
        Option<Operand>,
        Option<Operand>,
        Option<Operand>,
        Option<Operand>,
    ) {
        match self.op {
            Op::Adc { dst, op1, op2 } => (Some(dst.into()), Some(op1.into()), Some(op2), None),
            Op::Add { dst, op1, op2 } => (Some(dst.into()), Some(op1.into()), Some(op2), None),
            Op::Mull {
                dest_low,
                dest_high,
                op1,
                op2,
                ..
            } => (
                Some(dest_low.into()),
                Some(dest_high.into()),
                Some(op1.into()),
                Some(op2.into()),
            ),
            Op::Swp { src, dst, base, .. } => {
                (Some(dst.into()), Some(src.into()), Some(base.into()), None)
            }
            Op::Swi { comment_field } => (Some(comment_field.into()), None, None, None),
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
    DataProcessing,
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
            _ => todo!("{:?}", format),
        }
    }

    /// Get the condition field of the ARM instruction
    fn get_condition(instr: u32) -> Condition {
        let cond_bits: u8 = (instr >> (INSTR_SIZE - COND_SIZE)).try_into().unwrap();
        Condition::from_bytes(cond_bits)
            .unwrap_or_else(|e| panic!("Invalid bit pattern for ARM condition: {e}"))
    }

    // #[inline]
    // #[allow(dead_code)]
    // /// Return the 12 bit index for the [DecoderTable]:
    // /// ```
    // /// |   28    24    20    16    12     8     4     0
    // /// |-----|-----|-----|-----|-----|-----|-----|-----|
    // /// |1111 |1111 |1111 |1111 |1111 |1111 |1111 |1111 |
    // /// |-----|-----|-----|-----|-----|-----|-----|-----|
    // /// |     |BA98 |7654 |     |     |3210 |     |     |
    // /// ```
    // fn get_index(instr: u32) -> usize {
    //     ((instr >> 16) & 0xFF0 | ((instr >> 4) & 0x00F)) as usize
    // }

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

    #[test]
    fn swi_decode() {
        // SWINE 0
        assert_eq!(
            Decoder::decode(0x1F000000).op,
            Op::Swi {
                comment_field: Immediate(0)
            }
        );
        // SWILE 0
        assert_eq!(
            Decoder::decode(0xDF000000).op,
            Op::Swi {
                comment_field: Immediate(0)
            }
        );
        // SWIVC 0
        assert_eq!(
            Decoder::decode(0x7F000000).op,
            Op::Swi {
                comment_field: Immediate(0)
            }
        );
        // SWIGE 0xFFFFFF
        assert_eq!(
            Decoder::decode(0xAFFFFFFF).op,
            Op::Swi {
                comment_field: Immediate(0xFFFFFF)
            }
        );
    }
}
