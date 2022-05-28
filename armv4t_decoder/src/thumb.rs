/*
  x case IsTHUMBSoftwareInterrupt(opcode):
    return THUMBSoftwareInterrupt

  case IsUnconditionalBranch(opcode):
    return UnconditionalBranch

  case IsConditionalBranch(opcode):
    return ConditionalBranch

  case IsMultipleLoadstore(opcode):
    return MultipleLoadstore

  case IsLongBranchWithLink(opcode):
    return LongBranchWithLink

  case IsAddOffsetToStackPointer(opcode):
    return AddOffsetToStackPointer

  case IsPushPopRegisters(opcode):
    return PushPopRegisters

  case IsLoadStoreHalfword(opcode):
    return LoadStoreHalfword

  case IsSPRelativeLoadStore(opcode):
    return SPRelatvieLoadStore

  case IsLoadAddress(opcode):
    return LoadAddress

  case IsLoadStoreWithImmediateOffset(opcode):
    return LoadStoreWithImmediateOffset

  case IsLoadStoreWithRegisterOffset(opcode):
    return LoadStoreWithRegisterOffset

  case IsLoadStoreSignExtendedByteHalfword(opcode):
    return LoadStoreSignExtendedByteHalfword

  case IsPCRelativeLoad(opcode):
    return PCRelativeLoad

  case IsHiRegisterOperationsBranchExchange(opcode):
    return HiRegisterOperationsBranchExchange

  case IsALUOperations(opcode):
    return ALUOperations

  case IsMoveCompareAddSubtractImmediate(opcode):
    return MoveCompareAddSubtractImmediate

  case IsAddSubtract(opcode):
    return AddSubtract

  x case IsMoveShiftedRegister(opcode):
    return MoveShiftedRegister
} */
use modular_bitfield::{bitfield, specifiers::*, BitfieldSpecifier, Specifier};
use std::fmt::{Debug, Display};

use crate::common::RegisterName;
use modular_bitfield::specifiers::*;

pub const THUMB_INSTR_SIZE_BITS: u8 = 16;
pub const THUMB_INSTR_SIZE_BYTES: u8 = THUMB_INSTR_SIZE_BITS / 8;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Operands {
    One(String),
    Two(String, String),
    Three(String, String, String),
    Four(String, String, String, String),
}

#[bitfield(bits = 5)]
#[derive(Clone, Copy, Debug, PartialEq, Eq, BitfieldSpecifier)]

pub struct Operand5Bit {
    value: B5,
}

impl From<u8> for Operand5Bit {
    fn from(value: u8) -> Self {
        Operand5Bit::new().with_value(value)
    }
}

impl Display for Operand5Bit {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "#{}", self.value())
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ThumbInstruction {
    SoftwareInterrupt(software_interrupt::Op),
    UnconditionalBranch(unconditional_branch::Op),
    MoveShiftedRegister(move_shifted_reg::Op),
}

use ThumbInstruction::*;

impl ThumbInstruction {
    pub fn decode(instr: u16) -> Self {
        if software_interrupt::is_thumb_swi(instr) {
            SoftwareInterrupt(software_interrupt::parse(instr))
        } else if unconditional_branch::is_unconditional_branch(instr) {
            UnconditionalBranch(unconditional_branch::parse(instr))
        } else if move_shifted_reg::is_move_shifted_reg(instr) {
            MoveShiftedRegister(move_shifted_reg::parse(instr))
        } else {
            todo!("Unimplemented THUMB instruction {instr}")
        }
    }

    fn get_operands(&self) -> Operands {
        use Operands::*;

        match self {
            SoftwareInterrupt(op) => One(op.comment().to_string()),
            UnconditionalBranch(op) => One(op.offset11().to_string()),
            MoveShiftedRegister(op) => Three(
                op.reg_dest().to_string(),
                op.reg_src().to_string(),
                op.offset().to_string(),
            ),
        }
    }

    fn get_mnemonic(&self) -> String {
        use move_shifted_reg::ShiftOp::*;

        match self {
            SoftwareInterrupt(_) => "swi".into(),
            UnconditionalBranch(_) => "b".into(),
            MoveShiftedRegister(op) => match op.op() {
                Lsl => "lsl".into(),
                Lsr => "lsr".into(),
                Asr => "asr".into(),
            },
        }
    }
}

impl Display for ThumbInstruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Operands::*;
        let op = self.get_mnemonic();
        let operands = self.get_operands();

        match operands {
            One(op1) => write!(f, "{} {}", op, op1),
            Three(dst, op1, op2) => write!(f, "{} {}, {}, {}", op, dst, op1, op2),
            _ => todo!("More operands stringified"),
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug, BitfieldSpecifier)]
#[bits = 3]
pub enum RegisterName3Bit {
    R0,
    R1,
    R2,
    R3,
    R4,
    R5,
    R6,
    R7,
}

impl Display for RegisterName3Bit {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use RegisterName3Bit::*;
        write!(
            f,
            "r{}",
            match self {
                R0 => 0,
                R1 => 1,
                R2 => 2,
                R3 => 3,
                R4 => 4,
                R5 => 5,
                R6 => 6,
                R7 => 7,
            }
        )
    }
}

pub mod software_interrupt {
    use super::*;

    pub fn is_thumb_swi(opcode: u16) -> bool {
        let software_interrupt_format: u16 = 0b1101_1111_0000_0000;
        let format_mask: u16 = 0b1111_1111_0000_0000;
        let extracted_format = opcode & format_mask;
        extracted_format == software_interrupt_format
    }

    #[bitfield(bits = 16)]
    #[derive(Clone, Copy, Debug, Eq)]
    pub struct Op {
        pub comment: B8,
        #[skip]
        ignored: B8,
    }

    impl PartialEq for Op {
        fn eq(&self, other: &Self) -> bool {
            self.comment() == other.comment()
        }
    }

    pub fn parse(instr: u16) -> Op {
        Op::from_bytes(instr.to_le_bytes())
    }
}

pub mod unconditional_branch {
    use bitvec::macros::internal::funty::Fundamental;

    use super::*;

    pub fn is_unconditional_branch(opcode: u16) -> bool {
        let unconditional_branch_format: u16 = 0b1110_0000_0000_0000;
        let format_mask: u16 = 0b1111_1000_0000_0000;
        let extracted_format = opcode & format_mask;
        extracted_format == unconditional_branch_format
    }

    #[bitfield(bits = 11)]
    #[derive(Clone, Copy, Debug, PartialEq, Eq, BitfieldSpecifier)]
    /// Two's complement 12-bit signed PC relative offset that is 2-byte aligned (i.e. bit 0 is 0)
    pub struct Offset11 {
        value: B11,
    }

    impl From<i16> for Offset11 {
        fn from(value: i16) -> Self {
            let first_11bits = value & ((1 << 11) - 1);
            Offset11::new().with_value(first_11bits as u16)
        }
    }

    impl Display for Offset11 {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            let sign_extended = (0b1111_1000_0000_0000 | self.value()) as i16;
            // PC is two instructions ahead, so to recontruct add 2 * 2
            let val = (sign_extended << 1) + (THUMB_INSTR_SIZE_BYTES * 2) as i16;
            write!(f, "{}", val)
        }
    }

    #[bitfield(bits = 16)]
    #[derive(Clone, Copy, Debug, Eq)]
    pub struct Op {
        pub offset11: Offset11,
        #[skip]
        ignored: B5,
    }

    impl PartialEq for Op {
        fn eq(&self, other: &Self) -> bool {
            self.offset11() == other.offset11()
        }
    }

    pub fn parse(instr: u16) -> Op {
        Op::from_bytes(instr.to_le_bytes())
    }
}

pub mod conditional_branch {
    pub fn is_conditional_branch(opcode: u16) -> bool {
        let conditional_branch_format: u16 = 0b1101_0000_0000_0000;
        let format_mask: u16 = 0b1111_0000_0000_0000;
        let extracted_format = opcode & format_mask;
        extracted_format == conditional_branch_format
    }
}

pub mod multiple_load_store {
    pub fn is_multiple_load_store(opcode: u16) -> bool {
        let multiple_load_store_format: u16 = 0b1100_0000_0000_0000;
        let format_mask: u16 = 0b1111_0000_0000_0000;
        let extracted_format = opcode & format_mask;
        extracted_format == multiple_load_store_format
    }
}

pub mod long_branch_with_link {
    pub fn is_long_branch_with_link(opcode: u16) -> bool {
        let long_branch_with_link_format: u16 = 0b1011_0000_0000_0000;
        let format_mask: u16 = 0b1111_1111_0000_0000;
        let extracted_format = opcode & format_mask;
        extracted_format == long_branch_with_link_format
    }
}

pub mod add_offset_to_stack_pointer {
    pub fn is_add_offset_to_stack_pointer(opcode: u16) -> bool {
        let add_offset_to_stack_pointer_format: u16 = 0b1010_0000_0000_0000;
        let format_mask: u16 = 0b1111_0000_0000_0000;
        let extracted_format = opcode & format_mask;
        extracted_format == add_offset_to_stack_pointer_format
    }
}

pub mod push_pop_regs {
    pub fn is_push_pop_regs(opcode: u16) -> bool {
        let push_pop_regs_format: u16 = 0b1001_0000_0000_0000;
        let format_mask: u16 = 0b1111_0000_0000_0000;
        let extracted_format = opcode & format_mask;
        extracted_format == push_pop_regs_format
    }
}

pub mod load_store_half_word {
    pub fn is_load_store_half_word(opcode: u16) -> bool {
        let load_store_half_word_format: u16 = 0b1000_0000_0000_0000;
        let format_mask: u16 = 0b1111_0000_0000_0000;
        let extracted_format = opcode & format_mask;
        extracted_format == load_store_half_word_format
    }
}

pub mod sp_relative_load_store {
    pub fn is_sp_relative_load_store(opcode: u16) -> bool {
        let sp_relative_load_store_format: u16 = 0b0111_0000_0000_0000;
        let format_mask: u16 = 0b1111_0000_0000_0000;
        let extracted_format = opcode & format_mask;
        extracted_format == sp_relative_load_store_format
    }
}

pub mod load_address {
    pub fn is_load_address(opcode: u16) -> bool {
        let load_address_format: u16 = 0b0110_0000_0000_0000;
        let format_mask: u16 = 0b1111_0000_0000_0000;
        let extracted_format = opcode & format_mask;
        extracted_format == load_address_format
    }
}

pub mod load_store_imm_offset {
    pub fn is_load_store_imm_offset(opcode: u16) -> bool {
        let load_store_imm_offset_format: u16 = 0b0101_0000_0000_0000;
        let format_mask: u16 = 0b1111_0000_0000_0000;
        let extracted_format = opcode & format_mask;
        extracted_format == load_store_imm_offset_format
    }
}

pub mod load_store_reg_offset {
    pub fn is_load_store_reg_offset(opcode: u16) -> bool {
        let load_store_reg_offset_format: u16 = 0b0100_0000_0000_0000;
        let format_mask: u16 = 0b1111_0000_0000_0000;
        let extracted_format = opcode & format_mask;
        extracted_format == load_store_reg_offset_format
    }
}

pub mod load_store_sign_ext_byte_halfword {
    pub fn is_load_store_sign_ext_byte_halfword(opcode: u16) -> bool {
        let load_store_sign_ext_byte_halfword_format: u16 = 0b0011_0000_0000_0000;
        let format_mask: u16 = 0b1111_0000_0000_0000;
        let extracted_format = opcode & format_mask;
        extracted_format == load_store_sign_ext_byte_halfword_format
    }
}

pub mod pc_relative_load {
    pub fn is_pc_relative_load(opcode: u16) -> bool {
        let pc_relative_load_format: u16 = 0b0010_0000_0000_0000;
        let format_mask: u16 = 0b1111_0000_0000_0000;
        let extracted_format = opcode & format_mask;
        extracted_format == pc_relative_load_format
    }
}

pub mod hi_reg_ops_branch_exchange {
    pub fn is_hi_reg_ops_branch_exchange(opcode: u16) -> bool {
        let hi_reg_ops_branch_exchange_format: u16 = 0b0001_0000_0000_0000;
        let format_mask: u16 = 0b1111_0000_0000_0000;
        let extracted_format = opcode & format_mask;
        extracted_format == hi_reg_ops_branch_exchange_format
    }
}

pub mod alu_ops {
    pub fn is_alu_ops(opcode: u16) -> bool {
        let alu_ops_format: u16 = 0b0000_0000_0000_0000;
        let format_mask: u16 = 0b1111_0000_0000_0000;
        let extracted_format = opcode & format_mask;
        extracted_format == alu_ops_format
    }
}

pub mod move_compare_add_sub_imm {
    pub fn is_move_compare_add_sub_imm(opcode: u16) -> bool {
        let move_compare_add_sub_imm_format: u16 = 0b1111_0000_0000_0000;
        let format_mask: u16 = 0b1111_0000_0000_0000;
        let extracted_format = opcode & format_mask;
        extracted_format == move_compare_add_sub_imm_format
    }
}

pub mod add_sub {
    pub fn is_add_sub(opcode: u16) -> bool {
        let add_sub_format: u16 = 0b1110_0000_0000_0000;
        let format_mask: u16 = 0b1111_0000_0000_0000;
        let extracted_format = opcode & format_mask;
        extracted_format == add_sub_format
    }
}

pub mod move_shifted_reg {
    use super::*;

    pub fn is_move_shifted_reg(opcode: u16) -> bool {
        let move_shifted_reg_format: u16 = 0b0000_0000_0000_0000;
        let format_mask: u16 = 0b1110_0000_0000_0000;
        let extracted_format = opcode & format_mask;
        extracted_format == move_shifted_reg_format
    }

    #[derive(Clone, Copy, PartialEq, Eq, Debug, BitfieldSpecifier)]
    #[bits = 2]
    pub enum ShiftOp {
        Lsl,
        Lsr,
        Asr,
    }

    #[bitfield(bits = 16)]
    #[derive(Clone, Copy, Debug, Eq)]
    pub struct Op {
        pub reg_dest: RegisterName3Bit,
        pub reg_src: RegisterName3Bit,
        pub offset: Operand5Bit,
        pub op: ShiftOp,
        #[skip]
        ignored: B3,
    }
    impl PartialEq for Op {
        fn eq(&self, other: &Self) -> bool {
            self.reg_dest() == other.reg_dest()
                && self.reg_src() == other.reg_src()
                && self.offset() == other.offset()
                && self.op() == other.op()
        }
    }

    pub fn parse(instr: u16) -> Op {
        Op::from_bytes(instr.to_le_bytes())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use lazy_static::lazy_static;

    lazy_static! {
    // Instruction hex, assembly string, expected decoded instruction
        static ref TEST_INSTRUCTIONS: [(u16, &'static str, ThumbInstruction); 3] = [
          (
            0xdf08,
            "swi 8",
            ThumbInstruction::SoftwareInterrupt(software_interrupt::Op::new()
              .with_comment(8)
            ),
          ),
          (
            0x0091,
            "lsl r1, r2, #2",
            ThumbInstruction::MoveShiftedRegister(move_shifted_reg::Op::new()
              .with_reg_dest(RegisterName3Bit::R1)
              .with_reg_src(RegisterName3Bit::R2)
              .with_offset(2.into())
              .with_op(move_shifted_reg::ShiftOp::Lsl)
            ),
          ),
          (
            0xe7fe,
            "b 0",
            ThumbInstruction::UnconditionalBranch(unconditional_branch::Op::new()
              .with_offset11((-4 >> 1).into())
            ),
          )
        ];
    }

    #[test]
    fn decode_instructions_test() {
        for (instr, _, expect_decoded) in TEST_INSTRUCTIONS.into_iter() {
            let actual_decoded = ThumbInstruction::decode(instr);
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
            let actual_str = ThumbInstruction::decode(instr).to_string();
            assert_eq!(
                expected_str, actual_str,
                "\nEXPECTED:\n{:#?}\n\nACTUAL:\n{:#?}",
                expected_str, actual_str
            );
        }
    }
}
