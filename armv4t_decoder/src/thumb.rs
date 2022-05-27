/*
  case IsTHUMBSoftwareInterrupt(opcode):
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

  case IsMoveShiftedRegister(opcode):
    return MoveShiftedRegister
} */
use modular_bitfield::{bitfield, specifiers::*, BitfieldSpecifier, Specifier};
use std::fmt::{Debug, Display};

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Operands {
    One(String),
    Two(String, String),
    Three(String, String, String),
    Four(String, String, String, String),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ThumbInstruction {
    SoftwareInterrupt(software_interrupt::Op),
}

use ThumbInstruction::*;

impl ThumbInstruction {
    pub fn decode(instr: u16) -> Self {
        if software_interrupt::is_thumb_swi(instr) {
            Self::SoftwareInterrupt(software_interrupt::parse(instr))
        } else {
            todo!("Unimplemented THUMB instruction {instr}")
        }
    }

    fn get_operands(&self) -> Operands {
        use Operands::*;

        match self {
            SoftwareInterrupt(op) => One(op.comment().to_string()),
        }
    }

    fn get_mnemonic(&self) -> String {
        match self {
            SoftwareInterrupt(_) => "swi".into(),
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

            _ => todo!("More operands stringified"),
        }
    }
}

// pub pub mod software_interrupt {
//     use super::*;
//     pub fn is_thumb_swi(op: u16) -> bool {
//         let swi_format = 0b1101_1111_0000_0000;
//         let format_mask = 0b1111_1111_0000_0000;
//         let extracted_format = op & format_mask;
//         extracted_format == swi_format
//     }

// #[bitfield(bits = 16)]
// #[derive(Clone, Copy, Debug, PartialEq, Eq)]
// pub struct Op {
//     pub comment: B8,
//     #[skip]
//     ignored: B8,
// }

//     pub fn parse(op: u16) -> Op {
//         Op::from_bytes(op.to_le_bytes())
//     }
// }

/*

func IsTHUMBSoftwareInterrupt(opcode uint16) bool {
  let  softwareInterruptFormat = 0b1101_1111_0000_0000

  let  format_mask = 0b1111_1111_0000_0000

  let  extracted_format = opcode & format_mask

  return extracted_format == softwareInterruptFormat
}

func IsUnconditionalBranch(opcode uint16) bool {
  let  unconditionalBranchFormat = 0b1110_0000_0000_0000

  let  format_mask = 0b1111_1000_0000_0000

  let  extracted_format = opcode & format_mask

  return extracted_format == unconditionalBranchFormat
}

func IsConditionalBranch(opcode uint16) bool {
  let  conditionalBranchFormat = 0b1101_0000_0000_0000

  let  format_mask = 0b1111_0000_0000_0000

  let  extracted_format = opcode & format_mask

  return extracted_format == conditionalBranchFormat
}

func IsMultipleLoadstore(opcode uint16) bool {
  let  multipleLoadStoreFormat = 0b1100_0000_0000_0000

  let  format_mask = 0b1111_0000_0000_0000

  let  extracted_format = opcode & format_mask

  return extracted_format == multipleLoadStoreFormat
}

func IsLongBranchWithLink(opcode uint16) bool {
  let  longBranchWithLinkFormat = 0b1111_0000_0000_0000

  let  format_mask = 0b1111_0000_0000_0000

  let  extracted_format = opcode & format_mask

  return extracted_format == longBranchWithLinkFormat
}

func IsAddOffsetToStackPointer(opcode uint16) bool {
  let  addOffsetToStackPointerFormat = 0b1011_0000_0000_0000

  let  format_mask = 0b1111_1111_0000_0000

  let  extracted_format = opcode & format_mask

  return extracted_format == addOffsetToStackPointerFormat
}

func IsPushPopRegisters(opcode uint16) bool {
  let  pushopRegistersFormat = 0b1011_0100_0000_0000

  let  format_mask = 0b1111_0110_0000_0000

  let  extracted_format = opcode & format_mask

  return extracted_format == pushopRegistersFormat
}

func IsLoadStoreHalfword(opcode uint16) bool {
  let  loadStoreHalfwordFormat = 0b1000_0000_0000_0000

  let  format_mask = 0b1111_0000_0000_0000

  let  extracted_format = opcode & format_mask

  return extracted_format == loadStoreHalfwordFormat
}

func IsSPRelativeLoadStore(opcode uint16) bool {
  let  spRelativeLoadStoreFormat = 0b1001_0000_0000_0000

  let  format_mask = 0b1111_0000_0000_0000

  let  extracted_format = opcode & format_mask

  return extracted_format == spRelativeLoadStoreFormat
}

func IsLoadAddress(opcode uint16) bool {
  let  loadAddressFormat = 0b1010_0000_0000_0000

  let  format_mask = 0b1111_0000_0000_0000

  let  extracted_format = opcode & format_mask

  return extracted_format == loadAddressFormat
}

func IsLoadStoreWithImmediateOffset(opcode uint16) bool {
  let  loadStoreImmediateOffsetFormat = 0b0110_0000_0000_0000

  let  format_mask = 0b1110_0000_0000_0000

  let  extracted_format = opcode & format_mask

  return extracted_format == loadStoreImmediateOffsetFormat
}

func IsLoadStoreWithRegisterOffset(opcode uint16) bool {
  let  loadStoreRegisterOffsetFormat = 0b0101_0000_0000_0000

  let  format_mask = 0b1111_0010_0000_0000

  let  extracted_format = opcode & format_mask

  return extracted_format == loadStoreRegisterOffsetFormat
}

func IsLoadStoreSignExtendedByteHalfword(opcode uint16) bool {
  let  loadStoreSignExtendedByteHalfwordFormat = 0b0101_0010_0000_0000

  let  format_mask = 0b1111_0010_0000_0000

  let  extracted_format = opcode & format_mask

  return extracted_format == loadStoreSignExtendedByteHalfwordFormat
}

func IsPCRelativeLoad(opcode uint16) bool {
  let  pcRelativeLoadFormat = 0b0100_1000_0000_0000

  let  format_mask = 0b1111_1000_0000_0000

  let  extracted_format = opcode & format_mask

  return extracted_format == pcRelativeLoadFormat
}

func IsHiRegisterOperationsBranchExchange(opcode uint16) bool {
  let  hiRegisterOperationsBranchExchangeFormat = 0b0100_0100_0000_0000

  let  format_mask = 0b1111_1100_0000_0000

  let  extracted_format = opcode & format_mask

  return extracted_format == hiRegisterOperationsBranchExchangeFormat
}

func IsALUOperations(opcode uint16) bool {
  let  aluOperationsFormat = 0b0100_0000_0000_0000

  let  format_mask = 0b1111_1100_0000_0000

  let  extracted_format = opcode & format_mask

  return extracted_format == aluOperationsFormat
}

func IsMoveCompareAddSubtractImmediate(opcode uint16) bool {
  let  moveCompareAddSubtractImmediateFormat = 0b0010_0000_0000_0000

  let  format_mask = 0b1110_0000_0000_0000

  let  extracted_format = opcode & format_mask

  return extracted_format == moveCompareAddSubtractImmediateFormat
}

func IsAddSubtract(opcode uint16) bool {
  let  addSubtractFormat = 0b0001_1000_0000_0000

  let  format_mask = 0b1111_1000_0000_0000

  let  extracted_format = opcode & format_mask

  return extracted_format == addSubtractFormat
}

func IsMoveShiftedRegister(opcode uint16) bool {
  let  moveShiftedRegistersFormat = 0b0000_0000_0000_0000

  let  format_mask = 0b1110_0000_0000_0000

  let  extracted_format = opcode & format_mask

  return extracted_format == moveShiftedRegistersFormat
}
*/

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
    pub fn is_unconditional_branch(opcode: u16) -> bool {
        let unconditional_branch_format: u16 = 0b1110_0000_0000_0000;
        let format_mask: u16 = 0b1111_1000_0000_0000;
        let extracted_format = opcode & format_mask;
        extracted_format == unconditional_branch_format
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
    pub fn is_move_shifted_reg(opcode: u16) -> bool {
        let move_shifted_reg_format: u16 = 0b1101_0000_0000_0000;
        let format_mask: u16 = 0b1111_0000_0000_0000;
        let extracted_format = opcode & format_mask;
        extracted_format == move_shifted_reg_format
    }
}

/*
func DecodeTHUMBInstruction(opcode uint16) Instruction {
  switch {
    case IsTHUMBSoftwareInterrupt(opcode):
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

    case IsMoveShiftedRegister(opcode):
      return MoveShiftedRegister
  }

  return UnimplementedTHUMBInstruction
}
*/

#[cfg(test)]
mod tests {
    use super::*;
    use lazy_static::lazy_static;

    lazy_static! {
    // Instruction hex, assembly string, expected decoded instruction
        static ref TEST_INSTRUCTIONS: [(u16, &'static str, ThumbInstruction); 1] = [(
            0xdf08,
            "swi 8",
            ThumbInstruction::SoftwareInterrupt(software_interrupt::Op::new()
                .with_comment(8)),
        ),

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
