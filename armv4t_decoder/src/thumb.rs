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

/*

func IsTHUMBSoftwareInterrupt(opcode uint16) bool {
  const softwareInterruptFormat = 0b1101_1111_0000_0000

  const formatMask = 0b1111_1111_0000_0000

  var extractedFormat = opcode & formatMask

  return extractedFormat == softwareInterruptFormat
}

func IsUnconditionalBranch(opcode uint16) bool {
  const unconditionalBranchFormat = 0b1110_0000_0000_0000

  const formatMask = 0b1111_1000_0000_0000

  var extractedFormat = opcode & formatMask

  return extractedFormat == unconditionalBranchFormat
}

func IsConditionalBranch(opcode uint16) bool {
  const conditionalBranchFormat = 0b1101_0000_0000_0000

  const formatMask = 0b1111_0000_0000_0000

  var extractedFormat = opcode & formatMask

  return extractedFormat == conditionalBranchFormat
}

func IsMultipleLoadstore(opcode uint16) bool {
  const multipleLoadStoreFormat = 0b1100_0000_0000_0000

  const formatMask = 0b1111_0000_0000_0000

  var extractedFormat = opcode & formatMask

  return extractedFormat == multipleLoadStoreFormat
}

func IsLongBranchWithLink(opcode uint16) bool {
  const longBranchWithLinkFormat = 0b1111_0000_0000_0000

  const formatMask = 0b1111_0000_0000_0000

  var extractedFormat = opcode & formatMask

  return extractedFormat == longBranchWithLinkFormat
}

func IsAddOffsetToStackPointer(opcode uint16) bool {
  const addOffsetToStackPointerFormat = 0b1011_0000_0000_0000

  const formatMask = 0b1111_1111_0000_0000

  var extractedFormat = opcode & formatMask

  return extractedFormat == addOffsetToStackPointerFormat
}

func IsPushPopRegisters(opcode uint16) bool {
  const pushopRegistersFormat = 0b1011_0100_0000_0000

  const formatMask = 0b1111_0110_0000_0000

  var extractedFormat = opcode & formatMask

  return extractedFormat == pushopRegistersFormat
}

func IsLoadStoreHalfword(opcode uint16) bool {
  const loadStoreHalfwordFormat = 0b1000_0000_0000_0000

  const formatMask = 0b1111_0000_0000_0000

  var extractedFormat = opcode & formatMask

  return extractedFormat == loadStoreHalfwordFormat
}

func IsSPRelativeLoadStore(opcode uint16) bool {
  const spRelativeLoadStoreFormat = 0b1001_0000_0000_0000

  const formatMask = 0b1111_0000_0000_0000

  var extractedFormat = opcode & formatMask

  return extractedFormat == spRelativeLoadStoreFormat
}

func IsLoadAddress(opcode uint16) bool {
  const loadAddressFormat = 0b1010_0000_0000_0000

  const formatMask = 0b1111_0000_0000_0000

  var extractedFormat = opcode & formatMask

  return extractedFormat == loadAddressFormat
}

func IsLoadStoreWithImmediateOffset(opcode uint16) bool {
  const loadStoreImmediateOffsetFormat = 0b0110_0000_0000_0000

  const formatMask = 0b1110_0000_0000_0000

  var extractedFormat = opcode & formatMask

  return extractedFormat == loadStoreImmediateOffsetFormat
}

func IsLoadStoreWithRegisterOffset(opcode uint16) bool {
  const loadStoreRegisterOffsetFormat = 0b0101_0000_0000_0000

  const formatMask = 0b1111_0010_0000_0000

  var extractedFormat = opcode & formatMask

  return extractedFormat == loadStoreRegisterOffsetFormat
}

func IsLoadStoreSignExtendedByteHalfword(opcode uint16) bool {
  const loadStoreSignExtendedByteHalfwordFormat = 0b0101_0010_0000_0000

  const formatMask = 0b1111_0010_0000_0000

  var extractedFormat = opcode & formatMask

  return extractedFormat == loadStoreSignExtendedByteHalfwordFormat
}

func IsPCRelativeLoad(opcode uint16) bool {
  const pcRelativeLoadFormat = 0b0100_1000_0000_0000

  const formatMask = 0b1111_1000_0000_0000

  var extractedFormat = opcode & formatMask

  return extractedFormat == pcRelativeLoadFormat
}

func IsHiRegisterOperationsBranchExchange(opcode uint16) bool {
  const hiRegisterOperationsBranchExchangeFormat = 0b0100_0100_0000_0000

  const formatMask = 0b1111_1100_0000_0000

  var extractedFormat = opcode & formatMask

  return extractedFormat == hiRegisterOperationsBranchExchangeFormat
}

func IsALUOperations(opcode uint16) bool {
  const aluOperationsFormat = 0b0100_0000_0000_0000

  const formatMask = 0b1111_1100_0000_0000

  var extractedFormat = opcode & formatMask

  return extractedFormat == aluOperationsFormat
}

func IsMoveCompareAddSubtractImmediate(opcode uint16) bool {
  const moveCompareAddSubtractImmediateFormat = 0b0010_0000_0000_0000

  const formatMask = 0b1110_0000_0000_0000

  var extractedFormat = opcode & formatMask

  return extractedFormat == moveCompareAddSubtractImmediateFormat
}

func IsAddSubtract(opcode uint16) bool {
  const addSubtractFormat = 0b0001_1000_0000_0000

  const formatMask = 0b1111_1000_0000_0000

  var extractedFormat = opcode & formatMask

  return extractedFormat == addSubtractFormat
}

func IsMoveShiftedRegister(opcode uint16) bool {
  const moveShiftedRegistersFormat = 0b0000_0000_0000_0000

  const formatMask = 0b1110_0000_0000_0000

  var extractedFormat = opcode & formatMask

  return extractedFormat == moveShiftedRegistersFormat
}
*/

pub enum Instruction {}

mod software_interrupt {
    pub fn is_thumb_software_interrupt(opcode: u16) -> bool {
        let software_interrupt_format: u16 = 0b1101_1111_0000_0000;
        let format_mask: u16 = 0b1111_1111_0000_0000;
        let extracted_format = opcode & format_mask;
        extracted_format == software_interrupt_format
    }
}

mod unconditional_branch {
    pub fn is_unconditional_branch(opcode: u16) -> bool {
        let unconditional_branch_format: u16 = 0b1110_0000_0000_0000;
        let format_mask: u16 = 0b1111_1000_0000_0000;
        let extracted_format = opcode & format_mask;
        extracted_format == unconditional_branch_format
    }
}

mod conditional_branch {
    pub fn is_conditional_branch(opcode: u16) -> bool {
        let conditional_branch_format: u16 = 0b1101_0000_0000_0000;
        let format_mask: u16 = 0b1111_0000_0000_0000;
        let extracted_format = opcode & format_mask;
        extracted_format == conditional_branch_format
    }
}

mod multiple_load_store {
    pub fn is_multiple_load_store(opcode: u16) -> bool {
        let multiple_load_store_format: u16 = 0b1100_0000_0000_0000;
        let format_mask: u16 = 0b1111_0000_0000_0000;
        let extracted_format = opcode & format_mask;
        extracted_format == multiple_load_store_format
    }
}

mod long_branch_with_link {
    pub fn is_long_branch_with_link(opcode: u16) -> bool {
        let long_branch_with_link_format: u16 = 0b1011_0000_0000_0000;
        let format_mask: u16 = 0b1111_1111_0000_0000;
        let extracted_format = opcode & format_mask;
        extracted_format == long_branch_with_link_format
    }
}

mod add_offset_to_stack_pointer {
    pub fn is_add_offset_to_stack_pointer(opcode: u16) -> bool {
        let add_offset_to_stack_pointer_format: u16 = 0b1010_0000_0000_0000;
        let format_mask: u16 = 0b1111_0000_0000_0000;
        let extracted_format = opcode & format_mask;
        extracted_format == add_offset_to_stack_pointer_format
    }
}

mod push_pop_regs {
    pub fn is_push_pop_regs(opcode: u16) -> bool {
        let push_pop_regs_format: u16 = 0b1001_0000_0000_0000;
        let format_mask: u16 = 0b1111_0000_0000_0000;
        let extracted_format = opcode & format_mask;
        extracted_format == push_pop_regs_format
    }
}

mod load_store_half_word {
    pub fn is_load_store_half_word(opcode: u16) -> bool {
        let load_store_half_word_format: u16 = 0b1000_0000_0000_0000;
        let format_mask: u16 = 0b1111_0000_0000_0000;
        let extracted_format = opcode & format_mask;
        extracted_format == load_store_half_word_format
    }
}

mod sp_relative_load_store {
    pub fn is_sp_relative_load_store(opcode: u16) -> bool {
        let sp_relative_load_store_format: u16 = 0b0111_0000_0000_0000;
        let format_mask: u16 = 0b1111_0000_0000_0000;
        let extracted_format = opcode & format_mask;
        extracted_format == sp_relative_load_store_format
    }
}

mod load_address {
    pub fn is_load_address(opcode: u16) -> bool {
        let load_address_format: u16 = 0b0110_0000_0000_0000;
        let format_mask: u16 = 0b1111_0000_0000_0000;
        let extracted_format = opcode & format_mask;
        extracted_format == load_address_format
    }
}

mod load_store_imm_offset {
    pub fn is_load_store_imm_offset(opcode: u16) -> bool {
        let load_store_imm_offset_format: u16 = 0b0101_0000_0000_0000;
        let format_mask: u16 = 0b1111_0000_0000_0000;
        let extracted_format = opcode & format_mask;
        extracted_format == load_store_imm_offset_format
    }
}

mod load_store_reg_offset {
    pub fn is_load_store_reg_offset(opcode: u16) -> bool {
        let load_store_reg_offset_format: u16 = 0b0100_0000_0000_0000;
        let format_mask: u16 = 0b1111_0000_0000_0000;
        let extracted_format = opcode & format_mask;
        extracted_format == load_store_reg_offset_format
    }
}

mod load_store_sign_ext_byte_halfword {
    pub fn is_load_store_sign_ext_byte_halfword(opcode: u16) -> bool {
        let load_store_sign_ext_byte_halfword_format: u16 = 0b0011_0000_0000_0000;
        let format_mask: u16 = 0b1111_0000_0000_0000;
        let extracted_format = opcode & format_mask;
        extracted_format == load_store_sign_ext_byte_halfword_format
    }
}

mod pc_relative_load {
    pub fn is_pc_relative_load(opcode: u16) -> bool {
        let pc_relative_load_format: u16 = 0b0010_0000_0000_0000;
        let format_mask: u16 = 0b1111_0000_0000_0000;
        let extracted_format = opcode & format_mask;
        extracted_format == pc_relative_load_format
    }
}

mod hi_reg_ops_branch_exchange {
    pub fn is_hi_reg_ops_branch_exchange(opcode: u16) -> bool {
        let hi_reg_ops_branch_exchange_format: u16 = 0b0001_0000_0000_0000;
        let format_mask: u16 = 0b1111_0000_0000_0000;
        let extracted_format = opcode & format_mask;
        extracted_format == hi_reg_ops_branch_exchange_format
    }
}

mod alu_ops {
    pub fn is_alu_ops(opcode: u16) -> bool {
        let alu_ops_format: u16 = 0b0000_0000_0000_0000;
        let format_mask: u16 = 0b1111_0000_0000_0000;
        let extracted_format = opcode & format_mask;
        extracted_format == alu_ops_format
    }
}

mod move_compare_add_sub_imm {
    pub fn is_move_compare_add_sub_imm(opcode: u16) -> bool {
        let move_compare_add_sub_imm_format: u16 = 0b1111_0000_0000_0000;
        let format_mask: u16 = 0b1111_0000_0000_0000;
        let extracted_format = opcode & format_mask;
        extracted_format == move_compare_add_sub_imm_format
    }
}

mod add_sub {
    pub fn is_add_sub(opcode: u16) -> bool {
        let add_sub_format: u16 = 0b1110_0000_0000_0000;
        let format_mask: u16 = 0b1111_0000_0000_0000;
        let extracted_format = opcode & format_mask;
        extracted_format == add_sub_format
    }
}

mod move_shifted_reg {
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
