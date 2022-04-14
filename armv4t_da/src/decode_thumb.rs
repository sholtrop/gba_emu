pub enum Instruction {}

pub struct Decoder {}

impl Decoder {
    pub fn decode(instr: u32) -> Instruction {
        match instr {
            _ => todo!(),
        }
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
