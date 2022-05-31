use bitvec::order::Lsb0;
use bitvec::view::BitView;
/*
  x case IsTHUMBSoftwareInterrupt(opcode):
    return THUMBSoftwareInterrupt

  x case IsUnconditionalBranch(opcode):
    return UnconditionalBranch

  x case IsConditionalBranch(opcode):
    return ConditionalBranch

  x case IsMultipleLoadstore(opcode):
    return MultipleLoadstore

  x case IsLongBranchWithLink(opcode):
    return LongBranchWithLink

  x case IsAddOffsetToStackPointer(opcode):
    return AddOffsetToStackPointer

  x case IsPushPopRegisters(opcode):
    return PushPopRegisters

  x case IsLoadStoreHalfword(opcode):
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
use modular_bitfield::{bitfield, specifiers::*, BitfieldSpecifier};
use std::fmt::{Debug, Display};

use crate::common::{LoadOrStore::*, RegisterName};

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
    MultipleLoadstore(multiple_load_store::Op),
    LongBranchWithLink(long_branch_with_link::Op),
    AddOffsetToStackPointer(add_offset_to_stack_pointer::Op),
    PushPopRegister(push_pop_regs::Op),
    LoadStoreHalfWord(load_store_half_word::Op),
    MoveShiftedRegister(move_shifted_reg::Op),
    SpRelativeLoadStore(sp_relative_load_store::Op),
    ConditionalBranch(conditional_branch::Op),
}

use ThumbInstruction::*;

impl ThumbInstruction {
    pub fn decode(instr: u16) -> Self {
        if software_interrupt::is_thumb_swi(instr) {
            SoftwareInterrupt(software_interrupt::parse(instr))
        } else if unconditional_branch::is_unconditional_branch(instr) {
            UnconditionalBranch(unconditional_branch::parse(instr))
        } else if conditional_branch::is_conditional_branch(instr) {
            ConditionalBranch(conditional_branch::parse(instr))
        } else if multiple_load_store::is_multiple_load_store(instr) {
            MultipleLoadstore(multiple_load_store::parse(instr))
        } else if long_branch_with_link::is_long_branch_with_link(instr) {
            LongBranchWithLink(long_branch_with_link::parse(instr))
        } else if add_offset_to_stack_pointer::is_add_offset_to_stack_pointer(instr) {
            AddOffsetToStackPointer(add_offset_to_stack_pointer::parse(instr))
        } else if push_pop_regs::is_push_pop_regs(instr) {
            PushPopRegister(push_pop_regs::parse(instr))
        } else if load_store_half_word::is_load_store_half_word(instr) {
            LoadStoreHalfWord(load_store_half_word::parse(instr))
        } else if sp_relative_load_store::is_sp_relative_load_store(instr) {
            SpRelativeLoadStore(sp_relative_load_store::parse(instr))
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
            MultipleLoadstore(op) => Two(format!("{}!", op.base_reg()), op.reg_list().to_string()),
            LongBranchWithLink(op) => Two(op.offset11().to_string(), op.off_hi_lo().to_string()),
            AddOffsetToStackPointer(op) => Two("sp".into(), format!("#{}", op.get_imm())),
            PushPopRegister(op) => {
                let mut reg_list = op.reg_list().to_string();
                if op.load_pc_store_lr() {
                    let len = reg_list.len();
                    let lr_pc = match op.load_or_store() {
                        Store => ", lr",
                        Load => ", pc",
                    };
                    reg_list.insert_str(len - 1, lr_pc);
                }
                One(reg_list)
            }
            LoadStoreHalfWord(op) => {
                let rd = op.dest_reg().to_string();
                let op1 = format!("[{}, #{}]", op.base_reg(), op.offset());
                Two(rd, op1)
            }
            SpRelativeLoadStore(op) => {
                let rd = op.reg_dest().to_string();
                let op1 = format!("[sp, #{}]", op.offset());
                Two(rd, op1)
            }
            ConditionalBranch(op) => One(op.offset8().to_string()),
        }
    }

    fn get_mnemonic(&self) -> String {
        use move_shifted_reg::ShiftOp::*;

        match self {
            SoftwareInterrupt(_) => "swi".into(),
            UnconditionalBranch(_) => "b".into(),
            ConditionalBranch(op) => format!("b{}", op.condition()),
            MultipleLoadstore(op) => match op.load_or_store() {
                Load => "ldmia".into(),
                Store => "stmia".into(),
            },
            LongBranchWithLink(_) => "bl".into(),
            MoveShiftedRegister(op) => match op.op() {
                Lsl => "lsl".into(),
                Lsr => "lsr".into(),
                Asr => "asr".into(),
            },
            PushPopRegister(op) => match op.load_or_store() {
                Store => "push".into(),
                Load => "pop".into(),
            },
            LoadStoreHalfWord(op) => match op.load_or_store() {
                Load => "ldrh".into(),
                Store => "strh".into(),
            },
            SpRelativeLoadStore(op) => match op.load_or_store() {
                Load => "ldr".into(),
                Store => "str".into(),
            },
            AddOffsetToStackPointer(_) => "add".into(),
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
            Two(op1, op2) => write!(f, "{} {}, {}", op, op1, op2),
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

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct RegisterList8Bit(u8);

impl Display for RegisterList8Bit {
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
            let mask = 1 << (11 - 1) as i16;
            let sign_extended = (self.value() as i16 ^ mask) - mask;
            // PC is two instructions ahead, so to recontruct add 2 * 2
            // Offset is half-word aligned, so to recontruct shift left by 1
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
    use super::*;
    use crate::common::Condition;

    pub fn is_conditional_branch(opcode: u16) -> bool {
        let conditional_branch_format: u16 = 0b1101_0000_0000_0000;
        let format_mask: u16 = 0b1111_0000_0000_0000;
        let extracted_format = opcode & format_mask;
        extracted_format == conditional_branch_format
    }

    pub fn parse(instr: u16) -> Op {
        Op::from_bytes(instr.to_le_bytes())
    }

    #[bitfield(bits = 8)]
    #[derive(Clone, Copy, Debug, PartialEq, Eq, BitfieldSpecifier)]
    /// Two's complement 12-bit signed PC relative offset that is 2-byte aligned (i.e. bit 0 is 0)
    pub struct Offset8 {
        value: B8,
    }

    impl From<i8> for Offset8 {
        fn from(value: i8) -> Self {
            Offset8::new().with_value(value as u8)
        }
    }

    impl Display for Offset8 {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            let mask = 1 << (8 - 1) as i16;
            let sign_extended = (self.value() as i16 ^ mask) - mask;
            // PC is two instructions ahead, so to recontruct add 2 * 2
            // Offset is half-word aligned, so to recontruct shift left by 1
            let val = (sign_extended << 1) + (THUMB_INSTR_SIZE_BYTES * 2) as i16;
            write!(f, "{}", val)
        }
    }

    #[bitfield(bits = 16)]
    #[derive(Clone, Copy, Debug, Eq)]
    pub struct Op {
        pub offset8: Offset8,
        pub condition: Condition,
        #[skip]
        ignored: B4,
    }

    impl PartialEq for Op {
        fn eq(&self, other: &Self) -> bool {
            self.offset8() == other.offset8() && self.condition() == other.condition()
        }
    }
}

pub mod multiple_load_store {
    use super::*;
    use crate::common::LoadOrStore;

    pub fn is_multiple_load_store(opcode: u16) -> bool {
        let multiple_load_store_format: u16 = 0b1100_0000_0000_0000;
        let format_mask: u16 = 0b1111_0000_0000_0000;
        let extracted_format = opcode & format_mask;
        extracted_format == multiple_load_store_format
    }

    pub fn parse(instr: u16) -> Op {
        Op::from_bytes(instr.to_le_bytes())
    }

    #[bitfield(bits = 16)]
    #[derive(Clone, Copy, Debug, Eq)]
    pub struct Op {
        pub regs: B8,
        pub base_reg: RegisterName3Bit,
        pub load_or_store: LoadOrStore,
        #[skip]
        ignored: B4,
    }

    impl Op {
        pub fn reg_list(&self) -> RegisterList8Bit {
            let regs = self.regs();
            RegisterList8Bit(regs)
        }
    }

    impl PartialEq for Op {
        fn eq(&self, other: &Self) -> bool {
            self.regs() == other.regs()
                && self.base_reg() == other.base_reg()
                && self.load_or_store() == other.load_or_store()
        }
    }
}

pub mod long_branch_with_link {
    use super::*;

    pub fn is_long_branch_with_link(opcode: u16) -> bool {
        let long_branch_with_link_format: u16 = 0b1111_0000_0000_0000;
        let format_mask: u16 = 0b1111_0000_0000_0000;
        let extracted_format = opcode & format_mask;
        extracted_format == long_branch_with_link_format
    }

    pub fn parse(instr: u16) -> Op {
        Op::from_bytes(instr.to_le_bytes())
    }

    #[derive(Clone, Copy, PartialEq, Eq, Debug, BitfieldSpecifier)]
    #[bits = 1]
    pub enum OffsetHighLow {
        High,
        Low,
    }

    impl Display for OffsetHighLow {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(
                f,
                "{}",
                match self {
                    OffsetHighLow::High => "hi",
                    OffsetHighLow::Low => "lo",
                }
            )
        }
    }

    #[bitfield(bits = 16)]
    #[derive(Clone, Copy, Debug, Eq)]
    pub struct Op {
        pub offset11: B11,
        pub off_hi_lo: OffsetHighLow,
        #[skip]
        ignored: B4,
    }

    impl PartialEq for Op {
        fn eq(&self, other: &Self) -> bool {
            self.offset11() == other.offset11() && self.off_hi_lo() == other.off_hi_lo()
        }
    }
}

pub mod add_offset_to_stack_pointer {
    use super::*;
    use crate::common::Signedness;
    use crate::common::Signedness::*;

    pub fn is_add_offset_to_stack_pointer(opcode: u16) -> bool {
        let add_offset_to_stack_pointer_format: u16 = 0b1011_0000_0000_0000;
        let format_mask: u16 = 0b1111_1111_0000_0000;
        let extracted_format = opcode & format_mask;
        extracted_format == add_offset_to_stack_pointer_format
    }

    pub fn parse(instr: u16) -> Op {
        Op::from_bytes(instr.to_le_bytes())
    }

    #[bitfield(bits = 16)]
    #[derive(Clone, Copy, Debug, Eq)]
    pub struct Op {
        pub imm8: B7,
        pub sign: Signedness,
        #[skip]
        ignored: B8,
    }

    impl Op {
        pub fn get_imm(&self) -> i16 {
            dbg!(self.imm8());
            let val = (self.imm8() as i16) << 2;
            if self.sign() == Signed {
                -val
            } else {
                val
            }
        }
    }

    impl PartialEq for Op {
        fn eq(&self, other: &Self) -> bool {
            self.imm8() == other.imm8() && self.sign() == other.sign()
        }
    }
}

pub mod push_pop_regs {
    use crate::common::LoadOrStore;

    use super::*;

    pub fn is_push_pop_regs(opcode: u16) -> bool {
        let push_pop_regs_format: u16 = 0b1011_0100_0000_0000;
        let format_mask: u16 = 0b1111_0110_0000_0000;
        let extracted_format = opcode & format_mask;
        extracted_format == push_pop_regs_format
    }

    pub fn parse(instr: u16) -> Op {
        Op::from_bytes(instr.to_le_bytes())
    }

    #[bitfield(bits = 16)]
    #[derive(Clone, Copy, Debug, Eq)]
    pub struct Op {
        pub regs: B8,
        pub load_pc_store_lr: bool,
        #[skip]
        ignored: B2,
        pub load_or_store: LoadOrStore,
        #[skip]
        ignored2: B4,
    }

    impl Op {
        pub fn reg_list(&self) -> RegisterList8Bit {
            let regs = self.regs();
            RegisterList8Bit(regs)
        }
    }

    impl PartialEq for Op {
        fn eq(&self, other: &Self) -> bool {
            self.regs() == other.regs()
                && self.load_pc_store_lr() == other.load_pc_store_lr()
                && self.load_or_store() == other.load_or_store()
        }
    }
}

pub mod load_store_half_word {
    use super::*;
    use crate::common::LoadOrStore;

    pub fn is_load_store_half_word(opcode: u16) -> bool {
        let load_store_half_word_format: u16 = 0b1000_0000_0000_0000;
        let format_mask: u16 = 0b1111_0000_0000_0000;
        let extracted_format = opcode & format_mask;
        extracted_format == load_store_half_word_format
    }

    pub fn parse(instr: u16) -> Op {
        Op::from_bytes(instr.to_le_bytes())
    }

    #[bitfield(bits = 16)]
    #[derive(Clone, Copy, Debug, Eq)]
    pub struct Op {
        pub dest_reg: RegisterName3Bit,
        pub base_reg: RegisterName3Bit,
        pub offset5: B5,
        pub load_or_store: LoadOrStore,
        #[skip]
        ignored: B4,
    }

    impl Op {
        pub fn offset(&self) -> u8 {
            self.offset5() << 1
        }

        pub fn with_offset(&self, offset: u8) -> Self {
            let offset = offset >> 1;
            self.with_offset5(offset)
        }

        pub fn set_offset(&mut self, offset: u8) {
            let offset = offset >> 1;
            self.set_offset5(offset);
        }
    }

    impl PartialEq for Op {
        fn eq(&self, other: &Self) -> bool {
            self.dest_reg() == other.dest_reg()
                && self.base_reg() == other.base_reg()
                && self.offset5() == other.offset5()
                && self.load_or_store() == other.load_or_store()
        }
    }
}

pub mod sp_relative_load_store {
    use super::*;
    use crate::common::LoadOrStore;

    pub fn is_sp_relative_load_store(opcode: u16) -> bool {
        let sp_relative_load_store_format: u16 = 0b1001_0000_0000_0000;
        let format_mask: u16 = 0b1111_0000_0000_0000;
        let extracted_format = opcode & format_mask;
        extracted_format == sp_relative_load_store_format
    }

    pub fn parse(instr: u16) -> Op {
        Op::from_bytes(instr.to_le_bytes())
    }

    #[bitfield(bits = 16)]
    #[derive(Clone, Copy, Debug, Eq)]
    pub struct Op {
        pub word8: B8,
        pub reg_dest: RegisterName3Bit,
        pub load_or_store: LoadOrStore,
        #[skip]
        ignored: B4,
    }

    impl Op {
        pub fn offset(&self) -> u16 {
            (self.word8() as u16) << 2
        }

        pub fn set_offset(&mut self, offset: u16) {
            self.set_word8((offset >> 2) as u8);
        }

        pub fn with_offset(&self, offset: u16) -> Self {
            let word8 = (offset >> 2) as u8;
            self.with_word8(word8)
        }
    }

    impl PartialEq for Op {
        fn eq(&self, other: &Self) -> bool {
            self.word8() == other.word8()
                && self.reg_dest() == other.reg_dest()
                && self.load_or_store() == other.load_or_store()
        }
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
    use super::long_branch_with_link::OffsetHighLow::*;
    use super::RegisterName3Bit::*;
    use super::*;
    use crate::common::Condition::*;
    use crate::common::Signedness::*;
    use lazy_static::lazy_static;

    lazy_static! {
    // Instruction hex, assembly string, expected decoded instruction
        static ref TEST_INSTRUCTIONS: [(u16, &'static str, ThumbInstruction); 15] = [
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
          ),
          (
            0xdcfe,
            "bgt 0",
            ThumbInstruction::ConditionalBranch(conditional_branch::Op::new()
              .with_offset8((-4 >> 1).into())
              .with_condition(Gt)
            ),
          ),
          (
            0xd401,
            "bmi 6",
            ThumbInstruction::ConditionalBranch(conditional_branch::Op::new()
              .with_offset8((2 >> 1).into())
              .with_condition(Mi)
            ),
          ),
          (
            0xc0f8,
            "stmia r0!, {r3, r4, r5, r6, r7}",
            ThumbInstruction::MultipleLoadstore(multiple_load_store::Op::new()
                .with_base_reg(R0)
                .with_load_or_store(Store)
                .with_regs(0b1111_1000)
            ),
          ),
          (
            0xc812,
            "ldmia r0!, {r1, r4}",
            ThumbInstruction::MultipleLoadstore(multiple_load_store::Op::new()
                .with_base_reg(R0)
                .with_load_or_store(Load)
                .with_regs(0b0001_0010)
            ),
          ),
          // Next two instructions belong together, but we have to test them separately
          // They encode a long branch with link with offset 0x10000
          (
            0xf800,
            "bl 0, lo",
            ThumbInstruction::LongBranchWithLink(long_branch_with_link::Op::new()
              .with_offset11((0x10000_u32 & 0x7FF) as u16)
              .with_off_hi_lo(Low)
            ),
          ),
          (
            0xf010,
            "bl 16, hi",
            ThumbInstruction::LongBranchWithLink(long_branch_with_link::Op::new()
                .with_offset11((0x10000_u32 >> 12) as u16 & 0x7FF)
                .with_off_hi_lo(High)
            ),
          ),
          (
            0xb043,
            "add sp, #268",
            ThumbInstruction::AddOffsetToStackPointer(add_offset_to_stack_pointer::Op::new()
              .with_imm8((268_u16 >> 2) as u8)
              .with_sign(Unsigned)
            ),
          ),
          (
            0xb51f,
            "push {r0, r1, r2, r3, r4, lr}",
            ThumbInstruction::PushPopRegister(push_pop_regs::Op::new()
                .with_regs(0b0001_1111)
                .with_load_pc_store_lr(true)
                .with_load_or_store(Store)
            ),
          ),
          (
            0xbd44,
            "pop {r2, r6, pc}",
            ThumbInstruction::PushPopRegister(push_pop_regs::Op::new()
                .with_regs(0b0100_0100)
                .with_load_pc_store_lr(true)
                .with_load_or_store(Load)
            ),
          ),
          (
            0x870e,
            "strh r6, [r1, #56]",
            ThumbInstruction::LoadStoreHalfWord(load_store_half_word::Op::new()
                .with_dest_reg(R6)
                .with_load_or_store(Store)
                .with_offset(56)
                .with_base_reg(R1)
            ),
          ),
          (
            0x88bc,
            "ldrh r4, [r7, #4]",
                ThumbInstruction::LoadStoreHalfWord(load_store_half_word::Op::new()
                .with_dest_reg(R4)
                .with_load_or_store(Load)
                .with_offset(4)
                .with_base_reg(R7)
            ),
          ),
          (
            0x947b,
            "str r4, [sp, #492]",
            ThumbInstruction::SpRelativeLoadStore(sp_relative_load_store::Op::new()
              .with_load_or_store(Store)
              .with_offset(492)
              .with_reg_dest(R4)
            )
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
