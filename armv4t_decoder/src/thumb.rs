use bitvec::order::Lsb0;
use bitvec::view::BitView;
use modular_bitfield::{bitfield, specifiers::*, BitfieldSpecifier};
use std::fmt::{Debug, Display};

use crate::{
    common::{LoadOrStore::*, RegisterName},
    thumb::load_address::LoadSource,
};

pub const THUMB_INSTR_SIZE_BITS: u8 = 16;
pub const THUMB_INSTR_SIZE_BYTES: u8 = THUMB_INSTR_SIZE_BITS / 8;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Operands {
    One(String),
    Two(String, String),
    Three(String, String, String),
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

#[bitfield(bits = 8)]
#[derive(Clone, Copy, Debug, PartialEq, Eq, BitfieldSpecifier)]
/// Unsigned 10-bit constant that is 4-byte aligned (bit 0-1 are 0), thus stored in 8 bits
pub struct Offset8 {
    val: B8,
}

impl Offset8 {
    pub fn value(&self) -> u16 {
        (self.val() as u16) << 2
    }
}

impl From<u16> for Offset8 {
    fn from(value: u16) -> Self {
        Offset8::new().with_val((value >> 2) as u8)
    }
}

impl Display for Offset8 {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
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
    LoadAddress(load_address::Op),
    ConditionalBranch(conditional_branch::Op),
    LoadStoreImmOffset(load_store_imm_offset::Op),
    LoadStoreRegOffset(load_store_reg_offset::Op),
    LoadStoreSignExtHalfwordByte(load_store_sign_ext_byte_halfword::Op),
    PcRelativeLoad(pc_relative_load::Op),
    HiRegOpsBranchExchange(hi_reg_ops_branch_exchange::Op),
    AluOps(alu_ops::Op),
    MoveCompAddSubtractImm(move_comp_add_sub_imm::Op),
    AddSub(add_sub::Op),
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
        } else if load_address::is_load_address(instr) {
            LoadAddress(load_address::parse(instr))
        } else if load_store_imm_offset::is_load_store_imm_offset(instr) {
            LoadStoreImmOffset(load_store_imm_offset::parse(instr))
        } else if load_store_reg_offset::is_load_store_reg_offset(instr) {
            LoadStoreRegOffset(load_store_reg_offset::parse(instr))
        } else if load_store_sign_ext_byte_halfword::is_load_store_sign_ext_byte_halfword(instr) {
            LoadStoreSignExtHalfwordByte(load_store_sign_ext_byte_halfword::parse(instr))
        } else if pc_relative_load::is_pc_relative_load(instr) {
            PcRelativeLoad(pc_relative_load::parse(instr))
        } else if hi_reg_ops_branch_exchange::is_hi_reg_ops_branch_exchange(instr) {
            HiRegOpsBranchExchange(hi_reg_ops_branch_exchange::parse(instr))
        } else if alu_ops::is_alu_ops(instr) {
            AluOps(alu_ops::parse(instr))
        } else if move_comp_add_sub_imm::is_move_comp_add_sub_imm(instr) {
            MoveCompAddSubtractImm(move_comp_add_sub_imm::parse(instr))
        } else if add_sub::is_add_sub(instr) {
            AddSub(add_sub::parse(instr))
        } else if move_shifted_reg::is_move_shifted_reg(instr) {
            MoveShiftedRegister(move_shifted_reg::parse(instr))
        } else {
            todo!("Unimplemented THUMB instruction {instr}")
        }
    }

    fn get_operands(&self) -> Operands {
        use hi_reg_ops_branch_exchange::OpCode::*;
        use Operands::*;

        match self {
            SoftwareInterrupt(op) => One(op.comment().to_string()),
            UnconditionalBranch(op) => One(op.offset11().to_string()),
            MoveShiftedRegister(op) => Three(
                op.dest_reg().to_string(),
                op.src_reg().to_string(),
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
                let rd = op.dest_reg().to_string();
                let op1 = format!("[sp, #{}]", op.offset());
                Two(rd, op1)
            }
            LoadAddress(op) => {
                let op2 = match op.src() {
                    LoadSource::Pc => "pc".into(),
                    LoadSource::Sp => "sp".into(),
                };
                Three(op.dest_reg().to_string(), op2, op.word8().to_string())
            }
            LoadStoreImmOffset(op) => {
                let rd = op.dest_reg().to_string();
                let op1 = format!("[{}, #{}]", op.base_reg(), op.offset());
                Two(rd, op1)
            }
            LoadStoreRegOffset(op) => {
                let rd = op.dest_reg().to_string();
                let op1 = format!("[{}, {}]", op.base_reg(), op.offset_reg());
                Two(rd, op1)
            }
            LoadStoreSignExtHalfwordByte(op) => {
                let rd = op.dest_reg().to_string();
                let op1 = format!("[{}, {}]", op.base_reg(), op.offset_reg());
                Two(rd, op1)
            }
            PcRelativeLoad(op) => {
                let rd = op.dest_reg().to_string();
                let op1 = format!("[pc, {}]", op.word8());
                Two(rd, op1)
            }
            HiRegOpsBranchExchange(op) => {
                let dst = u8::from(op.dest_reg());
                let dst = if op.hi_op1() {
                    RegisterName::from(dst + 8)
                } else {
                    RegisterName::from(dst)
                };
                let src = u8::from(op.src_reg());
                let src = if op.hi_op2() {
                    RegisterName::from(src + 8)
                } else {
                    RegisterName::from(src)
                };
                if op.opcode() == Bx {
                    One(src.to_string())
                } else {
                    Two(dst.to_string(), src.to_string())
                }
            }
            AluOps(op) => Two(op.dest_reg().to_string(), op.src_reg().to_string()),
            MoveCompAddSubtractImm(op) => {
                let rd = op.dest_reg().to_string();
                let op1 = format!("#{}", op.imm8());
                Two(rd, op1)
            }
            AddSub(op) => Three(
                op.dest_reg().to_string(),
                op.src_reg().to_string(),
                if op.is_imm() {
                    format!("#{}", op.imm3())
                } else {
                    op.offset_reg().to_string()
                },
            ),
            ConditionalBranch(op) => One(op.offset8().to_string()),
        }
    }

    fn get_mnemonic(&self) -> String {
        use crate::common::ByteOrWord::*;
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
            LoadAddress(_) => "add".into(),
            AddOffsetToStackPointer(_) => "add".into(),
            LoadStoreImmOffset(op) => match (op.load_or_store(), op.byte_or_word()) {
                (Store, Word) => "str".into(),
                (Store, Byte) => "strb".into(),
                (Load, Word) => "ldr".into(),
                (Load, Byte) => "ldrb".into(),
            },
            LoadStoreRegOffset(op) => match (op.load_or_store(), op.byte_or_word()) {
                (Store, Word) => "str".into(),
                (Store, Byte) => "strb".into(),
                (Load, Word) => "ldr".into(),
                (Load, Byte) => "ldrb".into(),
            },
            LoadStoreSignExtHalfwordByte(op) => match (op.sign_extend(), op.h_flag()) {
                (false, false) => "strh".into(),
                (false, true) => "ldrh".into(),
                (true, false) => "ldsb".into(),
                (true, true) => "ldsh".into(),
            },
            PcRelativeLoad(_) => "ldr".into(),
            AluOps(op) => op.opcode().to_string(),
            MoveCompAddSubtractImm(op) => op.opcode().to_string(),
            HiRegOpsBranchExchange(op) => op.opcode().to_string(),
            AddSub(op) => op.opcode().to_string(),
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

impl From<u8> for RegisterName3Bit {
    fn from(value: u8) -> Self {
        use RegisterName3Bit::*;
        match value {
            0 => R0,
            1 => R1,
            2 => R2,
            3 => R3,
            4 => R4,
            5 => R5,
            6 => R6,
            7 => R7,
            _ => panic!("Invalid register 3-bit value: {}", value),
        }
    }
}

impl From<RegisterName3Bit> for u8 {
    fn from(reg_list: RegisterName3Bit) -> u8 {
        use RegisterName3Bit::*;
        match reg_list {
            R0 => 0,
            R1 => 1,
            R2 => 2,
            R3 => 3,
            R4 => 4,
            R5 => 5,
            R6 => 6,
            R7 => 7,
        }
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
        pub imm7: B7,
        pub sign: Signedness,
        #[skip]
        ignored: B8,
    }

    impl Op {
        pub fn get_imm(&self) -> i16 {
            let val = (self.imm7() as i16) << 2;
            if self.sign() == Signed {
                -val
            } else {
                val
            }
        }
    }

    impl PartialEq for Op {
        fn eq(&self, other: &Self) -> bool {
            self.imm7() == other.imm7() && self.sign() == other.sign()
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
        pub dest_reg: RegisterName3Bit,
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
                && self.dest_reg() == other.dest_reg()
                && self.load_or_store() == other.load_or_store()
        }
    }
}

pub mod load_address {
    use super::*;

    pub fn is_load_address(opcode: u16) -> bool {
        let load_address_format: u16 = 0b1010_0000_0000_0000;
        let format_mask: u16 = 0b1111_0000_0000_0000;
        let extracted_format = opcode & format_mask;
        extracted_format == load_address_format
    }

    pub fn parse(instr: u16) -> Op {
        Op::from_bytes(instr.to_le_bytes())
    }

    #[derive(Clone, Copy, Debug, PartialEq, Eq, BitfieldSpecifier)]
    pub enum LoadSource {
        Pc,
        Sp,
    }

    impl Display for LoadSource {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                LoadSource::Pc => write!(f, "pc"),
                LoadSource::Sp => write!(f, "sp"),
            }
        }
    }

    #[bitfield(bits = 16)]
    #[derive(Clone, Copy, Debug, Eq)]
    pub struct Op {
        pub word8: Offset8,
        pub dest_reg: RegisterName3Bit,
        pub src: LoadSource,
        #[skip]
        ignored: B4,
    }

    impl PartialEq for Op {
        fn eq(&self, other: &Self) -> bool {
            self.word8() == other.word8()
                && self.dest_reg() == other.dest_reg()
                && self.src() == other.src()
        }
    }
}

pub mod load_store_imm_offset {
    use super::*;
    use crate::common::{ByteOrWord, ByteOrWord::*, LoadOrStore};

    pub fn is_load_store_imm_offset(opcode: u16) -> bool {
        let load_store_imm_offset_format: u16 = 0b0110_0000_0000_0000;
        let format_mask: u16 = 0b1110_0000_0000_0000;
        let extracted_format = opcode & format_mask;
        extracted_format == load_store_imm_offset_format
    }

    pub fn parse(instr: u16) -> Op {
        Op::from_bytes(instr.to_le_bytes())
    }

    #[bitfield(bits = 16)]
    #[derive(Clone, Copy, Debug, Eq)]
    pub struct Op {
        pub dest_reg: RegisterName3Bit,
        pub base_reg: RegisterName3Bit,
        // 5-bit (Byte) or 7-bit (Word) offset
        pub offset5: B5,
        pub load_or_store: LoadOrStore,
        pub byte_or_word: ByteOrWord,
        #[skip]
        ignore: B3,
    }

    impl Op {
        pub fn offset(&self) -> u8 {
            match self.byte_or_word() {
                Byte => self.offset5(),
                Word => self.offset5() << 2,
            }
        }

        pub fn with_offset(&self, offset: u8) -> Self {
            let offset = match self.byte_or_word() {
                Byte => offset,
                Word => offset >> 2,
            };
            self.with_offset5(offset)
        }

        pub fn set_offset(&mut self, offset: u8) {
            let offset = match self.byte_or_word() {
                Byte => offset,
                Word => offset >> 2,
            };
            self.set_offset5(offset);
        }
    }

    impl PartialEq for Op {
        fn eq(&self, other: &Self) -> bool {
            self.dest_reg() == other.dest_reg()
                && self.base_reg() == other.base_reg()
                && self.offset5() == other.offset5()
                && self.load_or_store() == other.load_or_store()
                && self.byte_or_word() == other.byte_or_word()
        }
    }
}

pub mod load_store_reg_offset {
    use super::*;
    use crate::common::{ByteOrWord, LoadOrStore};

    use super::RegisterName3Bit;

    pub fn is_load_store_reg_offset(opcode: u16) -> bool {
        let load_store_reg_offset_format: u16 = 0b0101_0000_0000_0000;
        let format_mask: u16 = 0b1111_0010_0000_0000;
        let extracted_format = opcode & format_mask;
        extracted_format == load_store_reg_offset_format
    }

    pub fn parse(instr: u16) -> Op {
        Op::from_bytes(instr.to_le_bytes())
    }

    #[bitfield(bits = 16)]
    #[derive(Clone, Copy, Debug, Eq)]
    pub struct Op {
        pub dest_reg: RegisterName3Bit,
        pub base_reg: RegisterName3Bit,
        pub offset_reg: RegisterName3Bit,
        #[skip]
        ignored: B1,
        pub byte_or_word: ByteOrWord,
        pub load_or_store: LoadOrStore,
        #[skip]
        ignored: B4,
    }

    impl PartialEq for Op {
        fn eq(&self, other: &Self) -> bool {
            self.dest_reg() == other.dest_reg()
                && self.base_reg() == other.base_reg()
                && self.offset_reg() == other.offset_reg()
                && self.byte_or_word() == other.byte_or_word()
                && self.load_or_store() == other.load_or_store()
        }
    }
}

pub mod load_store_sign_ext_byte_halfword {
    use crate::common::Signedness;

    use super::*;

    pub fn is_load_store_sign_ext_byte_halfword(opcode: u16) -> bool {
        let load_store_sign_ext_byte_halfword_format: u16 = 0b0101_0010_0000_0000;
        let format_mask: u16 = 0b1111_0010_0000_0000;
        let extracted_format = opcode & format_mask;
        extracted_format == load_store_sign_ext_byte_halfword_format
    }

    pub fn parse(instr: u16) -> Op {
        Op::from_bytes(instr.to_le_bytes())
    }

    #[bitfield(bits = 16)]
    #[derive(Clone, Copy, Debug, Eq)]
    pub struct Op {
        pub dest_reg: RegisterName3Bit,
        pub base_reg: RegisterName3Bit,
        pub offset_reg: RegisterName3Bit,
        #[skip]
        ignored: B1,
        pub sign_extend: bool,
        pub h_flag: bool,
        #[skip]
        ignored: B4,
    }

    impl PartialEq for Op {
        fn eq(&self, other: &Self) -> bool {
            self.dest_reg() == other.dest_reg()
                && self.base_reg() == other.base_reg()
                && self.offset_reg() == other.offset_reg()
                && self.sign_extend() == other.sign_extend()
                && self.h_flag() == other.h_flag()
        }
    }
}

pub mod pc_relative_load {
    use super::*;

    pub fn is_pc_relative_load(opcode: u16) -> bool {
        let pc_relative_load_format: u16 = 0b0100_1000_0000_0000;
        let format_mask: u16 = 0b1111_1000_0000_0000;
        let extracted_format = opcode & format_mask;
        extracted_format == pc_relative_load_format
    }

    pub fn parse(instr: u16) -> Op {
        Op::from_bytes(instr.to_le_bytes())
    }

    #[bitfield(bits = 16)]
    #[derive(Clone, Copy, Debug, Eq)]
    pub struct Op {
        pub word8: Offset8,
        pub dest_reg: RegisterName3Bit,
        #[skip]
        ignored: B5,
    }

    impl PartialEq for Op {
        fn eq(&self, other: &Self) -> bool {
            self.word8() == other.word8() && self.dest_reg() == other.dest_reg()
        }
    }
}

pub mod hi_reg_ops_branch_exchange {
    use super::*;

    pub fn is_hi_reg_ops_branch_exchange(opcode: u16) -> bool {
        let hi_reg_ops_branch_exchange_format: u16 = 0b0100_0100_0000_0000;
        let format_mask: u16 = 0b1111_1100_0000_0000;
        let extracted_format = opcode & format_mask;
        extracted_format == hi_reg_ops_branch_exchange_format
    }

    pub fn parse(instr: u16) -> Op {
        Op::from_bytes(instr.to_le_bytes())
    }

    #[derive(Clone, Copy, PartialEq, Eq, Debug, BitfieldSpecifier)]
    #[bits = 2]
    pub enum OpCode {
        Add = 0b00,
        Cmp = 0b01,
        Mov = 0b10,
        Bx = 0b11,
    }

    impl Display for OpCode {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                OpCode::Add => write!(f, "add"),
                OpCode::Cmp => write!(f, "cmp"),
                OpCode::Mov => write!(f, "mov"),
                OpCode::Bx => write!(f, "bx"),
            }
        }
    }

    #[bitfield(bits = 16)]
    #[derive(Clone, Copy, Debug, Eq)]
    pub struct Op {
        pub dest_reg: RegisterName3Bit,
        pub src_reg: RegisterName3Bit,
        pub hi_op2: bool,
        pub hi_op1: bool,
        pub opcode: OpCode,
        #[skip]
        ignored: B6,
    }

    impl PartialEq for Op {
        fn eq(&self, other: &Self) -> bool {
            self.dest_reg() == other.dest_reg()
                && self.src_reg() == other.src_reg()
                && self.hi_op1() == other.hi_op1()
                && self.hi_op2() == other.hi_op2()
                && self.opcode() == other.opcode()
        }
    }
}

pub mod alu_ops {
    use super::*;

    pub fn is_alu_ops(opcode: u16) -> bool {
        let alu_ops_format: u16 = 0b0100_0000_0000_0000;
        let format_mask: u16 = 0b1111_1100_0000_0000;
        let extracted_format = opcode & format_mask;
        extracted_format == alu_ops_format
    }

    pub fn parse(instr: u16) -> Op {
        Op::from_bytes(instr.to_le_bytes())
    }

    #[derive(Clone, Copy, PartialEq, Eq, Debug, BitfieldSpecifier)]
    #[bits = 4]
    pub enum OpCode {
        And,
        Eor,
        Lsl,
        Lsr,
        Asr,
        Adc,
        Sbc,
        Ror,
        Tst,
        Neg,
        Cmp,
        Cmn,
        Orr,
        Mul,
        Bic,
        Mvn,
    }

    impl Display for OpCode {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(
                f,
                "{}s",
                match self {
                    OpCode::And => "and",
                    OpCode::Eor => "eor",
                    OpCode::Lsl => "lsl",
                    OpCode::Lsr => "lsr",
                    OpCode::Asr => "asr",
                    OpCode::Adc => "adc",
                    OpCode::Sbc => "sbc",
                    OpCode::Ror => "ror",
                    OpCode::Tst => "tst",
                    OpCode::Neg => "neg",
                    OpCode::Cmp => "cmp",
                    OpCode::Cmn => "cmn",
                    OpCode::Orr => "orr",
                    OpCode::Mul => "mul",
                    OpCode::Bic => "bic",
                    OpCode::Mvn => "mvn",
                }
            )
        }
    }

    #[bitfield(bits = 16)]
    #[derive(Clone, Copy, Debug, Eq)]
    pub struct Op {
        pub dest_reg: RegisterName3Bit,
        pub src_reg: RegisterName3Bit,
        pub opcode: OpCode,
        #[skip]
        ignored: B6,
    }

    impl PartialEq for Op {
        fn eq(&self, other: &Self) -> bool {
            self.dest_reg() == other.dest_reg()
                && self.src_reg() == other.src_reg()
                && self.opcode() == other.opcode()
        }
    }
}

pub mod move_comp_add_sub_imm {
    use super::*;

    pub fn is_move_comp_add_sub_imm(opcode: u16) -> bool {
        let move_compare_add_sub_imm_format: u16 = 0b0010_0000_0000_0000;
        let format_mask: u16 = 0b1110_0000_0000_0000;
        let extracted_format = opcode & format_mask;
        extracted_format == move_compare_add_sub_imm_format
    }

    pub fn parse(instr: u16) -> Op {
        Op::from_bytes(instr.to_le_bytes())
    }

    #[derive(Clone, Copy, PartialEq, Eq, Debug, BitfieldSpecifier)]
    #[bits = 2]
    pub enum OpCode {
        Mov,
        Cmp,
        Add,
        Sub,
    }

    impl Display for OpCode {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(
                f,
                "{}s",
                match self {
                    OpCode::Mov => "mov",
                    OpCode::Cmp => "cmp",
                    OpCode::Add => "add",
                    OpCode::Sub => "sub",
                }
            )
        }
    }

    #[bitfield(bits = 16)]
    #[derive(Clone, Copy, Eq, Debug, BitfieldSpecifier)]
    pub struct Op {
        pub imm8: u8,
        pub dest_reg: RegisterName3Bit,
        pub opcode: OpCode,
        #[skip]
        ignored: B3,
    }

    impl PartialEq for Op {
        fn eq(&self, other: &Self) -> bool {
            self.imm8() == other.imm8()
                && self.dest_reg() == other.dest_reg()
                && self.opcode() == other.opcode()
        }
    }
}

pub mod add_sub {
    use super::*;

    pub fn is_add_sub(opcode: u16) -> bool {
        let add_sub_format: u16 = 0b0001_1000_0000_0000;
        let format_mask: u16 = 0b1111_1000_0000_0000;
        let extracted_format = opcode & format_mask;
        extracted_format == add_sub_format
    }

    pub fn parse(instr: u16) -> Op {
        Op::from_bytes(instr.to_le_bytes())
    }

    #[derive(Clone, Copy, PartialEq, Eq, Debug, BitfieldSpecifier)]
    #[bits = 1]
    pub enum OpCode {
        Add,
        Sub,
    }

    impl Display for OpCode {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(
                f,
                "{}s",
                match self {
                    OpCode::Add => "add",
                    OpCode::Sub => "sub",
                }
            )
        }
    }

    #[bitfield(bits = 16)]
    #[derive(Clone, Copy, Eq, Debug, BitfieldSpecifier)]
    pub struct Op {
        pub dest_reg: RegisterName3Bit,
        pub src_reg: RegisterName3Bit,
        /// Either a [RegisterName3Bit] if `is_imm` == 0, or a 3-bit offset otherwise.
        pub reg_or_imm3: B3,
        pub opcode: OpCode,
        pub is_imm: bool,
        #[skip]
        ignored: B5,
    }

    impl Op {
        pub fn imm3(&self) -> u8 {
            self.reg_or_imm3() as u8
        }

        pub fn offset_reg(&self) -> RegisterName3Bit {
            self.reg_or_imm3().into()
        }
    }

    impl PartialEq for Op {
        fn eq(&self, other: &Self) -> bool {
            self.dest_reg() == other.dest_reg()
                && self.src_reg() == other.src_reg()
                && self.reg_or_imm3() == other.reg_or_imm3()
                && self.opcode() == other.opcode()
                && self.is_imm() == other.is_imm()
        }
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
        pub dest_reg: RegisterName3Bit,
        pub src_reg: RegisterName3Bit,
        pub offset: Operand5Bit,
        pub op: ShiftOp,
        #[skip]
        ignored: B3,
    }
    impl PartialEq for Op {
        fn eq(&self, other: &Self) -> bool {
            self.dest_reg() == other.dest_reg()
                && self.src_reg() == other.src_reg()
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
    use super::hi_reg_ops_branch_exchange::OpCode::*;
    use super::load_address::LoadSource::*;
    use super::long_branch_with_link::OffsetHighLow::*;
    use super::RegisterName3Bit::*;
    use super::*;
    use crate::common::ByteOrWord::*;
    use crate::common::Condition::*;
    use crate::common::Signedness::*;
    use lazy_static::lazy_static;

    lazy_static! {
    // Instruction hex, assembly string, expected decoded instruction
        static ref TEST_INSTRUCTIONS: [(u16, &'static str, ThumbInstruction); 37] = [
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
              .with_dest_reg(RegisterName3Bit::R1)
              .with_src_reg(RegisterName3Bit::R2)
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
              .with_imm7((268_u16 >> 2) as u8)
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
              .with_dest_reg(R4)
            )
          ),
          (
            0x9a01,
            "ldr r2, [sp, #4]",
            ThumbInstruction::SpRelativeLoadStore(sp_relative_load_store::Op::new()
              .with_load_or_store(Load)
              .with_offset(4)
              .with_dest_reg(R2)
            )
          ),
          (
            0xa28f,
            "add r2, pc, #572",
            ThumbInstruction::LoadAddress(load_address::Op::new()
              .with_dest_reg(R2)
              .with_src(Pc)
              .with_word8(572.into())
            )
          ),
          (
            0xae35,
            "add r6, sp, #212",
            ThumbInstruction::LoadAddress(load_address::Op::new()
              .with_dest_reg(R6)
              .with_src(Sp)
              .with_word8(212.into())
            )
          ),
          (
            0x6f6a,
            "ldr r2, [r5, #116]",
            ThumbInstruction::LoadStoreImmOffset(load_store_imm_offset::Op::new()
              .with_load_or_store(Load)
              .with_offset(116)
              .with_base_reg(R5)
              .with_dest_reg(R2)
            )
          ),
          (
            0x7341,
            "strb r1, [r0, #13]",
            ThumbInstruction::LoadStoreImmOffset(load_store_imm_offset::Op::new()
              .with_load_or_store(Store)
              .with_byte_or_word(Byte)
              .with_offset(13)
              .with_base_reg(R0)
              .with_dest_reg(R1)
            )
          ),
          (
            0x5193,
            "str r3, [r2, r6]",
            ThumbInstruction::LoadStoreRegOffset(load_store_reg_offset::Op::new()
              .with_load_or_store(Store)
              .with_base_reg(R2)
              .with_dest_reg(R3)
              .with_offset_reg(R6)
            )
          ),
          (
            0x5dc2,
            "ldrb r2, [r0, r7]",
            ThumbInstruction::LoadStoreRegOffset(load_store_reg_offset::Op::new()
              .with_load_or_store(Load)
              .with_base_reg(R0)
              .with_dest_reg(R2)
              .with_offset_reg(R7)
              .with_byte_or_word(Byte)
            )
          ),
          (
            0x521c,
            "strh r4, [r3, r0]",
            ThumbInstruction::LoadStoreSignExtHalfwordByte(load_store_sign_ext_byte_halfword::Op::new()
              .with_dest_reg(R4)
              .with_base_reg(R3)
              .with_offset_reg(R0)
              .with_sign_extend(false)
              .with_h_flag(false)

            )
          ),
          (
            0x567a,
            "ldsb r2, [r7, r1]",
            ThumbInstruction::LoadStoreSignExtHalfwordByte(load_store_sign_ext_byte_halfword::Op::new()
              .with_dest_reg(R2)
              .with_base_reg(R7)
              .with_offset_reg(R1)
              .with_sign_extend(true)
              .with_h_flag(false)
            )
          ),
          (
            0x5ea3,
            "ldsh r3, [r4, r2]",
            ThumbInstruction::LoadStoreSignExtHalfwordByte(load_store_sign_ext_byte_halfword::Op::new()
              .with_dest_reg(R3)
              .with_base_reg(R4)
              .with_offset_reg(R2)
              .with_sign_extend(true)
              .with_h_flag(true)
            )
          ),
          (
            0x4bd3,
            "ldr r3, [pc, #844]",
            ThumbInstruction::PcRelativeLoad(pc_relative_load::Op::new()
              .with_word8(844.into())
              .with_dest_reg(R3)
            )
          ),
          (
            0x44af,
            "add r15, r5",
            ThumbInstruction::HiRegOpsBranchExchange(hi_reg_ops_branch_exchange::Op::new()
              .with_hi_op1(true)
              .with_dest_reg(R7)
              .with_hi_op2(false)
              .with_src_reg(R5)
              .with_opcode(Add)
            )
          ),
          (
            0x4564,
            "cmp r4, r12",
            ThumbInstruction::HiRegOpsBranchExchange(hi_reg_ops_branch_exchange::Op::new()
              .with_hi_op1(false)
              .with_dest_reg(R4)
              .with_hi_op2(true)
              .with_src_reg(R4)
              .with_opcode(Cmp)
            )
          ),
          (
            0x4758,
            "bx r11",
            ThumbInstruction::HiRegOpsBranchExchange(hi_reg_ops_branch_exchange::Op::new()
              .with_hi_op2(true)
              .with_src_reg(R3)
              .with_opcode(Bx)
            )
          ),
          (
            0x46f7,
            "mov r15, r14",
            ThumbInstruction::HiRegOpsBranchExchange(hi_reg_ops_branch_exchange::Op::new()
              .with_hi_op1(true)
              .with_dest_reg(R7)
              .with_hi_op2(true)
              .with_src_reg(R6)
              .with_opcode(Mov)
            )
          ),
          (
            0x4063,
            "eors r3, r4",
            ThumbInstruction::AluOps(alu_ops::Op::new()
              .with_dest_reg(R3)
              .with_src_reg(R4)
              .with_opcode(alu_ops::OpCode::Eor)
            )
          ),
          (
            0x41a3,
            "sbcs r3, r4",
            ThumbInstruction::AluOps(alu_ops::Op::new()
              .with_dest_reg(R3)
              .with_src_reg(R4)
              .with_opcode(alu_ops::OpCode::Sbc)
            )
          ),
          (
            0x4240,
            "negs r0, r0",
            ThumbInstruction::AluOps(alu_ops::Op::new()
              .with_dest_reg(R0)
              .with_src_reg(R0)
              .with_opcode(alu_ops::OpCode::Neg)
            )
          ),
          (
            0x2080,
            "movs r0, #128",
            ThumbInstruction::MoveCompAddSubtractImm(move_comp_add_sub_imm::Op::new()
              .with_opcode(move_comp_add_sub_imm::OpCode::Mov)
              .with_dest_reg(R0)
              .with_imm8(128)
            )
          ),
          (
            0x3e91,
            "subs r6, #145",
            ThumbInstruction::MoveCompAddSubtractImm(move_comp_add_sub_imm::Op::new()
              .with_opcode(move_comp_add_sub_imm::OpCode::Sub)
              .with_dest_reg(R6)
              .with_imm8(145)
            )
          ),
          (
            0x1918,
            "adds r0, r3, r4",
            ThumbInstruction::AddSub(add_sub::Op::new()
              .with_dest_reg(R0)
              .with_src_reg(R3)
              .with_is_imm(false)
              .with_reg_or_imm3(R4.into())
              .with_opcode(add_sub::OpCode::Add)
            )
          ),
          (
            0x1f96,
            "subs r6, r2, #6",
            ThumbInstruction::AddSub(add_sub::Op::new()
              .with_dest_reg(R6)
              .with_src_reg(R2)
              .with_is_imm(true)
              .with_reg_or_imm3(6)
              .with_opcode(add_sub::OpCode::Sub)
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
