use modular_bitfield::BitfieldSpecifier;

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

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Op {
    Adc, // Add with carry
    Add, // Add
    And, // And
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
    Mla, // Multiply Accumulate
    Mov, // Move register or constant
    Mrc, // Move from coprocessor register to CPU register
    Mrs, // Move PSR status/flags to register
    Msr, // Move register to PSR status/flags
    Mul, // Multiply
    Mvn, // Move negative register
    Orr, // Or
    Rsb, // Reverse subtract
    Rsc, // Reverse subtract with carry
    Sbc, // Subtract with carry
    Stc, // Store coprocessor register to memory
    Stm, // Store multiple
    Str, // Store register to memory
    Sub, // Subtract
    Swi, // Software interrupt
    Swp, // Swap register with memory
    Teq, // Test bitwise equality
    Tst, // Test bits
    Unknown,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Instruction {
    condition: Condition,
    op: Op,
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
    Unknown,
}

pub struct Decoder {}

impl Decoder {
    pub fn decode(instr: u32) -> Instruction {
        let condition = Decoder::get_condition(instr);

        let idx = Decoder::get_index(instr);

        Instruction {
            condition: Condition::Al,
            op: Op::Unknown,
        }
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

    /// Get the condition field of the ARM instruction
    fn get_condition(instr: u32) -> Condition {
        let cond_bits: u8 = (instr >> (INSTR_SIZE - COND_SIZE)).try_into().unwrap();
        Condition::from_bytes(cond_bits)
            .unwrap_or_else(|e| panic!("Invalid bit pattern for ARM condition: {e}"))
    }

    #[inline]
    /// Return the 12 bit index for the [DecoderTable]:
    /// ```
    /// |   28    24    20    16    12     8     4     0
    /// |-----|-----|-----|-----|-----|-----|-----|-----|
    /// |1111 |1111 |1111 |1111 |1111 |1111 |1111 |1111 |
    /// |-----|-----|-----|-----|-----|-----|-----|-----|
    /// |     |BA98 |7654 |     |     |3210 |     |     |
    /// ```
    fn get_index(instr: u32) -> usize {
        ((instr >> 16) & 0xFF0 | ((instr >> 4) & 0x00F)) as usize
    }

    fn is_branch_and_branch_exchange(instr: u32) -> bool {
        let branchAndExchangeFormat = 0b0000_0001_0010_1111_1111_1111_0001_0000;

        let format_mask = 0b0000_1111_1111_1111_1111_1111_1111_0000;

        let extracted_format = instr & format_mask;

        extracted_format == branchAndExchangeFormat
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
        assert_eq!(Decoder::decode(0x1F000000).op, Op::Swi);
        // SWILE 0
        assert_eq!(Decoder::decode(0xDF000000).op, Op::Swi);
        // SWIVC 0
        assert_eq!(Decoder::decode(0x7F000000).op, Op::Swi);
        // SWIGE 0
        assert_eq!(Decoder::decode(0xAFFFFFFF).op, Op::Swi);
    }
}
