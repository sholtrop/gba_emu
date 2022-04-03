use modular_bitfield::BitfieldSpecifier;

pub(crate) mod arm_mode {
    use super::*;
    use modular_bitfield::Specifier;

    const INSTR_SIZE: u8 = 32;
    const COND_SIZE: u8 = 4;
    const DECODER_TABLE_IDX_BITS: u8 = 12;
    const DECODER_TABLE_SIZE: usize = 1 << DECODER_TABLE_IDX_BITS;

    type DecoderTable = [Op; DECODER_TABLE_SIZE];

    const DECODER_TABLE: DecoderTable = Decoder::create_decoder_table();
    #[derive(BitfieldSpecifier)]
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

    #[derive(Clone, Copy)]
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

    pub struct Instruction {
        condition: Condition,
        op: Op,
    }

    pub struct Decoder {}

    impl Decoder {
        pub fn decode(instr: u32) -> Instruction {
            let condition = Decoder::get_condition(instr);
            // 12 bits to index into the decoder table:
            // 1111 1111 1111 1111 1111 1111 1111 1111
            //           BA98 7654           3210
            let idx = ((instr >> 16) & 0xFF0 | ((instr >> 4) & 0x00F)) as usize;
            Instruction {
                condition: Condition::Al,
                op: Op::Bx,
            }
            /*
                000101001111xxxx000000000000 -> MRS with CPSR src
                000100001111xxxx000000000000 -> MRS with SPSR_<current_mode> src
                00010x101001111100000000xxxx -> MSR with reg operand
                00110x101001111100000000xxxx -> MSR with imm operand



            */
        }

        /// Get the condition field of the ARM instruction
        fn get_condition(instr: u32) -> Condition {
            let cond_bits: u8 = (instr >> (INSTR_SIZE - COND_SIZE)).try_into().unwrap();
            Condition::from_bytes(cond_bits)
                .unwrap_or_else(|e| panic!("Invalid bit pattern for ARM condition: {e}"))
        }

        const fn create_decoder_table() -> DecoderTable {
            let mut table: DecoderTable = [Op::Unknown; DECODER_TABLE_SIZE];
            table[0b0010_1111 | 0b0001] = Op::Bx;

            table
        }
    }
}

pub(crate) mod thumb_mode {
    pub enum Instruction {}

    pub struct Decoder {}

    impl Decoder {
        pub fn decode(instr: u32) -> Instruction {
            match instr {
                _ => todo!(),
            }
        }
    }
}
