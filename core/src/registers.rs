use self::psr::ProgramStatusRegister;
use armv4t_decoder::common::RegisterName;

// ProgramStatusRegister related bitfields and operations
pub mod psr {
    #![allow(dead_code)] // for associated functions generated by `bitfield`
    use modular_bitfield::{bitfield, specifiers::*, BitfieldSpecifier};

    use super::Register;

    pub const DEFAULT_STACKPOINTER_USER: u32 = 0x03007F00;
    pub const DEFAULT_STACKPOINTER_IRQ: u32 = 0x03007FA0;
    pub const DEFAULT_STACKPOINTER_SUPERVISOR: u32 = 0x03007FE0;

    #[derive(Debug, BitfieldSpecifier)]
    #[bits = 1]
    pub enum InstructionMode {
        /// 16-bit special subset of instructions that map to 32-bit counterparts
        Thumb,

        /// 32-bit ARM instruction set
        Arm,
    }

    #[derive(BitfieldSpecifier, Debug)]
    #[bits = 5]
    pub enum ProcessorMode {
        User = 0b10000,
        // FastInterruptRequest. Unused by default.
        Fiq = 0b10001,

        // InterruptRequest. Mode entered when hardware triggers an interrupt.
        Irq = 0b10010,
        Supervisor = 0b10011,
        Abort = 0b10111,
        Undefined = 0b11011,
        System = 0b11111,
    }

    #[bitfield(bits = 32)]
    #[derive(Debug, Clone, Copy)]
    pub struct ProgramStatusRegister {
        pub processor_mode: ProcessorMode,

        // Thumb state indicator. If set, CPU is in Thumb state. Otherwise in normal ARM state.
        pub instr_mode: InstructionMode,

        // FIQ interrupt disable. If set, disables FIQ interrupts.
        pub fiq_interrupts_disable: bool,

        // IRQ interrupt disable. If set, disabled IRQ interrupt. Is set by default when IRQ mode is entered.
        pub irq_interrupts_disable: bool,

        // Not used
        #[skip]
        reserved: B20,

        // Overflow condition code
        pub overflow_condition: bool,

        // Carry/Borrow/Extend condition code
        pub cbe_condition: bool,

        // Zero/equal condition code
        pub zero_condition: bool,

        // Negative/less than condition code
        pub neg_condition: bool,
    }

    impl Default for ProgramStatusRegister {
        fn default() -> Self {
            Self::new()
                .with_processor_mode(ProcessorMode::User)
                .with_instr_mode(InstructionMode::Arm)
        }
    }

    impl From<u32> for ProgramStatusRegister {
        fn from(n: u32) -> Self {
            Self::from_bytes(n.to_le_bytes())
        }
    }

    impl From<Register> for ProgramStatusRegister {
        fn from(reg: Register) -> Self {
            Self::from(reg.0)
        }
    }
}

use psr::*;

#[derive(Default, Debug, Clone, Copy)]
pub(crate) struct Register(pub u32);

impl From<u32> for Register {
    fn from(n: u32) -> Self {
        Self(n)
    }
}

impl From<ProgramStatusRegister> for Register {
    fn from(psr: ProgramStatusRegister) -> Self {
        Self(u32::from_le_bytes(psr.into_bytes()))
    }
}

#[derive(Default, Debug)]
pub(crate) struct CpuRegisters {
    /// R0-R12, R13/sp, R14/lr, R15/pc
    regs: [Register; 16],
    cpsr: ProgramStatusRegister,

    /// R13_irq, R14_irq
    irq_banked_regs: [Register; 2],
    spsr_irq: ProgramStatusRegister,

    /// R8_fiq, R9_fiq, R10_fiq, R11_fiq, R12_fiq, R14_fiq
    fiq_banked_regs: [Register; 7],
    spsr_fiq: ProgramStatusRegister,

    /// R13_svc, R14_svc
    svc_banked_regs: [Register; 2],
    spsr_svc: ProgramStatusRegister,

    /// R13_abt, R14_abt
    abt_banked_regs: [Register; 2],
    spsr_abt: ProgramStatusRegister,

    /// R13_und, R14_und
    und_banked_regs: [Register; 2],
    spsr_und: ProgramStatusRegister,
}

const STACK_POINTER: usize = 13;
const LINK_REG: usize = 14;
const PROGRAM_COUNTER: usize = 15;

/// Subtract from a normal register number to get the banked IRQ one
const IRQ_OFFSET: usize = 13;

/// Subtract from a normal register number to get the banked FIQ one
const FIQ_OFFSET: usize = 8;

/// Subtract from a normal register number to get the banked SVC one
const SVC_OFFSET: usize = 13;

/// Subtract from a normal register number to get the banked ABT one
const ABT_OFFSET: usize = 13;
/// Subtract from a normal register number to get the banked UND one
const UND_OFFSET: usize = 13;

impl CpuRegisters {
    pub fn new() -> Self {
        let mut regs = [Register(0); 16];
        regs[STACK_POINTER] = DEFAULT_STACKPOINTER_USER.into();
        let [cpsr, spsr_irq, spsr_fiq, spsr_svc, spsr_abt, spsr_und] =
            [ProgramStatusRegister::default(); 6];

        let mut irq_banked_regs = [Register(0); 2];
        irq_banked_regs[STACK_POINTER - IRQ_OFFSET] = DEFAULT_STACKPOINTER_IRQ.into();

        let fiq_banked_regs = [Register(0); 7];

        let mut svc_banked_regs = [Register(0); 2];
        svc_banked_regs[STACK_POINTER - SVC_OFFSET] = DEFAULT_STACKPOINTER_SUPERVISOR.into();

        let abt_banked_regs = [Register(0); 2];
        let und_banked_regs = [Register(0); 2];
        Self {
            regs,
            cpsr,
            irq_banked_regs,
            spsr_irq,
            fiq_banked_regs,
            spsr_fiq,
            svc_banked_regs,
            spsr_svc,
            abt_banked_regs,
            spsr_abt,
            und_banked_regs,
            spsr_und,
        }
    }

    pub fn read(&self, reg: RegisterName) -> u32 {
        let idx = self.reg_to_idx(reg);
        self.regs[idx].0
    }

    pub fn read_cpsr(&self) -> ProgramStatusRegister {
        self.cpsr
    }

    pub fn write(&mut self, reg: RegisterName, value: u32) {
        let idx = self.reg_to_idx(reg);
        self.regs[idx] = Register(value);
    }

    pub fn write_cpsr(&mut self, value: ProgramStatusRegister) {
        self.cpsr = value;
    }

    pub fn get_instr_mode(&self) -> InstructionMode {
        self.cpsr.instr_mode()
    }

    pub fn switch_instr_mode(&mut self, to: InstructionMode) {
        self.cpsr.set_instr_mode(to);
    }

    fn reg_to_idx(&self, reg: RegisterName) -> usize {
        let reg = reg as usize;
        match self.cpsr.processor_mode() {
            ProcessorMode::Irq => reg - IRQ_OFFSET,
            ProcessorMode::Fiq => reg - FIQ_OFFSET,
            ProcessorMode::Supervisor => reg - SVC_OFFSET,
            ProcessorMode::Abort => reg - ABT_OFFSET,
            ProcessorMode::Undefined => reg - UND_OFFSET,
            _ => reg,
        }
    }
}
