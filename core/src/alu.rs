use armv4t_decoder::arm::{data_processing::OpCode, ShiftType};

use crate::registers::{psr::ProgramStatusRegister, CpuRegisters};

pub struct AluResult {
    value: u32,
    write_back: bool,
}

pub enum AluSetCpsr<'a> {
    Alter(&'a mut ProgramStatusRegister),
    Preserve(&'a ProgramStatusRegister),
}

#[derive(Debug)]
pub struct Alu {
    // TODO: add status bits which the CPU can choose to copy to cpsr
}

impl Alu {
    pub fn shift(number: u32, shift_type: ShiftType, shift_amount: u8) -> u32 {
        todo!("logical shifts")
    }

    /// Executes the given `op` on `lhs` and `rhs`, using the conditions given in the `cpsr` for operations that require it.
    /// Sets the conditions according to the result in the `cpsr` afterwards.
    /// If the operation to be performed is unary, only `rhs` is used and the value of `lhs` can be chosen freely (but is ignored).
    pub fn exec(op: OpCode, lhs: u32, rhs: u32, set_cpsr: AluSetCpsr) -> AluResult {
        use OpCode::*;
        let mut write_back = true;
        let old_carry = match set_cpsr {
            AluSetCpsr::Alter(ref cpsr) => cpsr.cbe_condition() as u32,
            AluSetCpsr::Preserve(cpsr) => cpsr.cbe_condition() as u32,
        };

        let mut new_carry = old_carry != 0;

        // TODO: set carry conditions
        let value = match op {
            And => lhs & rhs,
            Eor => lhs ^ rhs,
            Sub => {
                let val = lhs.wrapping_sub(rhs);
                new_carry = val > lhs;
                val
            }
            Rsb => rhs.wrapping_sub(lhs),
            Add => lhs.wrapping_add(rhs),
            Adc => lhs.wrapping_add(rhs) + old_carry,
            Sbc => lhs.wrapping_sub(rhs) + old_carry - 1,
            Rsc => rhs.wrapping_sub(lhs) + old_carry - 1,
            Tst => {
                write_back = false;
                lhs & rhs
            }
            Teq => {
                write_back = false;
                lhs ^ rhs
            }
            Cmp => {
                write_back = false;
                lhs.wrapping_sub(rhs)
            }

            Cmn => {
                write_back = false;
                lhs.wrapping_add(rhs)
            }
            Orr => lhs | rhs,
            Mov => rhs,
            Bic => lhs & !rhs,
            Mvn => !rhs,
        };
        if let AluSetCpsr::Alter(cpsr) = set_cpsr {
            // cpsr.set_overflow_condition();

            // TODO: Set based on carry out from barrel shifter
            cpsr.set_cbe_condition(new_carry);
            cpsr.set_zero_condition(value == 0);
            cpsr.set_neg_condition(value >> 31 == 1);
        }

        AluResult { value, write_back }
    }
}

#[cfg(test)]
mod tests {
    // TODO: test ALU, especially condition flags
}
