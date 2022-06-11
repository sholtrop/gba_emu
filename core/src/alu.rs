use armv4t_decoder::arm::{data_processing::OpCode, ShiftType};
use bitvec::store::BitStore;

use crate::registers::psr::ProgramStatusRegister;

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
    /// If `cpsr` is [AluSetCpsr::Alter], the condition codes in the given [ProgramStatusRegister] will be altered based on the result of the calculation.
    /// If the operation to be performed is unary, only `rhs` is used and the value of `lhs` can be chosen freely (but is ignored).
    pub fn exec(op: OpCode, lhs: u32, rhs: u32, cpsr: AluSetCpsr) -> AluResult {
        use OpCode::*;
        let mut write_back = true;
        let (old_carry, old_overflow) = match cpsr {
            AluSetCpsr::Alter(ref cpsr) => (
                cpsr.cbe_condition() as u32,
                cpsr.overflow_condition() as u32,
            ),
            AluSetCpsr::Preserve(cpsr) => (
                cpsr.cbe_condition() as u32,
                cpsr.overflow_condition() as u32,
            ),
        };

        let mut new_carry = old_carry != 0;
        let mut new_overflow = old_overflow != 0;

        // TODO: set carry conditions
        let value = match op {
            And => lhs & rhs,
            Eor => lhs ^ rhs,
            Sub => {
                let (val, carry) = lhs.overflowing_sub(rhs);
                new_carry = carry;
                new_overflow = Alu::set_overflow_bit_sub(lhs, rhs, val);
                val
            }
            Rsb => {
                let (val, carry) = rhs.overflowing_sub(lhs);
                new_carry = carry;
                new_overflow = Alu::set_overflow_bit_sub(lhs, rhs, val);
                val
            }
            Add => {
                let (val, carry) = lhs.overflowing_add(rhs);
                new_carry = carry;
                new_overflow = Alu::set_overflow_bit_add(lhs, rhs, val);
                val
            }
            Adc => {
                let val = lhs.wrapping_add(rhs);
                let (val, carry) = val.overflowing_add(old_carry);
                new_carry = carry;
                new_overflow = Alu::set_overflow_bit_add(lhs, rhs, val);
                val
            }
            Sbc => {
                let val = lhs.wrapping_sub(rhs);
                let (val, carry) = val.overflowing_add(old_carry - 1);
                new_carry = carry;
                new_overflow = Alu::set_overflow_bit_add(lhs, rhs, val);
                val
            }
            Rsc => {
                let val = rhs.wrapping_sub(lhs);
                let (val, carry) = val.overflowing_add(old_carry - 1);
                new_carry = carry;
                new_overflow = Alu::set_overflow_bit_add(lhs, rhs, val);
                val
            }
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
                let (val, carry) = lhs.overflowing_sub(rhs);
                new_carry = carry;
                new_overflow = Alu::set_overflow_bit_sub(lhs, rhs, val);
                val
            }
            Cmn => {
                write_back = false;
                let (val, carry) = lhs.overflowing_add(rhs);
                new_carry = carry;
                new_overflow = Alu::set_overflow_bit_add(lhs, rhs, val);
                val
            }
            Orr => lhs | rhs,
            Mov => rhs,
            Bic => lhs & !rhs,
            Mvn => !rhs,
        };

        if let AluSetCpsr::Alter(cpsr) = cpsr {
            cpsr.set_overflow_condition(new_overflow);
            cpsr.set_cbe_condition(new_carry);
            cpsr.set_zero_condition(value == 0);
            cpsr.set_neg_condition(value >> 31 == 1);
        }

        AluResult { value, write_back }
    }

    fn set_overflow_bit_add(lhs: u32, rhs: u32, result: u32) -> bool {
        let sign_lhs = lhs >> 31;
        let sign_rhs = rhs >> 31;
        let sign_res = result >> 31;
        // Case 1: Adding two numbers with the sign bit off, but the result has the sign bit on
        let case1 = (sign_lhs & sign_rhs == 0) && sign_res == 1;
        // Case 2: Adding two numbers with the sign bit on, but the result has the sign bit off
        let case2 = (sign_lhs & sign_rhs == 1) && sign_res == 0;
        case1 || case2
    }
    fn set_overflow_bit_sub(lhs: u32, rhs: u32, result: u32) -> bool {
        let sign_lhs = lhs >> 31;
        let sign_rhs = rhs >> 31;
        let sign_res = result >> 31;
        // Case 1: Subtracting a negative (= adding a positive) from a positive created a negative
        let case1 = sign_lhs == 0 && (sign_rhs & sign_res == 1);
        // Case 2: Subtracting a positive (= adding a negative) from a negative created a positive
        let case2 = sign_lhs == 1 && (sign_rhs & sign_res == 0);
        case1 || case2
    }
}

// #[cfg(test)]
mod tests {
    use super::*;
    // TODO: test ALU, especially condition flags

    #[test]
    fn test_alu_flags_add() {
        /// Table of [ LHS, RHS, Expected result, Expected Overflow, Expected Carry]
        const TEST_CASES: [(u32, u32, u32, bool, bool); 4] = [
            (1 << 29, 1 << 29, 1 << 30, false, false),
            (u32::MAX, 1, 0, false, true),
            (1 << 30, 1 << 30, 1 << 31, true, false),
            (1 << 31, 1 << 31, 0, true, true),
        ];

        for (idx, (lhs, rhs, result, overflow, carry)) in TEST_CASES.into_iter().enumerate() {
            let mut cpsr = ProgramStatusRegister::new()
                .with_overflow_condition(false)
                .with_cbe_condition(false);
            let AluResult { value, write_back } =
                Alu::exec(OpCode::Add, lhs, rhs, AluSetCpsr::Alter(&mut cpsr));
            assert_eq!(
                value, result,
                "Case {}. Value was {} but expected {}",
                idx, value, result
            );
            assert!(write_back);
            assert_eq!(
                cpsr.overflow_condition(),
                overflow,
                "Case {}. Overflow was {} but expected {}",
                idx,
                cpsr.overflow_condition(),
                overflow
            );
            assert_eq!(
                cpsr.cbe_condition(),
                carry,
                "Case {}. Carry was {} but expected {}",
                idx,
                cpsr.cbe_condition(),
                carry
            );
        }
    }

    #[test]
    fn test_alu_flags_sub() {
        /// Table of [ LHS, RHS, Expected result, Expected Overflow, Expected Carry]
        const TEST_CASES: [(u32, u32, u32, bool, bool); 4] = [
            (2, 1, 1, false, false),
            (0, 1, u32::MAX /* == -1 */, false, true),
            (
                u32::MAX, /* == -1 */
                i32::MAX as u32,
                1 << 31,
                true,
                false,
            ),
            (
                ((1 << 31) - 1),
                u32::MAX, /* == -1*/
                1 << 31,
                true,
                true,
            ),
        ];

        for (idx, (lhs, rhs, result, overflow, carry)) in TEST_CASES.into_iter().enumerate() {
            let mut cpsr = ProgramStatusRegister::new()
                .with_overflow_condition(false)
                .with_cbe_condition(false);
            let AluResult { value, write_back } =
                Alu::exec(OpCode::Sub, lhs, rhs, AluSetCpsr::Alter(&mut cpsr));
            assert_eq!(
                value, result,
                "Case {}. Value was {} but expected {}",
                idx, value, result
            );
            assert!(write_back);
            assert_eq!(
                cpsr.overflow_condition(),
                overflow,
                "Case {}. Overflow was {} but expected {}",
                idx,
                cpsr.overflow_condition(),
                overflow
            );
            assert_eq!(
                cpsr.cbe_condition(),
                carry,
                "Case {}. Carry was {} but expected {}",
                idx,
                cpsr.cbe_condition(),
                carry
            );
        }
    }
}
