use crate::registers::psr::ProgramStatusRegister;
use armv4t_decoder::{
    arm::{data_processing::OpCode, ShiftType},
    common::Signedness,
};

/// The result of an [Alu] calculation.
/// If `write_back` is true, this result `value` should be written back into a destination register if applicable.
pub struct AluResult {
    pub value: u32,
    pub write_back: bool,
}

/// Used to signal to the ALU whether it should change the CPSR.
/// Provide a mutable reference to a cpsr if so, otherwise a read-only reference.
pub enum AluSetCpsr<'a> {
    Alter(&'a mut ProgramStatusRegister),
    Preserve(&'a ProgramStatusRegister),
}

#[derive(Debug)]
pub struct Alu {}

impl Alu {
    pub fn exec_shift(
        number: u32,
        shift_type: ShiftType,
        shift_amount: u8,
        cpsr: AluSetCpsr,
    ) -> u32 {
        let shift_amount = shift_amount as u32;
        let old_carry = match cpsr {
            AluSetCpsr::Alter(ref cpsr) => cpsr.cbe_condition() as u32,
            AluSetCpsr::Preserve(cpsr) => cpsr.cbe_condition() as u32,
        };

        let new_carry;

        let result = match shift_type {
            ShiftType::LogicalLeft => match shift_amount {
                0 => {
                    new_carry = old_carry != 0;
                    number
                }
                1..=31 => {
                    new_carry = (number >> (u32::BITS - shift_amount)) == 1;
                    number << shift_amount
                }
                32 => {
                    new_carry = (number >> (u32::BITS - shift_amount)) == 1;
                    0
                }
                _ => {
                    new_carry = false;
                    0
                }
            },
            ShiftType::LogicalRight => {
                match shift_amount {
                    // 0 is a special case that encodes LSR #32
                    0 => {
                        new_carry = (number >> 31) == 1;
                        0
                    }
                    1..=31 => {
                        new_carry = (number >> (shift_amount - 1)) == 1;
                        number >> shift_amount
                    }
                    _ => {
                        new_carry = false;
                        0
                    }
                }
            }
            ShiftType::ArithmeticRight => {
                let sign_bit = number >> 31;
                match shift_amount {
                    // 0 is a special case that encodes ASR #32
                    0 => {
                        new_carry = sign_bit == 1;
                        if sign_bit == 1 {
                            u32::MAX
                        } else {
                            0
                        }
                    }
                    1..=31 => {
                        let mask =
                            ((1 << (shift_amount * sign_bit)) - 1) << (u32::BITS - shift_amount);
                        new_carry = number >> (shift_amount - 1) == 1;
                        (number >> shift_amount) | mask
                    }
                    _ => {
                        new_carry = false;
                        if sign_bit == 1 {
                            u32::MAX
                        } else {
                            0
                        }
                    }
                }
            }
            ShiftType::RotateRight => {
                match shift_amount {
                    // 0 is a special case that encodes a right-shift of 1 on the
                    // 33-bit quantity made by appending the carry flag to the left of the input number
                    0 => {
                        new_carry = number & 1 == 1;
                        let mask = old_carry << 31;
                        (number >> 1) | mask
                    }
                    1..=32 => {
                        let result = number.rotate_right(shift_amount);
                        new_carry = result >> 31 == 1;
                        result
                    }
                    _ => {
                        let shift_amount = shift_amount % 32;
                        let result = number.rotate_right(shift_amount);
                        new_carry = result >> 31 == 1;
                        result
                    }
                }
            }
        };
        if let AluSetCpsr::Alter(cpsr) = cpsr {
            cpsr.set_cbe_condition(new_carry)
        }
        result
    }

    /// Calculates `lhs * rhs + acc`. If no `acc` should be added, simply pass in 0.
    /// Sets the conditions of the CPSR if `cpsr` is [AluSetCpsr::Alter].
    pub fn exec_mul(lhs: u32, rhs: u32, acc: u32, cpsr: AluSetCpsr) -> u32 {
        let mut res = (lhs as u64) * (rhs as u64);
        res += acc as u64;
        if let AluSetCpsr::Alter(cpsr) = cpsr {
            cpsr.set_zero_condition(res == 0);
            cpsr.set_neg_condition(res >> 31 & 1 == 1);
        }
        res as u32
    }

    /// Calculates `lhs * rhs + acc`. If no `acc` should be added, simply pass in 0.
    /// Sets the conditions of the CPSR if `cpsr` is [AluSetCpsr::Alter].
    /// Returns a 64-bit result as two 32-bit numbers: (hi_bits, low_bits)
    pub fn exec_mul_long(
        lhs: u32,
        rhs: u32,
        acc: u64,
        cpsr: AluSetCpsr,
        sign: Signedness,
    ) -> (u32, u32) {
        let mut res = match sign {
            Signedness::Signed => (i64::from(lhs) * i64::from(rhs)) as u64,
            Signedness::Unsigned => u64::from(lhs) * u64::from(rhs),
        };

        res += acc as u64;
        if let AluSetCpsr::Alter(cpsr) = cpsr {
            cpsr.set_zero_condition(res == 0);
            cpsr.set_neg_condition(res >> 63 & 1 == 1);
        }
        ((res >> 32) as u32, res as u32)
    }

    /// Executes the given `op` on `lhs` and `rhs`, using the conditions given in the `cpsr` for operations that require it.
    /// If `cpsr` is [AluSetCpsr::Alter], the condition codes in the given [ProgramStatusRegister] will be altered based on the result of the calculation.
    /// If the operation to be performed is unary, only `rhs` is used and the value of `lhs` can be chosen freely (but is ignored).
    pub fn exec_op(op: OpCode, lhs: u32, rhs: u32, cpsr: AluSetCpsr) -> AluResult {
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
    use registers::psr::ProgramStatusRegister;

    use crate::registers::{self, CpuRegisters};

    use super::*;

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
                Alu::exec_op(OpCode::Add, lhs, rhs, AluSetCpsr::Alter(&mut cpsr));
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
                Alu::exec_op(OpCode::Sub, lhs, rhs, AluSetCpsr::Alter(&mut cpsr));
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

    /* TODO: Unit tests for shifts:
        1 LSL by 32 has result zero, carry out equal to bit 0 of Rm.
        2 LSL by more than 32 has result zero, carry out zero.
        3 LSR by 32 has result zero, carry out equal to bit 31 of Rm.
        4 LSR by more than 32 has result zero, carry out zero.
        5 ASR by 32 or more has result filled with and carry out equal to bit 31 of Rm.
        6 ROR by 32 has result equal to Rm, carry out equal to bit 31 of Rm.
        7 ROR by n where n is greater than 32 will give the same result and carry out
        as ROR by n-32; therefore repeatedly subtract 32 from n until the amount is
        in the range 1 to 32 and see above.
    */

    #[test]
    // LSL by 32 has result zero, carry out equal to bit 0 of Rm.
    fn test_alu_lsl_by_32() {
        let mut cpsr = ProgramStatusRegister::default();
        let number = 0b1;
        let result = Alu::exec_shift(
            number,
            ShiftType::LogicalLeft,
            32,
            AluSetCpsr::Alter(&mut cpsr),
        );
        assert_eq!(result, 0);
        assert!(cpsr.cbe_condition());
    }

    #[test]
    // LSL by more than 32 has result zero, carry out zero.
    fn test_alu_lsl_by_more_than_32() {
        let mut cpsr = ProgramStatusRegister::default();
        let number = u32::MAX;
        let result = Alu::exec_shift(
            number,
            ShiftType::LogicalLeft,
            33,
            AluSetCpsr::Alter(&mut cpsr),
        );
        assert_eq!(result, 0);
        assert!(!cpsr.cbe_condition());
    }

    #[test]
    // LSR by 32 (encoded as LSR by 0) has result zero, carry out equal to bit 31 of Rm.
    fn test_alu_lsr_by_32() {
        let mut cpsr = ProgramStatusRegister::default();
        let number = 1 << 31;
        let result = Alu::exec_shift(
            number,
            ShiftType::LogicalRight,
            0,
            AluSetCpsr::Alter(&mut cpsr),
        );
        assert_eq!(result, 0);
        assert!(cpsr.cbe_condition());
    }

    #[test]
    // LSR by more than 32 has result zero, carry out zero.
    fn test_alu_lsr_by_more_than_32() {
        let mut cpsr = ProgramStatusRegister::default();
        let number = 1 << 31;
        let result = Alu::exec_shift(
            number,
            ShiftType::LogicalRight,
            33,
            AluSetCpsr::Alter(&mut cpsr),
        );
        assert_eq!(result, 0);
        assert!(!cpsr.cbe_condition());
    }

    #[test]
    // ASR by 32 or more has result filled with and carry out equal to bit 31 of Rm.
    fn test_alu_asr_by_32() {
        let mut cpsr = ProgramStatusRegister::default();
        let number = 1 << 31;
        let result = Alu::exec_shift(
            number,
            ShiftType::ArithmeticRight,
            0,
            AluSetCpsr::Alter(&mut cpsr),
        );
        assert_eq!(result, u32::MAX);
        assert!(cpsr.cbe_condition());
    }

    #[test]
    // ROR by 32 has result equal to Rm, carry out equal to bit 31 of Rm.
    fn test_alu_ror_by_32() {
        let mut cpsr = ProgramStatusRegister::default();
        let number = 0xF0F0_F0F0;
        let result = Alu::exec_shift(
            number,
            ShiftType::RotateRight,
            32,
            AluSetCpsr::Alter(&mut cpsr),
        );
        assert_eq!(result, number);
        assert!(cpsr.cbe_condition());
    }

    #[test]
    // ROR by n where n is greater than 32 will give the same result and carry out
    // as ROR by n-32; therefore repeatedly subtract 32 from n until the amount is
    // in the range 1 to 32 and see above.
    fn test_alu_ror_by_more_than_32() {
        let mut cpsr = ProgramStatusRegister::default();
        let number = 0xF0F0_F0F1;
        let n = 33;
        let result = Alu::exec_shift(
            number,
            ShiftType::RotateRight,
            n,
            AluSetCpsr::Alter(&mut cpsr),
        );
        assert_eq!(result, number.rotate_right((n % 32) as u32));
        assert!(cpsr.cbe_condition());
    }

    #[test]
    /// ROR by #0 encodes a special function Rotate Right Extended (RRX).
    /// This is a rotate right by one bit position of the 33 bit quantity formed by appending
    /// the CPSR C flag to the most significant end of the contents of Rm
    fn test_alu_rrx() {
        let mut cpsr = ProgramStatusRegister::default().with_cbe_condition(true);
        let number = 0b11;
        let result = Alu::exec_shift(
            number,
            ShiftType::RotateRight,
            0,
            AluSetCpsr::Alter(&mut cpsr),
        );
        assert_eq!(result, (0b1 << 31) | 0b1);
        assert!(cpsr.cbe_condition());
    }
}
