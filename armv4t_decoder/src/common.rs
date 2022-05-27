use modular_bitfield::BitfieldSpecifier;

#[derive(Clone, Copy, PartialEq, Eq, Debug, BitfieldSpecifier)]
#[bits = 4]
pub enum RegisterName {
    R0,
    R1,
    R2,
    R3,
    R4,
    R5,
    R6,
    R7,
    R8,
    R9,
    R10,
    R11,
    R12,
    R13,
    R14,
    R15,
}
