// WARN: FOR THE LOVE OF GOD DON'T CHANGE ANY ELEMENT
// IF YOU HAVE SOMETHING TO ADD, JUST ADD TO THE BOTTOM
// WITH UNIQUE ID.
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Op {
    Nop = 0x00,

    PushNull = 0x01,
    PushTrue = 0x02,
    PushFalse = 0x03,
    // Push a constant number to stack.
    /// Operand 1: (1 byte) constant
    PushI8 = 0x04,
    /// Operand 1: (2 bytes) constant
    PushI16 = 0x05,
    /// Operand 1: (4 bytes) constant
    PushI32 = 0x06,
    PushF32 = 0x07,
    // Simple optimization to avoid indirections
    PushZero = 0x08,
    PushOne = 0x09,

    AddI32 = 0x0A,
    SubI32 = 0x0B,
    MulI32 = 0x0C,
    DivI32 = 0x0D,
    AddF32 = 0x0E,
    SubF32 = 0x0F,
    MulF32 = 0x10,
    DivF32 = 0x11,
    AddI64 = 0x12,
    SubI64 = 0x13,
    MulI64 = 0x14,
    DivI64 = 0x15,
    AddF64 = 0x16,
    SubF64 = 0x17,
    MulF64 = 0x18,
    DivF64 = 0x19,

    /// Operand 1: (2 bytes) local slot
    LoadLocal = 0x1A,
    /// Operand 1: (2 bytes) local slot
    StoreLocal = 0x1B,

    /// Operand 1: (4 bytes) address
    Jump = 0x1C,

    /// Operand 1: (2 bytes) function Id
    /// Operand 2: (1 byte) arg count
    Call = 0x1D,
    Ret = 0x1E,
    RetValue = 0x1F,

    /// Operand 1: (2 bytes) string Id
    PushString = 0x20,

    EqualI32 = 0x21,
    NotEqualI32 = 0x22,
    GreaterThanI32 = 0x23,
    GreaterThanEqualI32 = 0x24,
    LessThanI32 = 0x25,
    LessThanEqualI32 = 0x26,
    EqualI64 = 0x27,
    NotEqualI64 = 0x28,
    GreaterThanI64 = 0x29,
    GreaterThanEqualI64 = 0x2A,
    LessThanI64 = 0x2B,
    LessThanEqualI64 = 0x2C,

    LogicalAnd = 0x2D,
    LogicalOr = 0x2E,

    /// Operand 1: (4 bytes) address
    JumpEqual = 0x33,
    /// Operand 1: (4 bytes) address
    JumpNotEqual = 0x34,

    ModI32 = 0x37,
    ModI64 = 0x38,
}

impl Op {
    pub fn from_u8(value: u8) -> Option<Self> {
        match value {
            0x00 => Some(Op::Nop),
            0x01 => Some(Op::PushNull),
            0x02 => Some(Op::PushTrue),
            0x03 => Some(Op::PushFalse),
            0x04 => Some(Op::PushI8),
            0x05 => Some(Op::PushI16),
            0x06 => Some(Op::PushI32),
            0x07 => Some(Op::PushF32),
            0x08 => Some(Op::PushZero),
            0x09 => Some(Op::PushOne),
            0x0A => Some(Op::AddI32),
            0x0B => Some(Op::SubI32),
            0x0C => Some(Op::MulI32),
            0x0D => Some(Op::DivI32),
            0x0E => Some(Op::AddF32),
            0x0F => Some(Op::SubF32),
            0x10 => Some(Op::MulF32),
            0x11 => Some(Op::DivF32),
            0x12 => Some(Op::AddI64),
            0x13 => Some(Op::SubI64),
            0x14 => Some(Op::MulI64),
            0x15 => Some(Op::DivI64),
            0x16 => Some(Op::AddF64),
            0x17 => Some(Op::SubF64),
            0x18 => Some(Op::MulF64),
            0x19 => Some(Op::DivF64),
            0x1A => Some(Op::LoadLocal),
            0x1B => Some(Op::StoreLocal),
            0x1C => Some(Op::Jump),
            0x1D => Some(Op::Call),
            0x1E => Some(Op::Ret),
            0x1F => Some(Op::RetValue),
            0x20 => Some(Op::PushString),
            0x21 => Some(Op::EqualI32),
            0x22 => Some(Op::NotEqualI32),
            0x23 => Some(Op::GreaterThanI32),
            0x24 => Some(Op::GreaterThanEqualI32),
            0x25 => Some(Op::LessThanI32),
            0x26 => Some(Op::LessThanEqualI32),
            0x27 => Some(Op::EqualI64),
            0x28 => Some(Op::NotEqualI64),
            0x29 => Some(Op::GreaterThanI64),
            0x2A => Some(Op::GreaterThanEqualI64),
            0x2B => Some(Op::LessThanI64),
            0x2C => Some(Op::LessThanEqualI64),
            0x2D => Some(Op::LogicalAnd),
            0x2E => Some(Op::LogicalOr),
            0x33 => Some(Op::JumpEqual),
            0x34 => Some(Op::JumpNotEqual),
            0x37 => Some(Op::ModI32),
            0x38 => Some(Op::ModI64),

            _ => None,
        }
    }
}
