// WARN: FOR THE LOVE OF GOD DON'T CHANGE ANY ELEMENT
// IF YOU HAVE SOMETHING TO ADD, JUST ADD TO THE BOTTOM
// WITH UNIQUE ID.
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Op {
    // Control flow
    Nop = 0x00,
    Ret = 0x01,
    RetValue = 0x02,

    // Constants & Stack
    PushNull = 0x03,
    PushTrue = 0x04,
    PushFalse = 0x05,
    PushZero = 0x06,
    PushOne = 0x07,
    PushI8 = 0x08,     // Operand: 1 byte
    PushI16 = 0x09,    // Operand: 2 bytes
    PushI32 = 0x0A,    // Operand: 4 bytes
    PushI64 = 0x0B,    // Operand: 8 bytes
    PushF32 = 0x0C,    // Operand: 4 bytes
    PushF64 = 0x0D,    // Operand: 8 bytes
    PushString = 0x0E, // Operand: 2 bytes (string id)

    // Local variables
    LoadLocal = 0x0F,  // Operand: 2 bytes (slot)
    StoreLocal = 0x10, // Operand: 2 bytes (slot)

    // Jumps
    Jump = 0x11,         // Operand: 4 bytes (address)
    JumpEqual = 0x12,    // Operand: 4 bytes (address)
    JumpNotEqual = 0x13, // Operand: 4 bytes (address)

    // Function calls
    Call = 0x14, // Operands: 2 bytes (func_id) + 1 byte (arg_count)

    // Arithmetic I32
    AddI32 = 0x15,
    SubI32 = 0x16,
    MulI32 = 0x17,
    DivI32 = 0x18,
    ModI32 = 0x19,

    // Arithmetic I64
    AddI64 = 0x1A,
    SubI64 = 0x1B,
    MulI64 = 0x1C,
    DivI64 = 0x1D,
    ModI64 = 0x1E,

    // Arithmetic F32
    AddF32 = 0x1F,
    SubF32 = 0x20,
    MulF32 = 0x21,
    DivF32 = 0x22,

    // Arithmetic F64
    AddF64 = 0x23,
    SubF64 = 0x24,
    MulF64 = 0x25,
    DivF64 = 0x26,

    // Comparison I32 (signed)
    EqualI32 = 0x27,
    NotEqualI32 = 0x28,
    LessThanI32 = 0x29,
    LessThanEqualI32 = 0x2A,
    GreaterThanI32 = 0x2B,
    GreaterThanEqualI32 = 0x2C,

    // Comparison U32 (unsigned)
    LessThanU32 = 0x2D,
    LessThanEqualU32 = 0x2E,
    GreaterThanU32 = 0x2F,
    GreaterThanEqualU32 = 0x30,

    // Comparison I64 (signed)
    EqualI64 = 0x31,
    NotEqualI64 = 0x32,
    LessThanI64 = 0x33,
    LessThanEqualI64 = 0x34,
    GreaterThanI64 = 0x35,
    GreaterThanEqualI64 = 0x36,

    // Comparison U64 (unsigned)
    LessThanU64 = 0x37,
    LessThanEqualU64 = 0x38,
    GreaterThanU64 = 0x39,
    GreaterThanEqualU64 = 0x3A,

    // Logical
    LogicalAnd = 0x3B,
    LogicalOr = 0x3C,

    // Bitwise I32
    BitAndI32 = 0x3D,
    BitOrI32 = 0x3E,
    BitXorI32 = 0x3F,
    BitNotI32 = 0x40,

    // Bitwise I64
    BitAndI64 = 0x41,
    BitOrI64 = 0x42,
    BitXorI64 = 0x43,
    BitNotI64 = 0x44,

    // Bitshift I32
    ShiftLeftI32 = 0x45,
    ShiftRightI32 = 0x46,

    // Bitshift I64
    ShiftLeftI64 = 0x47,
    ShiftRightI64 = 0x48,

    // References
    PushReference = 0x49, // Operands: 2 bytes (local Id)
    Dereference = 0x4A,
}

impl Op {
    pub fn from_u8(value: u8) -> Option<Self> {
        match value {
            0x00 => Some(Op::Nop),
            0x01 => Some(Op::Ret),
            0x02 => Some(Op::RetValue),
            0x03 => Some(Op::PushNull),
            0x04 => Some(Op::PushTrue),
            0x05 => Some(Op::PushFalse),
            0x06 => Some(Op::PushZero),
            0x07 => Some(Op::PushOne),
            0x08 => Some(Op::PushI8),
            0x09 => Some(Op::PushI16),
            0x0A => Some(Op::PushI32),
            0x0B => Some(Op::PushI64),
            0x0C => Some(Op::PushF32),
            0x0D => Some(Op::PushF64),
            0x0E => Some(Op::PushString),
            0x0F => Some(Op::LoadLocal),
            0x10 => Some(Op::StoreLocal),
            0x11 => Some(Op::Jump),
            0x12 => Some(Op::JumpEqual),
            0x13 => Some(Op::JumpNotEqual),
            0x14 => Some(Op::Call),
            0x15 => Some(Op::AddI32),
            0x16 => Some(Op::SubI32),
            0x17 => Some(Op::MulI32),
            0x18 => Some(Op::DivI32),
            0x19 => Some(Op::ModI32),
            0x1A => Some(Op::AddI64),
            0x1B => Some(Op::SubI64),
            0x1C => Some(Op::MulI64),
            0x1D => Some(Op::DivI64),
            0x1E => Some(Op::ModI64),
            0x1F => Some(Op::AddF32),
            0x20 => Some(Op::SubF32),
            0x21 => Some(Op::MulF32),
            0x22 => Some(Op::DivF32),
            0x23 => Some(Op::AddF64),
            0x24 => Some(Op::SubF64),
            0x25 => Some(Op::MulF64),
            0x26 => Some(Op::DivF64),
            0x27 => Some(Op::EqualI32),
            0x28 => Some(Op::NotEqualI32),
            0x29 => Some(Op::LessThanI32),
            0x2A => Some(Op::LessThanEqualI32),
            0x2B => Some(Op::GreaterThanI32),
            0x2C => Some(Op::GreaterThanEqualI32),
            0x2D => Some(Op::LessThanU32),
            0x2E => Some(Op::LessThanEqualU32),
            0x2F => Some(Op::GreaterThanU32),
            0x30 => Some(Op::GreaterThanEqualU32),
            0x31 => Some(Op::EqualI64),
            0x32 => Some(Op::NotEqualI64),
            0x33 => Some(Op::LessThanI64),
            0x34 => Some(Op::LessThanEqualI64),
            0x35 => Some(Op::GreaterThanI64),
            0x36 => Some(Op::GreaterThanEqualI64),
            0x37 => Some(Op::LessThanU64),
            0x38 => Some(Op::LessThanEqualU64),
            0x39 => Some(Op::GreaterThanU64),
            0x3A => Some(Op::GreaterThanEqualU64),
            0x3B => Some(Op::LogicalAnd),
            0x3C => Some(Op::LogicalOr),
            0x3D => Some(Op::BitAndI32),
            0x3E => Some(Op::BitOrI32),
            0x3F => Some(Op::BitXorI32),
            0x40 => Some(Op::BitNotI32),
            0x41 => Some(Op::BitAndI64),
            0x42 => Some(Op::BitOrI64),
            0x43 => Some(Op::BitXorI64),
            0x44 => Some(Op::BitNotI64),
            0x45 => Some(Op::ShiftLeftI32),
            0x46 => Some(Op::ShiftRightI32),
            0x47 => Some(Op::ShiftLeftI64),
            0x48 => Some(Op::ShiftRightI64),
            0x49 => Some(Op::PushReference),
            0x4A => Some(Op::Dereference),
            _ => None,
        }
    }
}
