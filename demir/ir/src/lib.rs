pub mod types;

use crate::types::BuiltinType;

#[derive(Debug, Clone)]
pub enum IrNode {
    // Root module
    Module {
        nodes: Vec<IrNode>,
        globals: Vec<IrNodeId>,
    },

    // Literals/Constants
    Constant(IrConstant),

    // Type annotations
    Type(BuiltinType),

    // Variables
    Variable {
        ty: IrNodeId,
    },

    // Memory operations
    Load {
        variable: IrNodeId,
    },
    Store {
        src: IrNodeId,
        dst: IrNodeId,
    },

    // Binary operations
    Add(IrNodeId, IrNodeId),
    Sub(IrNodeId, IrNodeId),
    Mul(IrNodeId, IrNodeId),
    Div(IrNodeId, IrNodeId),
    Mod(IrNodeId, IrNodeId),

    // Bitwise operations
    BitAnd(IrNodeId, IrNodeId),
    BitOr(IrNodeId, IrNodeId),
    BitXor(IrNodeId, IrNodeId),
    BitNot(IrNodeId),
    ShiftLeft(IrNodeId, IrNodeId),
    ShiftRight(IrNodeId, IrNodeId),

    // Comparison operations
    Equal(IrNodeId, IrNodeId),
    NotEqual(IrNodeId, IrNodeId),
    GreaterThan(IrNodeId, IrNodeId),
    GreaterThanOrEqual(IrNodeId, IrNodeId),
    LessThan(IrNodeId, IrNodeId),
    LessThanOrEqual(IrNodeId, IrNodeId),

    // Logical operations
    LogicalAnd(IrNodeId, IrNodeId),
    LogicalOr(IrNodeId, IrNodeId),
    LogicalNot(IrNodeId),

    // Control flow
    Label(Vec<IrNodeId>),
    Return(Option<IrNodeId>),
    Branch(IrNodeId),
    ConditionalBranch {
        condition: IrNodeId,
        true_block: IrNodeId,
        false_block: IrNodeId,
    },

    // Function operations
    Function {
        ty: IrNodeId,
        starter_block: IrNodeId,
        parameters: Vec<IrNodeId>,
    },
    Call {
        callee: IrNodeId,
        args: Vec<IrNodeId>,
    },
}

pub type IrNodeId = usize;

#[derive(Debug, Clone, PartialEq)]
pub enum IrConstant {
    Null,
    True,
    False,
    Int(i64),
    UInt(u64),
    Float(f64),
    String(String),
}

impl IrConstant {
    pub fn from_bool(b: bool) -> Self { if b { IrConstant::True } else { IrConstant::False } }
}
