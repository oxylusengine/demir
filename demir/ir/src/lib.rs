use core::types::BuiltinType;

#[derive(Debug, Clone)]
pub enum IrNode {
    // Root module
    Module {
        nodes: Vec<IrNode>,
        functions: Vec<IrNodeId>,
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
        ty: IrNodeId,
        variable: IrNodeId,
    },
    Store {
        src: IrNodeId,
        dst: IrNodeId,
    },

    // Binary operations
    Add {
        ty: IrNodeId,
        lhs: IrNodeId,
        rhs: IrNodeId,
    },
    Sub {
        ty: IrNodeId,
        lhs: IrNodeId,
        rhs: IrNodeId,
    },
    Mul {
        ty: IrNodeId,
        lhs: IrNodeId,
        rhs: IrNodeId,
    },
    Div {
        ty: IrNodeId,
        lhs: IrNodeId,
        rhs: IrNodeId,
    },
    Mod {
        ty: IrNodeId,
        lhs: IrNodeId,
        rhs: IrNodeId,
    },

    // Bitwise operations
    BitAnd {
        ty: IrNodeId,
        lhs: IrNodeId,
        rhs: IrNodeId,
    },
    BitOr {
        ty: IrNodeId,
        lhs: IrNodeId,
        rhs: IrNodeId,
    },
    BitXor {
        ty: IrNodeId,
        lhs: IrNodeId,
        rhs: IrNodeId,
    },
    BitNot(IrNodeId),
    ShiftLeft {
        ty: IrNodeId,
        lhs: IrNodeId,
        rhs: IrNodeId,
    },
    ShiftRight {
        ty: IrNodeId,
        lhs: IrNodeId,
        rhs: IrNodeId,
    },

    // Comparison operations
    Equal {
        ty: IrNodeId,
        lhs: IrNodeId,
        rhs: IrNodeId,
    },
    NotEqual {
        ty: IrNodeId,
        lhs: IrNodeId,
        rhs: IrNodeId,
    },
    GreaterThan {
        ty: IrNodeId,
        lhs: IrNodeId,
        rhs: IrNodeId,
    },
    GreaterThanOrEqual {
        ty: IrNodeId,
        lhs: IrNodeId,
        rhs: IrNodeId,
    },
    LessThan {
        ty: IrNodeId,
        lhs: IrNodeId,
        rhs: IrNodeId,
    },
    LessThanOrEqual {
        ty: IrNodeId,
        lhs: IrNodeId,
        rhs: IrNodeId,
    },

    // Logical operations
    LogicalAnd {
        ty: IrNodeId,
        lhs: IrNodeId,
        rhs: IrNodeId,
    },
    LogicalOr {
        ty: IrNodeId,
        lhs: IrNodeId,
        rhs: IrNodeId,
    },
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
    },
    FunctionParam {
        ty: IrNodeId,
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
    I32(i32),
    U32(u32),
    F32(f32),
    String(String),
}

impl IrConstant {
    pub fn from_bool(b: bool) -> Self { if b { IrConstant::True } else { IrConstant::False } }
}
