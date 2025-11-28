use core::types::BuiltinType;

use ast::AST;

use crate::ast_lowering::IrModuleBuilder;

pub mod ast_lowering;

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
    BitNot {
        ty: IrNodeId,
        dst: IrNodeId,
    },
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
    ExternalFunction {
        ty: IrNodeId,
        params: Vec<IrNodeId>,
    },
    Function {
        ty: IrNodeId,
        params: Vec<IrNodeId>,
        starter_block: IrNodeId,
    },
    FunctionParam {
        ty: IrNodeId,
    },
    Call {
        callee: IrNodeId,
        args: Vec<IrNodeId>,
    },
    Pointer {
        variable_id: IrNodeId,
    },
    Dereference {
        ty: IrNodeId,
        ptr: IrNodeId,
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

pub fn lower_ast(ast: AST) -> IrNode {
    let mut module_builder = IrModuleBuilder::new(&ast);
    module_builder.define_builtin("i8", BuiltinType::I8);
    module_builder.define_builtin("u8", BuiltinType::U8);
    module_builder.define_builtin("i16", BuiltinType::I16);
    module_builder.define_builtin("u16", BuiltinType::U16);
    module_builder.define_builtin("i32", BuiltinType::I32);
    module_builder.define_builtin("u32", BuiltinType::U32);
    module_builder.define_builtin("i64", BuiltinType::I64);
    module_builder.define_builtin("u64", BuiltinType::U64);
    module_builder.define_builtin("f32", BuiltinType::F32);
    module_builder.define_builtin("f64", BuiltinType::F64);
    module_builder.define_builtin("bool", BuiltinType::Bool);
    module_builder.define_builtin("str", BuiltinType::String);
    module_builder.lower_stmt(&ast.root);

    IrNode::Module {
        nodes: module_builder.nodes,
        functions: module_builder.functions,
        globals: module_builder.globals,
    }
}
