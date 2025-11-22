pub mod lowering;

use core::types::{BuiltinType, Identifier};

pub struct AST {
    pub root: Statement,
    pub expressions: Vec<Expression>,
    pub expression_types: Vec<BuiltinType>,
}

impl AST {
    pub fn new(root: Statement, expressions: Vec<Expression>) -> Self {
        Self {
            root,
            expressions,
            expression_types: Vec::new(),
        }
    }

    pub fn get_expr(&self, expr_id: &ExpressionId) -> Option<&Expression> { self.expressions.get(*expr_id) }

    pub fn get_expr_with_ty(&self, expr_id: &ExpressionId) -> Option<(&Expression, &BuiltinType)> {
        if self.expressions.len() < *expr_id || self.expression_types.len() < *expr_id {
            return None;
        }

        Some((
            self.expressions.get(*expr_id).unwrap(),
            self.expression_types.get(*expr_id).unwrap(),
        ))
    }
}

#[derive(Debug, Clone)]
pub enum Statement {
    Multi(Vec<Statement>),

    DeclFunction {
        identifier: Identifier,
        params: Vec<FunctionParam>,
        body: Option<Box<Statement>>,
        return_expr: Option<ExpressionId>,
        external: bool,
    },

    DeclVar {
        identifier: Identifier,
        type_expr: Option<ExpressionId>,
        initial_expr: Option<ExpressionId>,
        is_mutable: bool,
    },

    Return(ExpressionId),

    If {
        condition: ExpressionId,
        true_case: Box<Statement>,
        false_case: Option<Box<Statement>>,
    },

    While {
        condition: ExpressionId,
        true_case: Box<Statement>,
    },

    For {
        iter: Identifier,
        range: ExpressionId,
        body: Box<Statement>,
    },

    Continue,
    Break,

    Expression(ExpressionId),
}

#[derive(Debug, Clone)]
pub enum Expression {
    Literal(Literal),
    Identifier(Identifier),
    Assign {
        kind: AssignmentKind,
        lhs_expr: ExpressionId,
        rhs_expr: ExpressionId,
    },
    Binary {
        op: BinaryOp,
        lhs_expr: ExpressionId,
        rhs_expr: ExpressionId,
    },
    Range {
        kind: RangeKind,
        lhs_expr: ExpressionId,
        rhs_expr: ExpressionId,
    },
    CallFunction {
        callee: ExpressionId,
        parameters: Vec<ExpressionId>,
    },
}

pub type ExpressionId = usize;

#[derive(Debug, Clone)]
pub enum Attrib {
    External,
}

#[derive(Clone, Debug)]
pub enum Literal {
    String(String),
    Integer(i32),
    Float(f32),
    Bool(bool),
}

#[derive(Clone, Debug)]
pub enum AssignmentKind {
    Assign,      // x = y
    CompoundAdd, // x += y
    CompoundSub, // x -= y
    CompoundMul, // x *= y
    CompoundDiv, // x /= y
}

#[derive(Clone, Debug)]
pub enum BinaryOp {
    /// Arithmetic
    Add, // +
    Sub, // -
    Mul, // *
    Div, // /
    Mod, // %

    /// Bitwise
    BitAnd, // &
    BitXor, // ^
    BitOr,  // |

    /// Comparison
    CompGreater, // >
    CompLess,      //
    CompEq,        // ==
    CompNotEq,     // !=
    CompAnd,       // &&
    CompOr,        // ||
    CompGreaterEq, // >=
    CompLessEq,    // <=

    /// Bitwise shift
    ShiftLeft, //
    ShiftRight, // >>
}

#[derive(Clone, Debug)]
pub enum RangeKind {
    Range,
    // For some reason rust doesn't allow us to use this
    // RangeInclusive,
}

#[derive(Debug, Clone)]
pub enum InfixOperator {
    Assignment(AssignmentKind),
    Binary(BinaryOp),
    Range(RangeKind),
}

#[derive(Debug, Clone)]
pub struct FunctionParam(pub Identifier, pub ExpressionId);
