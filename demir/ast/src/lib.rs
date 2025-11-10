// pub mod lowering;

use core::types::BuiltinType;

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

    pub fn get_expr(&self, expr_id: ExpressionId) -> Option<&Expression> { self.expressions.get(expr_id) }
}

#[derive(Debug, Clone)]
pub enum Statement {
    Multi(Vec<Statement>),

    DeclFunction {
        identifier: Identifier,
        params: Vec<FunctionParam>,
        body: Box<Statement>,
        return_expr: Option<ExpressionId>,
    },

    DeclVar {
        identifier: Identifier,
        type_expr: Option<ExpressionId>,
        initial_expr: Option<ExpressionId>,
    },

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
    CallFunction {
        callee: ExpressionId,
        parameters: Vec<ExpressionId>,
    },
}

pub type ExpressionId = usize;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Identifier(pub String); // TODO: Maybe have an inner string table in the future

impl std::fmt::Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result { write!(f, "{}", self.0) }
}

#[derive(Clone, Debug)]
pub enum Literal {
    String(String),
    Integer(i64),
    Float(f64),
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

    /// Range
    RightExclusiveRange, // ..
    RightInclusiveRange, // ..=
}

#[derive(Debug, Clone)]
pub struct FunctionParam(pub Identifier, pub ExpressionId);
