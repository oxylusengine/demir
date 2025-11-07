#[derive(Debug, Clone)]
pub enum Statement {
    Multi(Vec<Statement>),

    DeclFunction {
        identifier: Identifier,
        params: Vec<FunctionParam>,
        body: Box<Statement>,
    },

    Expression(Expression),
}

#[derive(Debug, Clone)]
pub enum Expression {
    Literal(Literal),
    Identifier(Identifier),
    Assign {
        kind: AssignmentKind,
        lhs: Box<Expression>,
        rhs: Box<Expression>,
    },
    Binary {
        op: BinaryOp,
        lhs: Box<Expression>,
        rhs: Box<Expression>,
    },
    CallFunction {
        callee: Box<Expression>,
        parameters: Vec<Expression>,
    },
}

#[derive(Debug, Clone)]
pub struct Identifier(pub String); // TODO: Maybe have an inner string table in the future

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
pub struct FunctionParam(pub Identifier, pub Expression);
