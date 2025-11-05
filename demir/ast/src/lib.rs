#[derive(Debug, Clone)]
pub enum Statement {
    Multi(Vec<Statement>),

    DeclFunction {
        identifier: Identifier,
        params: Vec<FunctionParam>,
    },

    Expression(Expression),
}

#[derive(Debug, Clone)]
pub enum Expression {
    Literal(Literal),
    CallFunction {
        callee: Box<Expression>,
        parameters: Vec<Expression>,
    },
}

#[derive(Debug, Clone)]
pub struct Identifier(pub String);

#[derive(Clone, Debug)]
pub enum Literal {
    String(String),
    Integer(i64),
    Float(f64),
    Bool(bool),
}

#[derive(Debug, Clone)]
pub struct FunctionParam(pub Identifier, pub Expression);
