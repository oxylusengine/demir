use ast::{AssignmentKind, BinaryOp};
use lexer::token::Token;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Default)]
pub enum Precedence {
    Invalid = -1,
    #[default]
    Comma,
    Assignment,
    Range,
    LogicalOr,
    LogicalAnd,
    BitOr,
    BitAnd,
    CompareEqual,
    CompareRelational,
    BitShift,
    Additive,
    Multiplicative,
}

pub fn token_to_precedence(token: &Token<'_>) -> Precedence {
    match token {
        Token::Comma => Precedence::Comma,
        Token::Equal | Token::AddEqual | Token::SubEqual | Token::MulEqual | Token::DivEqual => Precedence::Assignment,
        Token::Range | Token::RangeEqual => Precedence::Range,
        Token::LogicalOr => Precedence::LogicalOr,
        Token::LogicalAnd => Precedence::LogicalAnd,
        Token::BitOr => Precedence::BitOr,
        Token::BitAnd => Precedence::BitAnd,
        Token::CompareEqual | Token::CompareNotEqual => Precedence::CompareEqual,
        Token::AngleLeft | Token::AngleRight | Token::GreaterEqual | Token::LessEqual => Precedence::CompareRelational,
        Token::ShiftLeft | Token::ShiftRight => Precedence::BitShift,
        Token::Add | Token::Sub => Precedence::Additive,
        Token::Mul | Token::Div => Precedence::Multiplicative,

        _ => Precedence::Invalid,
    }
}

pub fn token_to_binary_op(token: &Token<'_>) -> Option<BinaryOp> {
    match token {
        Token::Equal => Some(BinaryOp::CompEq),
        Token::Add => Some(BinaryOp::Add),
        Token::Sub => Some(BinaryOp::Sub),
        Token::Mul => Some(BinaryOp::Mul),
        Token::Div => Some(BinaryOp::Div),
        Token::Modulo => Some(BinaryOp::Mod),
        Token::BitAnd => Some(BinaryOp::BitAnd),
        Token::BitXor => Some(BinaryOp::BitXor),
        Token::BitOr => Some(BinaryOp::BitOr),
        Token::AngleRight => Some(BinaryOp::CompGreater),
        Token::AngleLeft => Some(BinaryOp::CompLess),
        Token::CompareEqual => Some(BinaryOp::CompEq),
        Token::CompareNotEqual => Some(BinaryOp::CompNotEq),
        Token::LogicalAnd => Some(BinaryOp::CompAnd),
        Token::LogicalOr => Some(BinaryOp::CompOr),
        Token::GreaterEqual => Some(BinaryOp::CompGreaterEq),
        Token::LessEqual => Some(BinaryOp::CompLessEq),
        Token::ShiftLeft => Some(BinaryOp::ShiftLeft),
        Token::ShiftRight => Some(BinaryOp::ShiftRight),
        Token::Range => Some(BinaryOp::RightExclusiveRange),
        Token::RangeEqual => Some(BinaryOp::RightInclusiveRange),
        _ => None,
    }
}

pub fn token_to_assignment_kind(token: &Token<'_>) -> Option<AssignmentKind> {
    match token {
        Token::Equal => Some(AssignmentKind::Assign),
        Token::AddEqual => Some(AssignmentKind::CompoundAdd),
        Token::SubEqual => Some(AssignmentKind::CompoundSub),
        Token::MulEqual => Some(AssignmentKind::CompoundMul),
        Token::DivEqual => Some(AssignmentKind::CompoundDiv),
        _ => None,
    }
}
