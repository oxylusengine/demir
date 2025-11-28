#![allow(dead_code)]

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Token<'a> {
    /// End of the lexing file
    Eof,
    Identifier(&'a str),

    /// Keywords
    Null,
    Let,
    Var,
    Fn,
    Return,
    If,
    Else,
    True,
    False,
    While,
    Continue,
    Break,
    For,
    In,
    Mut,

    /// Punctuators - Single character
    Equal, // =
    Add,         // +
    Sub,         // -
    Mul,         // *
    Div,         // /
    Comma,       // ,
    Dot,         // .
    Colon,       // :
    Semicolon,   // ;
    Backslash,   // \
    Exclaim,     // !
    Question,    // ?
    Hash,        // #
    Modulo,      // %
    ParenLeft,   // (
    ParenRight,  // )
    BraceLeft,   // {
    BraceRight,  // }
    SquareLeft,  // [
    SquareRight, // ]
    AngleLeft,   // <
    AngleRight,  // >
    BitAnd,      // &
    BitXor,      // ^
    BitOr,       // |
    BitNot,      // ~
    SingleQuote, // '
    Quote,       // "
    At,          // @
    Dollar,      // $

    /// Punctuators - Multi character
    Range, // ..
    RangeEqual,      // ..=
    Ellipsis,        // ...
    AddEqual,        // +=
    SubEqual,        // -=
    MulEqual,        // *=
    DivEqual,        // /=
    GreaterEqual,    // >=
    LessEqual,       // <=
    Arrow,           // ->
    ShipRight,       // =>
    LogicalAnd,      // &&
    LogicalOr,       // ||
    CompareEqual,    // ==
    CompareNotEqual, // !=
    ShiftLeft,       // <<
    ShiftRight,      // >>

    /// Literals
    IntegerLiteral(&'a str),
    FloatingPointLiteral(&'a str),
    StringLiteral(&'a str),

    /// Comments
    LineComment(&'a str),
    BlockComment(&'a str),
}

impl<'a> Token<'a> {
    pub fn from_identifier(ident: &'a str) -> Self {
        match ident {
            "null" => Token::Null,
            "let" => Token::Let,
            "var" => Token::Var,
            "fn" => Token::Fn,
            "return" => Token::Return,
            "if" => Token::If,
            "else" => Token::Else,
            "true" => Token::True,
            "false" => Token::False,
            "while" => Token::While,
            "continue" => Token::Continue,
            "break" => Token::Break,
            "for" => Token::For,
            "in" => Token::In,
            "mut" => Token::Mut,
            _ => Token::Identifier(ident),
        }
    }
}

impl std::fmt::Display for Token<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
            Token::Eof => write!(f, "eof"),
            Token::Identifier(s) => write!(f, "{s}"),

            // Keywords
            Token::Null => write!(f, "null"),
            Token::Let => write!(f, "let"),
            Token::Var => write!(f, "var"),
            Token::Fn => write!(f, "fn"),
            Token::Return => write!(f, "return"),
            Token::If => write!(f, "if"),
            Token::Else => write!(f, "else"),
            Token::True => write!(f, "true"),
            Token::False => write!(f, "false"),
            Token::While => write!(f, "while"),
            Token::Continue => write!(f, "continue"),
            Token::Break => write!(f, "break"),
            Token::For => write!(f, "for"),
            Token::In => write!(f, "in"),
            Token::Mut => write!(f, "mut"),

            // Single character punctuators
            Token::Equal => write!(f, "="),
            Token::Add => write!(f, "+"),
            Token::Sub => write!(f, "-"),
            Token::Mul => write!(f, "*"),
            Token::Div => write!(f, "/"),
            Token::Comma => write!(f, ","),
            Token::Dot => write!(f, "."),
            Token::Colon => write!(f, ":"),
            Token::Semicolon => write!(f, ";"),
            Token::Backslash => write!(f, "\\"),
            Token::Exclaim => write!(f, "!"),
            Token::Question => write!(f, "?"),
            Token::Hash => write!(f, "#"),
            Token::Modulo => write!(f, "%"),
            Token::ParenLeft => write!(f, "("),
            Token::ParenRight => write!(f, ")"),
            Token::BraceLeft => write!(f, "{{"),
            Token::BraceRight => write!(f, "}}"),
            Token::SquareLeft => write!(f, "["),
            Token::SquareRight => write!(f, "]"),
            Token::AngleLeft => write!(f, "<"),
            Token::AngleRight => write!(f, ">"),
            Token::BitAnd => write!(f, "&"),
            Token::BitXor => write!(f, "^"),
            Token::BitOr => write!(f, "|"),
            Token::BitNot => write!(f, "~"),
            Token::SingleQuote => write!(f, "'"),
            Token::Quote => write!(f, "\""),
            Token::At => write!(f, "@"),
            Token::Dollar => write!(f, "$"),

            // Multi-character punctuators
            Token::Range => write!(f, ".."),
            Token::RangeEqual => write!(f, "..="),
            Token::Ellipsis => write!(f, "..."),
            Token::AddEqual => write!(f, "+="),
            Token::SubEqual => write!(f, "-="),
            Token::MulEqual => write!(f, "*="),
            Token::DivEqual => write!(f, "/="),
            Token::GreaterEqual => write!(f, ">="),
            Token::LessEqual => write!(f, "<="),
            Token::Arrow => write!(f, "->"),
            Token::ShipRight => write!(f, "=>"),
            Token::LogicalAnd => write!(f, "&&"),
            Token::LogicalOr => write!(f, "||"),
            Token::CompareEqual => write!(f, "=="),
            Token::CompareNotEqual => write!(f, "!="),
            Token::ShiftLeft => write!(f, "<<"),
            Token::ShiftRight => write!(f, ">>"),

            // Literals
            Token::IntegerLiteral(s) => write!(f, "{s}"),
            Token::FloatingPointLiteral(s) => write!(f, "{s}"),
            Token::StringLiteral(s) => write!(f, "{s}"),

            // Comments
            Token::LineComment(s) => write!(f, "{s}"),
            Token::BlockComment(s) => write!(f, "{s}"),
        }
    }
}

#[derive(Default, Debug, Clone, Copy)]
pub struct Position {
    pub line: usize,
    pub col: usize,
}

impl Position {
    pub fn new(line: usize, col: usize) -> Self { Self { line, col } }
}

#[derive(Default, Debug, Clone, Copy)]
pub struct Location {
    pub begin: Position,
    pub end: Position,
}

impl Location {
    pub fn new(begin: Position, end: Position) -> Self { Self { begin, end } }
}
