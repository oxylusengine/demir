#![allow(dead_code)]

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Token<'a> {
    /// End of the lexing file
    Eof,
    Identifier(&'a str),
    /// Keywords
    Null,
    Var,
    Fn,
    /// Punctuators
    Slash, // /
    Semicolon,  // ;
    ParenLeft,  // (
    ParenRight, // )
    BraceLeft,  // {
    BraceRight, // }

    /// Literals
    IntegerLiteral(&'a str),
    FloatingPointLiteral(&'a str),
    StringLiteral(&'a str),

    LineComment(&'a str),
    BlockComment(&'a str),
}

impl std::fmt::Display for Token<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Eof => write!(f, "eof"),
            Token::Identifier(s) => write!(f, "{s}"),
            Token::Null => write!(f, "null"),
            Token::Var => write!(f, "var"),
            Token::Fn => write!(f, "fn"),
            Token::Slash => write!(f, "/"),
            Token::Semicolon => write!(f, ";"),
            Token::ParenLeft => write!(f, "("),
            Token::ParenRight => write!(f, ")"),
            Token::BraceLeft => write!(f, "{{"),
            Token::BraceRight => write!(f, "}}"),
            Token::IntegerLiteral(s) => write!(f, "{}", s),
            Token::FloatingPointLiteral(s) => write!(f, "{}", s),
            Token::StringLiteral(s) => write!(f, "{}", s),
            Token::LineComment(s) => write!(f, "{}", s),
            Token::BlockComment(s) => write!(f, "{}", s),
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
