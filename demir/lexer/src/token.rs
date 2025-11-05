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
    IntegerLiteral,
    FloatingPointLiteral,
    StringLiteral(&'a str),

    LineComment,
    BlockComment,
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

