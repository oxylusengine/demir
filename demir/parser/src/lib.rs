use ast::Statement;
use lexer::{
    lexer::Lexer,
    token::{Location, Token},
};

use crate::error::{ParseError, ParseErrorKind};

mod error;

type ParseResult<T> = Result<T, ParseError>;

pub struct Parser<'a> {
    lexer: Lexer<'a>,
}

impl<'a> Parser<'a> {
    pub fn new(buffer_view: &'a str) -> Self {
        Self {
            lexer: Lexer::new(buffer_view),
        }
    }

    pub fn advance(&mut self) -> ParseResult<(Token<'a>, Location)> {
        let (token, location) = self.lexer.advance();
        match token {
            Token::Eof => Err(ParseError {
                kind: ParseErrorKind::EndOfFile,
                location,
            }),
            _ => Ok((token, location)),
        }
    }
}
