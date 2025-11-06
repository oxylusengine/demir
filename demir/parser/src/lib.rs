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
    peeked_token: Option<(Token<'a>, Location)>,
}

impl<'a> Parser<'a> {
    pub fn new(buffer_view: &'a str) -> Self {
        Self {
            lexer: Lexer::new(buffer_view),
            peeked_token: None,
        }
    }

    fn peek(&mut self) -> &Option<(Token<'a>, Location)> {
        if self.peeked_token.is_none() {
            self.peeked_token = Some(self.lexer.advance());
        }

        &self.peeked_token
    }

    fn expect(&mut self, expected_token: Token) -> ParseResult<Location> {
        let (token, location) = self.advance()?;
        if token == expected_token {
            Ok(location)
        }

        Err(ParseError::unexpected(expected_token, got, location))
    }

    pub fn advance(&mut self) -> ParseResult<(Token<'a>, Location)> {
        if self.peeked_token.is_some() {
            return Ok(self.peeked_token.take().unwrap());
        }

        let (token, location) = self.lexer.advance();
        match token {
            Token::Eof => Err(ParseError::end_of_file(location)),
            _ => Ok((token, location)),
        }
    }
}
