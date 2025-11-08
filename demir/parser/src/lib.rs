use ast::{Expression, FunctionParam, Identifier, Literal, Statement};
use lexer::{
    lexer::Lexer,
    token::{Location, Token},
};

use crate::{
    error::ParseError,
    precedence::{Precedence, token_to_assignment_kind, token_to_binary_op, token_to_precedence},
};

mod error;
mod precedence;

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

    pub fn parse(&mut self) -> ParseResult<Statement> {
        let mut statements = Vec::new();
        while !self.peek_is(Token::Eof) {
            statements.push(self.parse_single_stmt()?);
        }

        Ok(Statement::Multi(statements))
    }

    fn parse_identifier(&mut self) -> ParseResult<Identifier> {
        let (token, location) = self.advance()?;
        let identifier = match token {
            Token::Identifier(s) => s,
            _ => return Err(ParseError::unexpected("an identifier", token, location)),
        };

        Ok(Identifier(identifier.to_string()))
    }

    fn parse_stmt(&mut self) -> ParseResult<Statement> {
        if self.peek_is(Token::BraceLeft) {
            self.parse_multi_stmt()
        } else {
            self.parse_single_stmt()
        }
    }

    fn parse_multi_stmt(&mut self) -> ParseResult<Statement> {
        self.expect_next(Token::BraceLeft)?;

        let mut statements = Vec::new();
        while !self.peek_is(Token::Eof) {
            if self.peek_is(Token::BraceRight) {
                break;
            }

            statements.push(self.parse_single_stmt()?);
        }

        self.expect_next(Token::BraceRight)?;

        Ok(Statement::Multi(statements))
    }

    fn parse_single_stmt(&mut self) -> ParseResult<Statement> {
        let (token, location) = self.peek().ok_or(ParseError::end_of_file())?;

        match token {
            Token::Var => self.parse_decl_var_stmt(),
            Token::Fn => self.parse_decl_function_stmt(),
            Token::Identifier(_)
            | Token::IntegerLiteral(_)
            | Token::FloatingPointLiteral(_)
            | Token::StringLiteral(_) => self.parse_expr_stmt(),
            _ => Err(ParseError::invalid_token(location)),
        }
    }

    fn parse_expr_stmt(&mut self) -> ParseResult<Statement> {
        let lhs_expr = self.parse_expr(Precedence::default())?;
        self.expect_next(Token::Semicolon)?;

        Ok(Statement::Expression(lhs_expr))
    }

    fn parse_decl_var_stmt(&mut self) -> ParseResult<Statement> {
        self.expect_next(Token::Var)?;
        let identifier = self.parse_identifier()?;

        let mut type_expr = Option::None;
        if self.peek_is(Token::Colon) {
            self.advance()?;
            type_expr = Some(self.parse_identifier_expr()?);
        }

        let mut initial_expr = Option::None;
        if self.peek_is(Token::Equal) {
            self.advance()?;
            initial_expr = Some(self.parse_expr(Precedence::default())?);
        }

        self.expect_next(Token::Semicolon)?;

        Ok(Statement::DeclVar {
            identifier,
            type_expr,
            initial_expr,
        })
    }

    fn parse_decl_function_stmt(&mut self) -> ParseResult<Statement> {
        self.expect_next(Token::Fn)?;
        let identifier = self.parse_identifier()?;
        self.expect_next(Token::ParenLeft)?;

        let mut params = Vec::new();
        let mut first_param = false;
        while !self.peek_is(Token::Eof) {
            if self.peek_is(Token::ParenRight) {
                break;
            }

            if !first_param {
                self.expect_next(Token::Comma)?;
            }

            let param_identifier = self.parse_identifier()?;
            self.expect_next(Token::Colon)?;
            let param_type_expr = self.parse_expr(Precedence::default())?;
            params.push(FunctionParam(param_identifier, param_type_expr));

            first_param = false;
        }

        self.expect_next(Token::ParenRight)?;

        // TODO: Return type

        let body = self.parse_stmt()?;

        Ok(Statement::DeclFunction {
            identifier,
            params,
            body: Box::new(body),
        })
    }

    fn parse_expr(&mut self, precedence: Precedence) -> ParseResult<Expression> {
        let postfix_expr = self.parse_postfix_expr()?;
        self.parse_expr_with_precedence(precedence, postfix_expr)
    }

    fn parse_expr_with_precedence(
        &mut self, precedence: Precedence, lhs_expression: Expression,
    ) -> ParseResult<Expression> {
        let mut expression = lhs_expression;

        while !self.peek_is(Token::Eof) {
            let first_precedence = {
                let (first_token, _) = self.peek().ok_or(ParseError::end_of_file())?;
                token_to_precedence(&first_token)
            };
            if first_precedence < precedence {
                break;
            }

            let (first_token, first_location) = self.advance()?;
            let assign_kind = token_to_assignment_kind(&first_token).ok_or(ParseError::unexpected(
                "an assignment",
                first_token,
                first_location,
            ));
            let binary_op =
                token_to_binary_op(&first_token).ok_or(ParseError::unexpected("a binary", first_token, first_location));

            let mut rhs_expression = self.parse_postfix_expr()?;

            while !self.peek_is(Token::Eof) {
                let (next_token, _) = self.peek().ok_or(ParseError::end_of_file())?;
                let next_precedence = token_to_precedence(&next_token);
                let associate_right = next_precedence == Precedence::Assignment;

                if associate_right {
                    if next_precedence < first_precedence {
                        break;
                    }
                } else if next_precedence <= first_precedence {
                    break;
                }

                rhs_expression = self.parse_expr_with_precedence(next_precedence, rhs_expression)?;
            }

            expression = if first_precedence == Precedence::Assignment {
                Expression::Assign {
                    kind: assign_kind?,
                    lhs_expr: Box::new(expression),
                    rhs_expr: Box::new(rhs_expression),
                }
            } else {
                Expression::Binary {
                    op: binary_op?,
                    lhs_expr: Box::new(expression),
                    rhs_expr: Box::new(rhs_expression),
                }
            };
        }

        Ok(expression)
    }

    fn parse_prefix_expr(&mut self) -> ParseResult<Expression> {
        let (token, location) = self.peek().ok_or(ParseError::end_of_file())?;
        match token {
            Token::Identifier(_) => self.parse_identifier_expr(),
            Token::IntegerLiteral(_) | Token::FloatingPointLiteral(_) | Token::StringLiteral(_) => {
                self.parse_literal_expr()
            },
            _ => Err(ParseError::unexpected("a prefix expression", token, location)),
        }
    }

    fn parse_postfix_expr(&mut self) -> ParseResult<Expression> {
        let mut prefix_expr = self.parse_prefix_expr()?;
        loop {
            let (token, _) = self.peek().ok_or(ParseError::end_of_file())?;
            match token {
                Token::ParenLeft => {
                    prefix_expr = self.parse_call_function_expr(prefix_expr)?;
                },
                Token::Dot => {
                    // prefix_expr = self.parse_access_field_expr(prefix_expr)?;
                },
                _ => break,
            }
        }

        Ok(prefix_expr)
    }

    fn parse_expr_list(&mut self, terminator: Token) -> ParseResult<Vec<Expression>> {
        let mut expressions = Vec::new();

        let mut is_first = true;
        while !self.peek_is(terminator) {
            if !is_first {
                self.expect_next(Token::Comma)?;
            }

            is_first = false;
            expressions.push(self.parse_expr(Precedence::Assignment)?);
        }

        Ok(expressions)
    }

    fn parse_identifier_expr(&mut self) -> ParseResult<Expression> {
        let identifier = self.parse_identifier()?;
        Ok(Expression::Identifier(identifier))
    }

    fn parse_literal_expr(&mut self) -> ParseResult<Expression> {
        let (token, location) = self.advance()?;

        let literal = match token {
            Token::IntegerLiteral(s) => Literal::Integer(s.parse::<i64>().unwrap()),
            Token::FloatingPointLiteral(s) => Literal::Float(s.parse::<f64>().unwrap()),
            Token::StringLiteral(s) => Literal::String(s.to_string()),
            _ => return Err(ParseError::unexpected("a literal", token, location)),
        };

        Ok(Expression::Literal(literal))
    }

    fn parse_call_function_expr(&mut self, callee_expr: Expression) -> ParseResult<Expression> {
        self.expect_next(Token::ParenLeft)?;
        let param_exprs = self.parse_expr_list(Token::ParenRight)?;
        self.expect_next(Token::ParenRight)?;

        Ok(Expression::CallFunction {
            callee: Box::new(callee_expr),
            parameters: param_exprs,
        })
    }

    fn peek(&mut self) -> &Option<(Token<'_>, Location)> {
        if self.peeked_token.is_none() {
            self.peeked_token = Some(self.lexer.advance());
        }

        &self.peeked_token
    }

    fn peek_is(&mut self, token: Token<'_>) -> bool {
        let Some((peeked_token, _)) = self.peek() else {
            return false;
        };

        &token == peeked_token
    }

    fn expect_next(&mut self, expected_token: Token<'_>) -> ParseResult<(Token<'_>, Location)> {
        let (token, location) = self.advance()?;
        if token == expected_token {
            return Ok((token, location));
        }

        Err(ParseError::unexpected(expected_token, token, location))
    }

    fn advance(&mut self) -> ParseResult<(Token<'_>, Location)> {
        if self.peeked_token.is_some() {
            return Ok(self.peeked_token.take().unwrap());
        }

        let (token, location) = self.lexer.advance();
        match token {
            Token::Eof => Err(ParseError::end_of_file()),
            _ => Ok((token, location)),
        }
    }
}
