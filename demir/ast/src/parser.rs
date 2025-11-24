use core::types::Identifier;

use lexer::{
    Lexer,
    token::{Location, Token},
};

use crate::{
    AST,
    Attrib,
    Expression,
    ExpressionId,
    FunctionParam,
    InfixOperator,
    Literal,
    Statement,
    error::ParseError,
    precedence::{Precedence, token_to_assignment_kind, token_to_binary_op, token_to_precedence, token_to_range_kind},
};

pub type ParseResult<T> = Result<T, ParseError>;

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    peeked_token: Option<(Token<'a>, Location)>,
    expressions: Vec<Expression>,
}

impl<'a> Parser<'a> {
    pub fn new(buffer_view: &'a str) -> Self {
        Self {
            lexer: Lexer::new(buffer_view),
            peeked_token: None,
            expressions: Vec::new(),
        }
    }

    pub fn make_expr(&mut self, expr: Expression) -> ExpressionId {
        let expr_id = self.expressions.len();
        self.expressions.push(expr);

        expr_id
    }

    pub fn parse(&mut self) -> ParseResult<AST> {
        let mut statements = Vec::new();
        while !self.peek_is(Token::Eof) {
            statements.push(self.parse_single_stmt()?);
        }

        Ok(AST::new(
            Statement::Multi(statements),
            std::mem::take(&mut self.expressions),
        ))
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
        let mut attributes = Vec::new();

        loop {
            let (token, location) = self.peek().ok_or(ParseError::end_of_file())?;
            match token {
                Token::At => {
                    let attrib = self.parse_attributes()?;
                    attributes.push(attrib);
                    continue;
                },
                Token::BraceLeft => return self.parse_multi_stmt(),
                Token::Let | Token::Var => return self.parse_decl_var_stmt(),
                Token::Fn => return self.parse_decl_function_stmt(attributes),
                Token::Return => return self.parse_return_stmt(),
                Token::If => return self.parse_if_stmt(),
                Token::While => return self.parse_while_stmt(),
                Token::For => return self.parse_for_stmt(),
                Token::Continue => {
                    self.advance()?;
                    self.expect_next(Token::Semicolon)?;
                    return Ok(Statement::Continue);
                },
                Token::Break => {
                    self.advance()?;
                    self.expect_next(Token::Semicolon)?;
                    return Ok(Statement::Break);
                },
                Token::Identifier(_)
                | Token::IntegerLiteral(_)
                | Token::FloatingPointLiteral(_)
                | Token::StringLiteral(_) => return self.parse_expr_stmt(),
                _ => return Err(ParseError::invalid_token(location)),
            }
        }
    }

    fn parse_expr_stmt(&mut self) -> ParseResult<Statement> {
        let lhs_expr = self.parse_expr(Precedence::default())?;
        self.expect_next(Token::Semicolon)?;

        Ok(Statement::Expression(lhs_expr))
    }

    fn parse_decl_var_stmt(&mut self) -> ParseResult<Statement> {
        let (token, location) = self.advance()?;
        let is_mutable = match token {
            Token::Var => true,
            Token::Let => false,
            _ => Err(ParseError::unexpected("let, var", token, location))?,
        };

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
            is_mutable,
        })
    }

    fn parse_decl_function_stmt(&mut self, attributes: Vec<Attrib>) -> ParseResult<Statement> {
        self.expect_next(Token::Fn)?;
        let identifier = self.parse_identifier()?;
        self.expect_next(Token::ParenLeft)?;

        let mut params = Vec::new();
        let mut first_param = true;
        while !self.peek_is(Token::Eof) {
            if self.peek_is(Token::ParenRight) {
                break;
            }

            if !first_param {
                self.expect_next(Token::Comma)?;
            }

            let param_identifier = self.parse_identifier()?;
            self.expect_next(Token::Colon)?;
            let param_type_expr = self.parse_prefix_expr()?;
            params.push(FunctionParam(param_identifier, param_type_expr));

            first_param = false;
        }

        self.expect_next(Token::ParenRight)?;

        let return_expr = if self.peek_is(Token::Arrow) {
            self.advance()?;

            Some(self.parse_expr(Precedence::default())?)
        } else {
            None
        };

        let is_ext = attributes.iter().any(|attrib| matches!(attrib, Attrib::External));

        let mut body = None;
        if !is_ext {
            body = Some(Box::new(self.parse_stmt()?));
        } else {
            self.expect_next(Token::Semicolon)?;
        }

        Ok(Statement::DeclFunction {
            identifier,
            params,
            body,
            return_expr,
            external: is_ext,
        })
    }

    fn parse_return_stmt(&mut self) -> ParseResult<Statement> {
        self.expect_next(Token::Return)?;
        let expr = self.parse_expr(Precedence::default())?;
        self.expect_next(Token::Semicolon)?;

        Ok(Statement::Return(expr))
    }

    fn parse_if_stmt(&mut self) -> ParseResult<Statement> {
        self.expect_next(Token::If)?;
        let condition = self.parse_expr(Precedence::default())?;
        let true_case = Box::new(self.parse_stmt()?);

        let false_case = if self.peek_is(Token::Else) {
            self.advance()?;
            if self.peek_is(Token::If) {
                Some(Box::new(self.parse_if_stmt()?))
            } else {
                Some(Box::new(self.parse_stmt()?))
            }
        } else {
            None
        };

        Ok(Statement::If {
            condition,
            true_case,
            false_case,
        })
    }

    fn parse_while_stmt(&mut self) -> ParseResult<Statement> {
        self.expect_next(Token::While)?;
        let condition = self.parse_expr(Precedence::default())?;
        let true_case = Box::new(self.parse_stmt()?);

        Ok(Statement::While { condition, true_case })
    }

    fn parse_for_stmt(&mut self) -> ParseResult<Statement> {
        self.expect_next(Token::For)?;
        let iter = self.parse_identifier()?;
        self.expect_next(Token::In)?;
        let range = self.parse_expr(Precedence::default())?;
        let body = Box::new(self.parse_stmt()?);

        Ok(Statement::For { iter, range, body })
    }

    fn parse_expr(&mut self, precedence: Precedence) -> ParseResult<ExpressionId> {
        let postfix_expr = self.parse_postfix_expr()?;
        self.parse_expr_with_precedence(precedence, postfix_expr)
    }

    fn parse_expr_with_precedence(
        &mut self, precedence: Precedence, lhs_expression: ExpressionId,
    ) -> ParseResult<ExpressionId> {
        let mut expression = lhs_expression;

        while !self.peek_is(Token::Eof) {
            let first_precedence = {
                let (first_token, _) = self.peek().ok_or(ParseError::end_of_file())?;
                token_to_precedence(first_token)
            };
            if first_precedence < precedence {
                break;
            }

            let (first_token, first_location) = self.advance()?;
            let operator_info = match first_precedence {
                Precedence::Assignment => {
                    let kind = token_to_assignment_kind(first_token).ok_or(ParseError::unexpected(
                        "an assignment",
                        first_token,
                        first_location,
                    ))?;
                    InfixOperator::Assignment(kind)
                },
                Precedence::Range => {
                    let kind = token_to_range_kind(first_token).ok_or(ParseError::unexpected(
                        "a range",
                        first_token,
                        first_location,
                    ))?;
                    InfixOperator::Range(kind)
                },
                _ => {
                    let op = token_to_binary_op(first_token).ok_or(ParseError::unexpected(
                        "a binary",
                        first_token,
                        first_location,
                    ))?;
                    InfixOperator::Binary(op)
                },
            };

            let mut rhs_expr = self.parse_postfix_expr()?;

            while !self.peek_is(Token::Eof) {
                let (next_token, _) = self.peek().ok_or(ParseError::end_of_file())?;
                let next_precedence = token_to_precedence(next_token);
                let associate_right = next_precedence == Precedence::Assignment;

                if associate_right {
                    if next_precedence < first_precedence {
                        break;
                    }
                } else if next_precedence <= first_precedence {
                    break;
                }

                rhs_expr = self.parse_expr_with_precedence(next_precedence, rhs_expr)?;
            }

            expression = match operator_info {
                InfixOperator::Assignment(kind) => self.make_expr(Expression::Assign {
                    kind,
                    lhs_expr: expression,
                    rhs_expr,
                }),
                InfixOperator::Range(kind) => self.make_expr(Expression::Range {
                    kind,
                    lhs_expr: expression,
                    rhs_expr,
                }),
                InfixOperator::Binary(op) => self.make_expr(Expression::Binary {
                    op,
                    lhs_expr: expression,
                    rhs_expr,
                }),
            };
        }

        Ok(expression)
    }

    fn parse_prefix_expr(&mut self) -> ParseResult<ExpressionId> {
        let (token, location) = self.peek().ok_or(ParseError::end_of_file())?;
        match token {
            Token::BitAnd => self.parse_reference_expr(),
            Token::Identifier(_) => self.parse_identifier_expr(),
            Token::IntegerLiteral(_)
            | Token::FloatingPointLiteral(_)
            | Token::StringLiteral(_)
            | Token::True
            | Token::False => self.parse_literal_expr(),
            _ => Err(ParseError::unexpected("a prefix expression", token, location)),
        }
    }

    fn parse_postfix_expr(&mut self) -> ParseResult<ExpressionId> {
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

    fn parse_expr_list(&mut self, terminator: Token) -> ParseResult<Vec<ExpressionId>> {
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

    fn parse_identifier_expr(&mut self) -> ParseResult<ExpressionId> {
        let identifier = self.parse_identifier()?;
        Ok(self.make_expr(Expression::Identifier(identifier)))
    }

    fn parse_literal_expr(&mut self) -> ParseResult<ExpressionId> {
        let (token, location) = self.advance()?;

        let literal = match token {
            Token::IntegerLiteral(s) => Literal::Integer(s.parse::<i32>().unwrap()),
            Token::FloatingPointLiteral(s) => Literal::Float(s.parse::<f32>().unwrap()),
            Token::StringLiteral(s) => Literal::String(s.to_string()),
            Token::True => Literal::Bool(true),
            Token::False => Literal::Bool(false),
            _ => return Err(ParseError::unexpected("a literal", token, location)),
        };

        Ok(self.make_expr(Expression::Literal(literal)))
    }

    fn parse_call_function_expr(&mut self, callee_expr: ExpressionId) -> ParseResult<ExpressionId> {
        self.expect_next(Token::ParenLeft)?;
        let param_exprs = self.parse_expr_list(Token::ParenRight)?;
        self.expect_next(Token::ParenRight)?;

        Ok(self.make_expr(Expression::CallFunction {
            callee: callee_expr,
            parameters: param_exprs,
        }))
    }

    fn parse_reference_expr(&mut self) -> ParseResult<ExpressionId> {
        self.expect_next(Token::BitAnd)?;

        let is_mutable = self.peek_is(Token::Mut);
        let referent = self.parse_prefix_expr()?;

        Ok(self.make_expr(Expression::Reference {
            referent,
            is_mutable,
        }))
    }

    fn parse_attributes(&mut self) -> ParseResult<Attrib> {
        self.expect_next(Token::At)?;

        let start_pos = self.lexer.position();
        let attrib = self.parse_identifier()?;
        let end_pos = self.lexer.position();
        let location = Location::new(start_pos, end_pos);

        match attrib.0.as_str() {
            "external" => Ok(Attrib::External),
            _ => Err(ParseError::undefined_attrib(attrib, location)),
        }
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
