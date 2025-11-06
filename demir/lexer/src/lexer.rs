use super::token::{Location, Position, Token};

pub struct Lexer<'a> {
    buffer_view: &'a str,
    offset: usize,
    line: usize,
    line_offset: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(buffer_view: &'a str) -> Self {
        Self {
            buffer_view,
            offset: 0,
            line: 0,
            line_offset: 0,
        }
    }

    pub fn position(&self) -> Position { Position::new(self.line + 1, self.offset - self.line_offset + 1) }

    fn peek(&self, look_ahead: usize) -> char { self.buffer_view.chars().nth(self.offset + look_ahead).unwrap_or('\0') }

    fn consume(&mut self) {
        assert_ne!(self.peek(0), '\n', "Unexpected new line");
        self.offset += 1;
    }

    fn consume_multiline(&mut self) {
        if self.peek(0) == '\n' {
            self.line += 1;
            self.line_offset = self.offset + 1;
        }

        self.offset += 1;
    }

    fn consume_identifier(&mut self) -> Token<'a> {
        let start_offset = self.offset;

        self.consume();
        while self.peek(0).is_ascii_alphabetic() {
            self.consume();
        }

        let identifier_str = &self.buffer_view[start_offset..self.offset];
        match identifier_str {
            "null" => Token::Null,
            "var" => Token::Var,
            "fn" => Token::Fn,
            _ => Token::Identifier(identifier_str),
        }
    }

    fn consume_number(&mut self) -> Token<'a> {
        let mut has_dot = false;
        let mut has_digits_after_dot = false;
        loop {
            match self.peek(0) {
                '0'..='9' => {
                    self.consume();
                    if has_dot {
                        has_digits_after_dot = true;
                    }
                },
                '.' => {
                    if has_dot {
                        break;
                    }

                    if !self.peek(1).is_ascii_digit() {
                        break;
                    }

                    has_dot = true;
                    self.consume();
                },
                _ => break,
            }
        }

        if has_dot && !has_digits_after_dot {
            panic!()
        }

        if has_dot {
            Token::FloatingPointLiteral
        } else {
            Token::IntegerLiteral
        }
    }

    pub fn consume_string_literal(&mut self) -> Token<'a> {
        // Skip quote
        self.consume();
        let start_offset = self.offset;

        loop {
            match self.peek(0) {
                '"' => {
                    let str_lit = &self.buffer_view[start_offset..self.offset];
                    // Skip quote
                    self.consume();
                    return Token::StringLiteral(str_lit);
                },
                '\\' => {
                    self.consume();
                    match self.peek(0) {
                        '0' | 'n' | 'r' | 't' | '\\' | '"' => {
                            self.consume_multiline();
                        },
                        _ => panic!(),
                    }
                },
                _ => self.consume_multiline(),
            }
        }
    }

    pub fn skip_whitespace(&mut self) {
        loop {
            let c = self.peek(0);
            if c.is_whitespace() {
                self.consume_multiline();
            } else {
                break;
            }
        }
    }

    pub fn advance(&mut self) -> (Token<'a>, Location) {
        self.skip_whitespace();

        let start_pos = self.position();
        let c = self.peek(0);
        let token = match c {
            '/' => {
                self.consume();
                Token::Slash
            },
            ';' => {
                self.consume();
                Token::Semicolon
            },
            '(' => {
                self.consume();
                Token::ParenLeft
            },
            ')' => {
                self.consume();
                Token::ParenRight
            },
            '{' => {
                self.consume();
                Token::BraceLeft
            },
            '}' => {
                self.consume();
                Token::BraceRight
            },
            '"' => self.consume_string_literal(),
            c if c.is_ascii_alphabetic() => self.consume_identifier(),
            c if c.is_ascii_digit() => self.consume_number(),
            _ => {
                self.consume();
                Token::Eof
            },
        };

        (token, Location::new(start_pos, self.position()))
    }
}
