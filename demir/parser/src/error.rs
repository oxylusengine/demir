use lexer::token::Location;

#[derive(Clone, Debug)]
pub enum ParseErrorKind {
    EndOfFile,
    InvalidToken,
    UnexpectedToken { expected: String, got: String },
    UndefinedAttribute(String),
}

impl std::fmt::Display for ParseErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::EndOfFile => write!(f, "end of file"),
            Self::InvalidToken => write!(f, "invalid token"),
            Self::UnexpectedToken { expected, got, .. } => write!(f, "expected '{expected}' got '{got}'"),
            Self::UndefinedAttribute(s) => write!(f, "undefined attribute {s}"),
        }
    }
}

#[derive(Clone, Debug)]
pub struct ParseError {
    pub location: Location,
    pub kind: ParseErrorKind,
}

impl ParseError {
    pub fn end_of_file() -> Self {
        Self {
            kind: ParseErrorKind::EndOfFile,
            location: Location::default(),
        }
    }

    pub fn invalid_token(location: Location) -> Self {
        Self {
            kind: ParseErrorKind::InvalidToken,
            location,
        }
    }

    pub fn unexpected(expected: impl std::fmt::Display, got: impl std::fmt::Display, location: Location) -> Self {
        Self {
            kind: ParseErrorKind::UnexpectedToken {
                expected: expected.to_string(),
                got: got.to_string(),
            },
            location,
        }
    }

    pub fn undefined_attrib(attrib: impl std::fmt::Display, location: Location) -> Self {
        Self {
            kind: ParseErrorKind::UndefinedAttribute(attrib.to_string()),
            location,
        }
    }
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let begin = self.location.begin;
        write!(f, "Parser error at {}:{}: {}", begin.line, begin.col, self.kind)
    }
}

impl std::error::Error for ParseError {}
