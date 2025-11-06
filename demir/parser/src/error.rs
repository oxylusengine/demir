use lexer::token::{Location, Token};

#[derive(Clone, Debug)]
pub enum ParseErrorKind {
    EndOfFile,
    InvalidToken,
    UnexpectedToken { expected: String, got: String },
}

impl std::fmt::Display for ParseErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::EndOfFile => write!(f, "end of file"),
            Self::InvalidToken => write!(f, "invalid token"),
            Self::UnexpectedToken { expected, got, .. } => {
                write!(f, "expected '{expected}' got '{got}'")
            },
        }
    }
}

#[derive(Clone, Debug)]
pub struct ParseError {
    pub location: Location,
    pub kind: ParseErrorKind,
}

impl ParseError {
    pub fn end_of_file(location: Location) -> Self {
        Self {
            kind: ParseErrorKind::EndOfFile,
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
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result { write!(f, "{}", self.kind) }
}
