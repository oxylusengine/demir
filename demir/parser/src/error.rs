use lexer::token::Location;

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

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result { write!(f, "{}", self.kind) }
}
