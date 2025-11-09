#[derive(Clone, Debug)]
pub enum SemaErrorKind {
    UndefinedVariable(String),
    UndefinedType(String),
    TypeMismatch { expected: String, got: String },
    WrongArgCount { expected: usize, got: usize },
    NotCallable,
    Redefinition(String),
    CannotInferType(String),
}

impl std::fmt::Display for SemaErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SemaErrorKind::UndefinedVariable(s) => write!(f, "undefined variable {s}"),
            SemaErrorKind::UndefinedType(s) => write!(f, "undefined type {s}"),
            SemaErrorKind::TypeMismatch { expected, got, .. } => {
                write!(f, "expected '{expected}' got '{got}'")
            },
            SemaErrorKind::WrongArgCount { expected, got } => {
                write!(f, "wrong argument count, expected '{expected}' got '{got}'")
            },
            SemaErrorKind::NotCallable => write!(f, "not callable"),
            SemaErrorKind::Redefinition(s) => write!(f, "redefinition of {s}"),
            SemaErrorKind::CannotInferType(s) => write!(f, "cannot infer type of {s}"),
        }
    }
}

#[derive(Clone, Debug)]
pub struct SemaError {
    pub kind: SemaErrorKind,
}

impl SemaError {
    pub fn undefined_var(var: impl std::fmt::Display) -> Self {
        Self {
            kind: SemaErrorKind::UndefinedVariable(var.to_string()),
        }
    }

    pub fn undefined_type(ty: impl std::fmt::Display) -> Self {
        Self {
            kind: SemaErrorKind::UndefinedType(ty.to_string()),
        }
    }

    pub fn type_mismatch(expected: impl std::fmt::Display, got: impl std::fmt::Display) -> Self {
        Self {
            kind: SemaErrorKind::TypeMismatch {
                expected: expected.to_string(),
                got: got.to_string(),
            },
        }
    }

    pub fn wrong_arg_count(expected: usize, got: usize) -> Self {
        Self {
            kind: SemaErrorKind::WrongArgCount { expected, got },
        }
    }

    pub fn not_callable() -> Self {
        Self {
            kind: SemaErrorKind::NotCallable,
        }
    }

    pub fn redefinition(what: impl std::fmt::Display) -> Self {
        Self {
            kind: SemaErrorKind::Redefinition(what.to_string()),
        }
    }

    pub fn cannot_infer(ty: impl std::fmt::Display) -> Self {
        Self {
            kind: SemaErrorKind::CannotInferType(ty.to_string()),
        }
    }
}

impl std::fmt::Display for SemaError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result { write!(f, "Semantic error: {}", self.kind) }
}

impl std::error::Error for SemaError {}
