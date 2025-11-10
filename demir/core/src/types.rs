#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum BuiltinType {
    I8,
    U8,
    I16,
    U16,
    I32,
    U32,
    I64,
    U64,
    F32,
    F64,
    Bool,
    String,
    Never, // No return type
    Unit,  // Similar to rust's (), useful for error handling
    Function {
        params: Vec<BuiltinType>,
        return_ty: Box<BuiltinType>,
    },
}

impl std::fmt::Display for BuiltinType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BuiltinType::I8 => write!(f, "i8"),
            BuiltinType::U8 => write!(f, "u8"),
            BuiltinType::I16 => write!(f, "i16"),
            BuiltinType::U16 => write!(f, "u16"),
            BuiltinType::I32 => write!(f, "i32"),
            BuiltinType::U32 => write!(f, "u32"),
            BuiltinType::I64 => write!(f, "i64"),
            BuiltinType::U64 => write!(f, "u64"),
            BuiltinType::F32 => write!(f, "f32"),
            BuiltinType::F64 => write!(f, "f64"),
            BuiltinType::Bool => write!(f, "bool"),
            BuiltinType::String => write!(f, "str"),
            BuiltinType::Never => write!(f, "never"),
            BuiltinType::Unit => write!(f, "unit"),
            BuiltinType::Function { .. } => write!(f, "function"),
        }
    }
}
