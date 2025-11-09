mod error;
mod symbol_table;

use ast::{BinaryOp, Expression, FunctionParam, Identifier, Literal, Statement};

use crate::{error::SemaError, symbol_table::SymbolTable};

#[derive(Debug, Clone)]
struct SymbolInfo {
    ty: ResolvedType,
    kind: SymbolKind,
}

#[derive(Debug, Clone)]
enum SymbolKind {
    Variable { is_mutable: bool },
    Function,
    Parameter,
}

#[derive(Debug, Clone, PartialEq)]
enum ResolvedType {
    Void,
    Int,
    Float,
    Bool,
    String,
    Function {
        params: Vec<ResolvedType>,
        return_type: Box<ResolvedType>,
    },
}

impl std::fmt::Display for ResolvedType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ResolvedType::Void => write!(f, "void"),
            ResolvedType::Int => write!(f, "i32"),
            ResolvedType::Float => write!(f, "f32"),
            ResolvedType::Bool => write!(f, "bool"),
            ResolvedType::String => write!(f, "str"),
            ResolvedType::Function { .. } => write!(f, "function"),
        }
    }
}

struct SemanticAnalyzer {
    symbols: SymbolTable<Identifier, SymbolInfo>,
}

impl SemanticAnalyzer {
    fn new() -> Self {
        Self {
            symbols: SymbolTable::new(),
        }
    }

    fn literal_type(&self, lit: &Literal) -> ResolvedType {
        match lit {
            Literal::String(_) => ResolvedType::String,
            Literal::Integer(_) => ResolvedType::Int,
            Literal::Float(_) => ResolvedType::Float,
            Literal::Bool(_) => ResolvedType::Bool,
        }
    }

    fn resolve_type_expr(&self, expr: &Expression) -> Result<ResolvedType, SemaError> {
        match expr {
            Expression::Identifier(ident) => match ident.0.as_str() {
                "i32" => Ok(ResolvedType::Int),
                "f32" => Ok(ResolvedType::Float),
                "bool" => Ok(ResolvedType::Bool),
                "str" => Ok(ResolvedType::String),
                "void" => Ok(ResolvedType::Void),
                _ => Err(SemaError::undefined_type(ident)),
            },
            _ => Err(SemaError::type_mismatch("a type", "an expression")),
        }
    }

    fn binary_result_type(&self, op: &BinaryOp, operand_ty: &ResolvedType) -> ResolvedType {
        match op {
            BinaryOp::CompEq
            | BinaryOp::CompNotEq
            | BinaryOp::CompGreater
            | BinaryOp::CompLess
            | BinaryOp::CompGreaterEq
            | BinaryOp::CompLessEq
            | BinaryOp::CompAnd
            | BinaryOp::CompOr => ResolvedType::Bool,
            _ => operand_ty.clone(),
        }
    }

    fn check_expression(&mut self, expr: &Expression) -> Result<ResolvedType, SemaError> {
        match expr {
            Expression::Literal(lit) => Ok(self.literal_type(lit)),

            Expression::Identifier(ident) => {
                let symbol = self.symbols.lookup(ident).ok_or(SemaError::undefined_var(ident))?;
                Ok(symbol.ty.clone())
            },

            Expression::Binary { op, lhs_expr, rhs_expr } => {
                let lhs_ty = self.check_expression(lhs_expr)?;
                let rhs_ty = self.check_expression(rhs_expr)?;

                if lhs_ty != rhs_ty {
                    return Err(SemaError::type_mismatch(lhs_ty, rhs_ty));
                }

                Ok(self.binary_result_type(op, &lhs_ty))
            },

            Expression::Assign {
                kind,
                lhs_expr,
                rhs_expr,
            } => {
                let lhs_ty = self.check_expression(lhs_expr)?;
                let rhs_ty = self.check_expression(rhs_expr)?;

                if lhs_ty != rhs_ty {
                    return Err(SemaError::type_mismatch(lhs_ty, rhs_ty));
                }

                Ok(lhs_ty)
            },

            Expression::CallFunction { callee, parameters } => {
                let callee_ty = self.check_expression(callee)?;

                match callee_ty {
                    ResolvedType::Function { params, return_type } => {
                        if params.len() != parameters.len() {
                            return Err(SemaError::wrong_arg_count(params.len(), parameters.len()));
                        }

                        for (param_ty, arg_expr) in params.iter().zip(parameters) {
                            let arg_ty = self.check_expression(arg_expr)?;
                            if param_ty != &arg_ty {
                                return Err(SemaError::type_mismatch(param_ty, arg_ty));
                            }
                        }

                        Ok(*return_type)
                    },
                    _ => Err(SemaError::not_callable()),
                }
            },
        }
    }

    fn analyze_statement(&mut self, stmt: Statement) -> Result<(), SemaError> {
        match stmt {
            Statement::Multi(stmts) => {
                self.symbols.push_scope();

                for stmt in stmts {
                    self.analyze_statement(stmt)?;
                }

                self.symbols.pop_scope();
                Ok(())
            },
            Statement::DeclFunction {
                identifier,
                params,
                body,
            } => {
                if self.symbols.lookup(&identifier).is_some() {
                    return Err(SemaError::redefinition(identifier));
                }

                let param_types: Result<Vec<_>, _> = params
                    .iter()
                    .map(|FunctionParam(_, type_expr)| self.resolve_type_expr(type_expr))
                    .collect();
                let param_types = param_types?;

                self.symbols.define(
                    identifier.clone(),
                    SymbolInfo {
                        ty: ResolvedType::Function {
                            params: param_types.clone(),
                            return_type: Box::new(ResolvedType::Void), // TODO: return types
                        },
                        kind: SymbolKind::Function,
                    },
                );

                self.symbols.push_scope();

                for (FunctionParam(param_name, _), param_ty) in params.iter().zip(param_types.iter()) {
                    self.symbols.define(
                        param_name.clone(),
                        SymbolInfo {
                            ty: param_ty.clone(),
                            kind: SymbolKind::Parameter,
                        },
                    );
                }

                self.analyze_statement(*body)?;

                self.symbols.pop_scope();

                Ok(())
            },
            Statement::DeclVar {
                identifier,
                type_expr,
                initial_expr,
            } => {
                if self.symbols.lookup(&identifier).is_some() {
                    return Err(SemaError::redefinition(identifier));
                }

                let explicit_ty = if let Some(type_expr) = type_expr {
                    Some(self.resolve_type_expr(&type_expr)?)
                } else {
                    None
                };

                let inferred_type = if let Some(initial_expr) = initial_expr {
                    Some(self.check_expression(&initial_expr)?)
                } else {
                    None
                };

                let ty = match (explicit_ty, inferred_type) {
                    (Some(explicit), Some(inferred)) => {
                        if explicit != inferred {
                            return Err(SemaError::type_mismatch(explicit, inferred));
                        }
                        explicit
                    },
                    (Some(explicit), None) => explicit,
                    (None, Some(inferred)) => inferred,
                    (None, None) => {
                        return Err(SemaError::cannot_infer(identifier));
                    },
                };

                self.symbols.define(
                    identifier.clone(),
                    SymbolInfo {
                        ty,
                        kind: SymbolKind::Variable { is_mutable: true },
                    },
                );

                Ok(())
            },
            Statement::Expression(expression) => {
                self.check_expression(&expression)?;
                Ok(())
            },
        }
    }
}

pub fn analyze(stmt: Statement) -> Result<(), SemaError> {
    let mut anaylzer = SemanticAnalyzer::new();
    anaylzer.analyze_statement(stmt)?;

    Ok(())
}
