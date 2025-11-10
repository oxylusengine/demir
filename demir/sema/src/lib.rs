mod error;
mod symbol_table;

use ast::{AssignmentKind, BinaryOp, Expression, FunctionParam, Identifier, Literal, Statement};
use ir::types::BuiltinType;

use crate::{error::SemaError, symbol_table::SymbolTable};

#[derive(Debug, Clone)]
enum SymbolKind {
    Literal,
    Variable { identifier: Identifier, is_mutable: bool },
    Function(Identifier),
    Parameter(Identifier),
    Aliasing,
}

struct SemanticAnalyzer {
    symbols: SymbolTable<Identifier, (SymbolKind, BuiltinType)>,
}

impl SemanticAnalyzer {
    fn new() -> Self {
        Self {
            symbols: SymbolTable::new(),
        }
    }

    fn is_assignable(&self, ty: &BuiltinType) -> bool {
        match ty {
            BuiltinType::String => false,
            BuiltinType::Never => false,
            BuiltinType::Unit => false,
            BuiltinType::Function { params: _, return_ty } => self.is_assignable(return_ty),
            _ => true,
        }
    }

    fn supports_arithmetic(&self, ty: &BuiltinType) -> bool {
        matches!(
            ty,
            BuiltinType::I8
                | BuiltinType::U8
                | BuiltinType::I16
                | BuiltinType::U16
                | BuiltinType::I32
                | BuiltinType::U32
                | BuiltinType::I64
                | BuiltinType::U64
                | BuiltinType::F32
                | BuiltinType::F64
        )
    }

    fn literal_type(&self, lit: &Literal) -> BuiltinType {
        match lit {
            Literal::String(_) => BuiltinType::String,
            Literal::Integer(_) => BuiltinType::I32,
            Literal::Float(_) => BuiltinType::F32,
            Literal::Bool(_) => BuiltinType::Bool,
        }
    }

    fn resolve_type_expr(&self, expr: &Expression) -> Result<BuiltinType, SemaError> {
        match expr {
            Expression::Identifier(ident) => match ident.0.as_str() {
                "i8" => Ok(BuiltinType::I8),
                "u8" => Ok(BuiltinType::U8),
                "i16" => Ok(BuiltinType::I16),
                "u16" => Ok(BuiltinType::U16),
                "i32" => Ok(BuiltinType::I32),
                "u32" => Ok(BuiltinType::U32),
                "f32" => Ok(BuiltinType::F32),
                "f64" => Ok(BuiltinType::F64),
                "bool" => Ok(BuiltinType::Bool),
                "str" => Ok(BuiltinType::String),
                _ => {
                    let (symbol_kind, ty) = self.symbols.lookup(ident).ok_or(SemaError::undefined_type(ident))?;
                    match symbol_kind {
                        SymbolKind::Aliasing => Ok(ty.clone()),
                        _ => Err(SemaError::undefined_type(ident)),
                    }
                },
            },
            _ => Err(SemaError::type_mismatch("a type", "an expression")),
        }
    }

    fn check_lvalue(&self, expr: &Expression) -> Result<(), SemaError> {
        match expr {
            Expression::Identifier(_) => Ok(()),
            Expression::Literal(_) => Err(SemaError::cannot_assign("a literal")),
            Expression::Assign { .. } => panic!(), // What?
            Expression::Binary { .. } => Err(SemaError::cannot_assign("a binary op")),
            Expression::CallFunction { .. } => Err(SemaError::cannot_assign("a function call")),
        }
    }

    fn check_expression(&mut self, expr: &Expression) -> Result<(SymbolKind, BuiltinType), SemaError> {
        match expr {
            Expression::Literal(lit) => Ok((SymbolKind::Literal, self.literal_type(lit))),

            Expression::Identifier(ident) => {
                let (symbol_kind, ty) = self.symbols.lookup(ident).ok_or(SemaError::undefined_var(ident))?;
                if !matches!(symbol_kind, SymbolKind::Variable { .. } | SymbolKind::Function { .. }) {
                    return Err(SemaError::undefined_var(ident));
                }

                Ok((symbol_kind.clone(), ty.clone()))
            },

            Expression::Binary { op, lhs_expr, rhs_expr } => {
                let (lhs_symbol, lhs_ty) = self.check_expression(lhs_expr)?;
                let (_, rhs_ty) = self.check_expression(rhs_expr)?;

                if lhs_ty != rhs_ty {
                    return Err(SemaError::type_mismatch(lhs_ty, rhs_ty));
                }

                let resolved_ty = match op {
                    BinaryOp::CompEq
                    | BinaryOp::CompNotEq
                    | BinaryOp::CompGreater
                    | BinaryOp::CompLess
                    | BinaryOp::CompGreaterEq
                    | BinaryOp::CompLessEq
                    | BinaryOp::CompAnd
                    | BinaryOp::CompOr => BuiltinType::Bool,
                    _ => lhs_ty,
                };

                Ok((lhs_symbol, resolved_ty))
            },

            Expression::Assign {
                kind,
                lhs_expr,
                rhs_expr,
            } => {
                self.check_lvalue(lhs_expr)?;

                let (lhs_symbol, lhs_ty) = self.check_expression(lhs_expr)?;
                let (_, rhs_ty) = self.check_expression(rhs_expr)?;

                if let SymbolKind::Variable { identifier, is_mutable } = &lhs_symbol {
                    if !is_mutable {
                        return Err(SemaError::cannot_assign_to_immutable(identifier));
                    }
                };

                if lhs_ty != rhs_ty {
                    return Err(SemaError::type_mismatch(lhs_ty, rhs_ty));
                }

                if !self.is_assignable(&lhs_ty) {
                    return Err(SemaError::cannot_assign_to(rhs_ty, lhs_ty));
                }

                match kind {
                    AssignmentKind::Assign => {},
                    AssignmentKind::CompoundAdd
                    | AssignmentKind::CompoundSub
                    | AssignmentKind::CompoundMul
                    | AssignmentKind::CompoundDiv => {
                        if !self.supports_arithmetic(&lhs_ty) {
                            return Err(SemaError::cannot_assign_to(rhs_ty, lhs_ty));
                        }
                    },
                };

                Ok((lhs_symbol, lhs_ty))
            },

            Expression::CallFunction { callee, parameters } => {
                let (callee_symbol, callee_ty) = self.check_expression(callee)?;

                match callee_ty {
                    BuiltinType::Function { params, return_ty } => {
                        if params.len() != parameters.len() {
                            return Err(SemaError::wrong_arg_count(params.len(), parameters.len()));
                        }

                        for (param_ty, arg_expr) in params.iter().zip(parameters) {
                            let (_, arg_ty) = self.check_expression(arg_expr)?;
                            if param_ty != &arg_ty {
                                return Err(SemaError::type_mismatch(param_ty, arg_ty));
                            }
                        }

                        Ok((callee_symbol, *return_ty))
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
                return_expr,
            } => {
                if self.symbols.lookup(&identifier).is_some() {
                    return Err(SemaError::redefinition(identifier));
                }

                let param_types: Result<Vec<_>, _> = params
                    .iter()
                    .map(|FunctionParam(_, type_expr)| self.resolve_type_expr(type_expr))
                    .collect();
                let param_types = param_types?;

                let return_ty = if let Some(return_expr) = return_expr {
                    Box::new(self.resolve_type_expr(&return_expr)?)
                } else {
                    Box::new(BuiltinType::Never)
                };

                let ty = BuiltinType::Function {
                    params: param_types.clone(),
                    return_ty,
                };
                self.symbols
                    .define(identifier.clone(), (SymbolKind::Function(identifier), ty));

                self.symbols.push_scope();

                for (FunctionParam(param_name, _), param_ty) in params.iter().zip(param_types.iter()) {
                    self.symbols.define(
                        param_name.clone(),
                        (SymbolKind::Parameter(param_name.clone()), param_ty.clone()),
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

                let (_, inferred_ty) = if let Some(initial_expr) = initial_expr {
                    let (symbol, ty) = self.check_expression(&initial_expr)?;
                    (Some(symbol), Some(ty))
                } else {
                    (None, None)
                };

                let resolved_ty = match (explicit_ty, inferred_ty) {
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

                if !self.is_assignable(&resolved_ty) {
                    return Err(SemaError::cannot_assign_to(resolved_ty, "to a variable"));
                }

                self.symbols.define(
                    identifier.clone(),
                    (
                        SymbolKind::Variable {
                            identifier,
                            is_mutable: true,
                        },
                        resolved_ty,
                    ),
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
