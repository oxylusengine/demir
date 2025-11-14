mod error;

use core::{
    symbol_table::SymbolTable,
    types::{BuiltinType, Identifier, SymbolKind},
};

use ast::{AST, AssignmentKind, BinaryOp, Expression, ExpressionId, FunctionParam, Literal, Statement};

use crate::error::SemaError;

type Symbols = SymbolTable<Identifier, (SymbolKind, BuiltinType)>;

fn is_assignable(ty: &BuiltinType) -> bool {
    match ty {
        BuiltinType::String => false,
        BuiltinType::Never => false,
        BuiltinType::Unit => false,
        BuiltinType::Function { return_ty, .. } => is_assignable(return_ty),
        _ => true,
    }
}

fn supports_arithmetic(ty: &BuiltinType) -> bool {
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

fn literal_type(lit: &Literal) -> BuiltinType {
    match lit {
        Literal::String(_) => BuiltinType::String,
        Literal::Integer(_) => BuiltinType::I32,
        Literal::Float(_) => BuiltinType::F32,
        Literal::Bool(_) => BuiltinType::Bool,
    }
}

struct ActiveFunction {
    return_ty: BuiltinType,
}

pub struct SemanticAnalyzer<'a> {
    ast: &'a AST,
    symbols: Symbols,
    expr_types: Vec<BuiltinType>,
    current_function: Option<ActiveFunction>,
}

impl<'a> SemanticAnalyzer<'a> {
    pub fn new(ast: &'a AST) -> Self {
        Self {
            ast,
            symbols: Symbols::new(),
            expr_types: Vec::new(),
            current_function: None,
        }
    }

    pub fn run_analysis(&mut self) -> Result<(), SemaError> {
        let root_stmt = self.ast.root.clone();
        self.check_stmt(&root_stmt)
    }

    fn annotate_ty(&mut self, expr_id: &ExpressionId, ty: BuiltinType) {
        if self.expr_types.len() <= *expr_id {
            self.expr_types.resize(*expr_id + 1, BuiltinType::Never);
        }

        self.expr_types[*expr_id] = ty;
    }

    fn resolve_type_expr(&mut self, expr_id: &ExpressionId) -> Result<BuiltinType, SemaError> {
        let expr = self.ast.get_expr(expr_id).ok_or(SemaError::unknown())?;
        let ty = match expr {
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
        }?;

        self.annotate_ty(expr_id, ty.clone());

        Ok(ty)
    }

    fn check_lvalue(&self, expr_id: &ExpressionId) -> Result<(), SemaError> {
        let expr = self.ast.get_expr(expr_id).ok_or(SemaError::unknown())?;
        match expr {
            Expression::Identifier(_) => Ok(()),
            Expression::Literal(_) => Err(SemaError::cannot_assign("a literal")),
            Expression::Assign { .. } => panic!(), // What?
            Expression::Binary { .. } => Err(SemaError::cannot_assign("a binary op")),
            Expression::CallFunction { .. } => Err(SemaError::cannot_assign("a function call")),
        }
    }

    fn check_expr(&mut self, expr_id: &ExpressionId) -> Result<(SymbolKind, BuiltinType), SemaError> {
        let expr = self.ast.get_expr(expr_id).ok_or(SemaError::unknown())?;
        let (symbol, ty) = match expr {
            Expression::Literal(lit) => Ok((SymbolKind::Literal, literal_type(lit))),

            Expression::Identifier(ident) => {
                let (symbol_kind, ty) = self.symbols.lookup(ident).ok_or(SemaError::undefined_var(ident))?;
                if !matches!(
                    symbol_kind,
                    SymbolKind::Variable { .. } | SymbolKind::Function { .. } | SymbolKind::Parameter { .. }
                ) {
                    return Err(SemaError::undefined_var(ident));
                }

                Ok((symbol_kind.clone(), ty.clone()))
            },

            Expression::Binary { op, lhs_expr, rhs_expr } => {
                let (lhs_symbol, lhs_ty) = self.check_expr(lhs_expr)?;
                let (_, rhs_ty) = self.check_expr(rhs_expr)?;

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

                let (lhs_symbol, lhs_ty) = self.check_expr(lhs_expr)?;
                let (_, rhs_ty) = self.check_expr(rhs_expr)?;

                #[allow(clippy::collapsible_if)]
                if let SymbolKind::Variable { identifier, is_mutable } = &lhs_symbol {
                    if !is_mutable {
                        return Err(SemaError::cannot_assign_to_immutable(identifier));
                    }
                };

                if lhs_ty != rhs_ty {
                    return Err(SemaError::type_mismatch(lhs_ty, rhs_ty));
                }

                if !is_assignable(&lhs_ty) {
                    return Err(SemaError::cannot_assign_to(rhs_ty, lhs_ty));
                }

                match kind {
                    AssignmentKind::Assign => {},
                    AssignmentKind::CompoundAdd
                    | AssignmentKind::CompoundSub
                    | AssignmentKind::CompoundMul
                    | AssignmentKind::CompoundDiv => {
                        if !supports_arithmetic(&lhs_ty) {
                            return Err(SemaError::cannot_assign_to(rhs_ty, lhs_ty));
                        }
                    },
                };

                Ok((lhs_symbol, lhs_ty))
            },

            Expression::CallFunction { callee, parameters } => {
                let (callee_symbol, callee_ty) = self.check_expr(callee)?;

                match callee_ty {
                    BuiltinType::Function { params, return_ty } => {
                        if params.len() != parameters.len() {
                            return Err(SemaError::wrong_arg_count(params.len(), parameters.len()));
                        }

                        for (param_ty, arg_expr) in params.iter().zip(parameters) {
                            let (_, arg_ty) = self.check_expr(arg_expr)?;
                            if param_ty != &arg_ty {
                                return Err(SemaError::type_mismatch(param_ty, arg_ty));
                            }
                        }

                        Ok((callee_symbol, *return_ty))
                    },
                    _ => Err(SemaError::not_callable()),
                }
            },
        }?;

        self.annotate_ty(expr_id, ty.clone());

        Ok((symbol, ty))
    }

    fn check_stmt(&mut self, stmt: &Statement) -> Result<(), SemaError> {
        match stmt {
            Statement::Multi(stmts) => {
                self.symbols.push_scope();

                for stmt in stmts {
                    self.check_stmt(stmt)?;
                }

                self.symbols.pop_scope();
                Ok(())
            },

            Statement::DeclFunction {
                identifier,
                params,
                body,
                return_expr,
                ..
            } => {
                if self.symbols.lookup(identifier).is_some() {
                    return Err(SemaError::redefinition(identifier));
                }

                let param_types = params
                    .iter()
                    .map(|FunctionParam(_, type_expr)| self.resolve_type_expr(type_expr))
                    .collect::<Result<Vec<_>, _>>()?;

                let return_ty = if let Some(return_expr) = return_expr {
                    Box::new(self.resolve_type_expr(return_expr)?)
                } else {
                    Box::new(BuiltinType::Never)
                };

                self.current_function = Some(ActiveFunction {
                    return_ty: *return_ty.clone(),
                });

                let ty = BuiltinType::Function {
                    params: param_types.clone(),
                    return_ty,
                };
                self.symbols
                    .define(identifier.clone(), (SymbolKind::Function(identifier.clone()), ty));

                self.symbols.push_scope();

                for (FunctionParam(param_name, _), param_ty) in params.iter().zip(param_types.iter()) {
                    self.symbols.define(
                        param_name.clone(),
                        (SymbolKind::Parameter(param_name.clone()), param_ty.clone()),
                    );
                }

                if let Some(body) = body {
                    self.check_stmt(body)?;
                }

                self.symbols.pop_scope();

                self.current_function.take();

                Ok(())
            },

            Statement::DeclVar {
                identifier,
                type_expr,
                initial_expr,
            } => {
                if self.symbols.lookup(identifier).is_some() {
                    return Err(SemaError::redefinition(identifier));
                }

                let explicit_ty = if let Some(type_expr) = type_expr {
                    Some(self.resolve_type_expr(type_expr)?)
                } else {
                    None
                };

                let (_, inferred_ty) = if let Some(initial_expr) = initial_expr {
                    let (symbol, ty) = self.check_expr(initial_expr)?;
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

                if !is_assignable(&resolved_ty) {
                    return Err(SemaError::cannot_assign_to(resolved_ty, "to a variable"));
                }

                self.symbols.define(
                    identifier.clone(),
                    (
                        SymbolKind::Variable {
                            identifier: identifier.clone(),
                            is_mutable: true,
                        },
                        resolved_ty,
                    ),
                );

                Ok(())
            },

            Statement::Expression(expression) => {
                self.check_expr(expression)?;
                Ok(())
            },

            Statement::Return(expression) => {
                if self.current_function.is_none() {
                    return Err(SemaError::return_outside_function());
                }

                let (_, ty) = self.check_expr(expression)?;
                if let Some(current_function) = &self.current_function
                    && current_function.return_ty != ty
                {
                    return Err(SemaError::return_type_mismatch(current_function.return_ty.clone(), ty));
                }

                Ok(())
            },

            Statement::If {
                condition,
                true_case,
                false_case,
            } => {
                self.check_expr(condition)?;
                self.check_stmt(true_case)?;
                if let Some(false_case) = false_case {
                    self.check_stmt(false_case)?;
                }

                Ok(())
            },

            Statement::While { condition, true_case } => {
                self.check_expr(condition)?;
                self.check_stmt(true_case)?;

                Ok(())
            },

            Statement::Continue | Statement::Break => Ok(()),
        }
    }
}

pub fn analyze(mut ast: AST) -> Result<AST, SemaError> {
    let mut analyzer = SemanticAnalyzer::new(&ast);
    analyzer.run_analysis()?;

    ast.expression_types = analyzer.expr_types;

    Ok(ast)
}
