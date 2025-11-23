use core::{
    symbol_table::SymbolTable,
    types::{BuiltinType, Identifier},
};
use std::collections::HashMap;

use ast::{AST, AssignmentKind, BinaryOp, Expression, ExpressionId, FunctionParam, Literal, Statement};

use crate::{IrConstant, IrNode, IrNodeId};

pub struct IrModuleBuilder<'a> {
    ast: &'a AST,
    pub nodes: Vec<IrNode>,
    pub functions: Vec<IrNodeId>,
    pub globals: Vec<IrNodeId>,
    types: HashMap<BuiltinType, IrNodeId>,
    current_block: Option<IrNodeId>,
    current_function: Option<IrNodeId>,
    symbols: SymbolTable<Identifier, IrNodeId>,
    loop_stack: Vec<(IrNodeId, IrNodeId)>,
}

impl<'a> IrModuleBuilder<'a> {
    pub fn new(ast: &'a AST) -> Self {
        Self {
            ast,
            nodes: Vec::new(),
            functions: Vec::new(),
            globals: Vec::new(),
            types: HashMap::new(),
            current_block: None,
            current_function: None,
            symbols: SymbolTable::new(),
            loop_stack: Vec::new(),
        }
    }

    fn make_node(&mut self, node: IrNode) -> IrNodeId {
        let node_id = self.nodes.len();
        self.nodes.push(node);

        node_id
    }

    fn get_node_mut(&mut self, node_id: IrNodeId) -> Option<&mut IrNode> { self.nodes.get_mut(node_id) }

    pub fn define_builtin(&mut self, identifier: &str, ty: BuiltinType) {
        let ty_node_id = self.lower_type(&ty);
        self.symbols.define(Identifier(identifier.to_string()), ty_node_id);
    }

    fn make_block(&mut self) -> IrNodeId { self.make_node(IrNode::Label(Vec::new())) }

    fn set_current_block(&mut self, node_id: IrNodeId) { self.current_block = Some(node_id); }

    fn terminate_current_block(&mut self, terminator_node: IrNode) {
        if self.current_block.is_none() {
            return;
        }

        self.emit_instr(terminator_node);
        self.current_block = None;
    }

    fn emit_instr(&mut self, node: IrNode) -> Option<IrNodeId> {
        let node_id = self.make_node(node);
        if let Some(IrNode::Label(instructions)) = self.get_node_mut(self.current_block?) {
            instructions.push(node_id);
            return Some(node_id);
        }

        None
    }

    fn lower_type(&mut self, ty: &BuiltinType) -> IrNodeId {
        if let Some(&id) = self.types.get(ty) {
            return id;
        }

        let node_id = self.make_node(IrNode::Type(ty.clone()));
        self.types.insert(ty.clone(), node_id);
        self.globals.push(node_id);

        node_id
    }

    pub fn lower_stmt(&mut self, stmt: &Statement) {
        match stmt {
            Statement::Multi(statements) => {
                for s in statements {
                    self.lower_stmt(s);
                }
            },

            Statement::DeclFunction {
                identifier,
                params,
                body,
                return_expr,
                external,
            } => {
                let return_ty_id = if let Some(return_expr) = return_expr {
                    let (_, return_ty) = self.ast.get_expr_with_ty(return_expr).unwrap();
                    self.lower_type(return_ty)
                } else {
                    self.lower_type(&BuiltinType::Never)
                };

                let func_node_id = if !external {
                    self.make_node(IrNode::Function {
                        ty: return_ty_id,
                        params: Vec::default(),
                        starter_block: IrNodeId::MAX,
                    })
                } else {
                    self.make_node(IrNode::ExternalFunction {
                        ty: return_ty_id,
                        params: Vec::default(),
                    })
                };

                self.current_function = Some(func_node_id);
                self.functions.push(func_node_id);
                self.symbols.define(identifier.clone(), func_node_id);
                self.symbols.push_scope();

                let param_node_ids = params
                    .iter()
                    .map(|FunctionParam(param_identifier, param_type_expr)| {
                        let (_, param_ty) = self.ast.get_expr_with_ty(param_type_expr).unwrap();
                        let param_ty_id = self.lower_type(param_ty);
                        let param_id = self.make_node(IrNode::FunctionParam { ty: param_ty_id });
                        self.symbols.define(param_identifier.clone(), param_id);

                        param_id
                    })
                    .collect::<Vec<_>>();

                if !external {
                    let starter_block_id = self.make_block();

                    if let Some(IrNode::Function {
                        params, starter_block, ..
                    }) = self.get_node_mut(func_node_id)
                    {
                        *params = param_node_ids;
                        *starter_block = starter_block_id;
                    }

                    self.set_current_block(starter_block_id);

                    if let Some(body) = body {
                        self.lower_stmt(body);
                    }

                    // Implicit return
                    self.terminate_current_block(IrNode::Return(None));
                } else if let Some(IrNode::ExternalFunction { params, .. }) = self.get_node_mut(func_node_id) {
                    *params = param_node_ids;
                }

                self.symbols.pop_scope();
            },

            Statement::DeclVar {
                identifier,
                type_expr,
                initial_expr,
                ..
            } => {
                let mut initial_node_id = None;
                let var_ty_id = if let Some(type_expr) = type_expr {
                    let (_, var_ty) = self.ast.get_expr_with_ty(type_expr).unwrap();
                    self.lower_type(var_ty)
                } else if let Some(initial_expr) = initial_expr {
                    initial_node_id = self.lower_expr(initial_expr);

                    let (_, initial_expr_ty) = self.ast.get_expr_with_ty(initial_expr).unwrap();
                    self.lower_type(initial_expr_ty)
                } else {
                    self.lower_type(&BuiltinType::Never)
                };

                let var_id = self.emit_instr(IrNode::Variable { ty: var_ty_id }).unwrap();
                if let Some(initial_node_id) = initial_node_id {
                    self.emit_instr(IrNode::Store {
                        src: initial_node_id,
                        dst: var_id,
                    });
                }

                self.symbols.define(identifier.clone(), var_id);
            },

            Statement::Expression(expression) => {
                self.lower_expr(expression);
            },

            Statement::Return(expression) => {
                let node_id = self.lower_expr(expression);
                self.terminate_current_block(IrNode::Return(node_id));
            },

            Statement::If {
                condition,
                true_case,
                false_case,
            } => {
                let cond_node_id = self.lower_expr(condition).unwrap();
                let true_block = self.make_block();
                let false_block = self.make_block();
                let exiting_block = if false_case.is_some() {
                    self.make_block()
                } else {
                    false_block
                };

                self.terminate_current_block(IrNode::ConditionalBranch {
                    condition: cond_node_id,
                    true_block,
                    false_block,
                });

                self.set_current_block(true_block);

                self.symbols.push_scope();
                self.lower_stmt(true_case);
                self.symbols.pop_scope();

                self.terminate_current_block(IrNode::Branch(exiting_block));

                if let Some(false_case) = false_case {
                    self.set_current_block(false_block);

                    self.symbols.push_scope();
                    self.lower_stmt(false_case);
                    self.symbols.pop_scope();

                    self.terminate_current_block(IrNode::Branch(exiting_block));
                }

                self.set_current_block(exiting_block);
            },

            Statement::While { condition, true_case } => {
                let cond_block = self.make_block();
                let true_block = self.make_block();
                let exiting_block = self.make_block();

                // Terminate ongoing block to condition check:
                // ```
                // while x < 10
                // ```
                self.terminate_current_block(IrNode::Branch(cond_block));
                self.set_current_block(cond_block);
                let cond_node_id = self.lower_expr(condition).unwrap();
                self.terminate_current_block(IrNode::ConditionalBranch {
                    condition: cond_node_id,
                    true_block,
                    false_block: exiting_block,
                });

                // Lower the actual loop body
                self.loop_stack.push((cond_block, exiting_block));
                self.set_current_block(true_block);
                self.symbols.push_scope();
                self.lower_stmt(true_case);
                self.symbols.pop_scope();
                self.loop_stack.pop();

                // Exit out of the loop
                self.terminate_current_block(IrNode::Branch(cond_block));
                self.set_current_block(exiting_block);
            },

            Statement::For { iter, range, body } => {
                // Lower loop range
                let (range_expr, range_ty) = self.ast.get_expr_with_ty(range).unwrap();
                let (iter_start_expr, iter_end_expr) = match range_expr {
                    // Exclusive ranges
                    // ```
                    // for i in 0..15
                    // ```
                    Expression::Range {
                        kind: _,
                        lhs_expr,
                        rhs_expr,
                    } => (lhs_expr, rhs_expr),

                    // TODO: Somehow figure out how to implement implicit ranges.
                    // ```
                    // let x = [1, 2, 3];
                    // for v in x
                    // ```
                    _ => {
                        todo!("Non-range expressions are not supported yet.")
                    },
                };
                // Initialize iterator with start of the range
                let iter_ty_id = self.lower_type(range_ty);
                let iter_start_id = self.lower_expr(iter_start_expr).unwrap();
                let iter_var_id = self.emit_instr(IrNode::Variable { ty: iter_ty_id }).unwrap();
                self.emit_instr(IrNode::Store {
                    src: iter_start_id,
                    dst: iter_var_id,
                });

                let cond_block = self.make_block();
                let true_block = self.make_block();
                let next_iter_block = self.make_block();
                let exiting_block = self.make_block();

                // Terminate ongoing block to condition check:
                // ```
                // // This is guaranteed and always the case
                // iter < iter_end
                // ```
                self.terminate_current_block(IrNode::Branch(cond_block));
                self.set_current_block(cond_block);
                let lhs_instr_id = self
                    .emit_instr(IrNode::Load {
                        ty: iter_ty_id,
                        variable: iter_var_id,
                    })
                    .unwrap();
                let iter_end_id = self.lower_expr(iter_end_expr).unwrap();
                let condition = self
                    .emit_instr(IrNode::LessThan {
                        ty: iter_ty_id,
                        lhs: lhs_instr_id,
                        rhs: iter_end_id,
                    })
                    .unwrap();

                // Lower the actual loop body
                self.terminate_current_block(IrNode::ConditionalBranch {
                    condition,
                    true_block,
                    false_block: exiting_block,
                });
                self.set_current_block(true_block);
                // Unlike while loop, `continue` statement doesn't go to condition
                // because we have an iterator to increment
                self.loop_stack.push((next_iter_block, exiting_block));
                self.symbols.push_scope();
                self.symbols.define(iter.clone(), iter_var_id);
                self.lower_stmt(body);
                self.symbols.pop_scope();
                self.loop_stack.pop();

                // Terminate to iterator incrementer
                self.terminate_current_block(IrNode::Branch(next_iter_block));
                self.set_current_block(next_iter_block);
                // TODO: we should probably check if iteration is arithmatic
                let lhs_instr_id = self
                    .emit_instr(IrNode::Load {
                        ty: iter_ty_id,
                        variable: iter_var_id,
                    })
                    .unwrap();
                let one_instr_id = self.lower_literal(&Literal::Integer(1));
                let add_instr_id = self
                    .emit_instr(IrNode::Add {
                        ty: iter_ty_id,
                        lhs: lhs_instr_id,
                        rhs: one_instr_id,
                    })
                    .unwrap();
                self.emit_instr(IrNode::Store {
                    src: add_instr_id,
                    dst: iter_var_id,
                });

                // Go back to loop condition
                self.terminate_current_block(IrNode::Branch(cond_block));

                // Exit out of the loop
                self.set_current_block(exiting_block);
            },

            Statement::Continue => {
                let (continue_block, _) = self.loop_stack.last().unwrap();
                self.terminate_current_block(IrNode::Branch(*continue_block));
            },

            Statement::Break => {
                let (_, break_block) = self.loop_stack.last().unwrap();
                self.terminate_current_block(IrNode::Branch(*break_block));
            },
        }
    }

    fn lower_expr(&mut self, expr_id: &ExpressionId) -> Option<IrNodeId> {
        let (expr, ty) = self.ast.get_expr_with_ty(expr_id)?;
        match expr {
            Expression::Literal(literal) => Some(self.lower_literal(literal)),
            Expression::Identifier(identifier) => {
                let node_id = *self.symbols.lookup(identifier)?;

                match ty {
                    BuiltinType::Function { .. } => Some(node_id),
                    _ => {
                        // Variables/parameters must be loaded
                        let lvalue_id = self.lower_lvalue(expr_id)?;
                        let ty_node_id = self.lower_type(ty);
                        self.emit_instr(IrNode::Load {
                            ty: ty_node_id,
                            variable: lvalue_id,
                        })
                    },
                }
            },
            Expression::Assign {
                kind,
                lhs_expr,
                rhs_expr,
            } => {
                let lhs_instr_id = self.lower_lvalue(lhs_expr)?;
                let rhs_instr_id = self.lower_expr(rhs_expr)?;
                let assigner_id = if !matches!(kind, AssignmentKind::Assign) {
                    let (_, rhs_ty) = self.ast.get_expr_with_ty(rhs_expr)?;
                    let op = match kind {
                        AssignmentKind::Assign => panic!(),
                        AssignmentKind::CompoundAdd => BinaryOp::Add,
                        AssignmentKind::CompoundSub => BinaryOp::Sub,
                        AssignmentKind::CompoundMul => BinaryOp::Mul,
                        AssignmentKind::CompoundDiv => BinaryOp::Div,
                    };

                    self.lower_binary_op(rhs_ty, &op, &lhs_instr_id, &rhs_instr_id)?
                } else {
                    rhs_instr_id
                };

                self.emit_instr(IrNode::Store {
                    src: assigner_id,
                    dst: lhs_instr_id,
                })
            },
            Expression::Range { .. } => None, // This is special case
            Expression::Binary { op, lhs_expr, rhs_expr } => {
                let lhs_instr_id = self.lower_expr(lhs_expr)?;
                let rhs_instr_id = self.lower_expr(rhs_expr)?;
                let (_, rhs_ty) = self.ast.get_expr_with_ty(rhs_expr)?;

                self.lower_binary_op(rhs_ty, op, &lhs_instr_id, &rhs_instr_id)
            },
            Expression::CallFunction { callee, parameters } => {
                let callee_instr = self.lower_expr(callee)?;
                let args = parameters
                    .iter()
                    .map(|param_expr| self.lower_expr(param_expr))
                    .collect::<Option<Vec<_>>>()?;

                self.emit_instr(IrNode::Call {
                    callee: callee_instr,
                    args,
                })
            },
        }
    }

    fn lower_lvalue(&mut self, expr_id: &ExpressionId) -> Option<IrNodeId> {
        let (expr, ty) = self.ast.get_expr_with_ty(expr_id)?;
        match expr {
            Expression::Identifier(identifier) => {
                let node_id = *self.symbols.lookup(identifier)?;

                match ty {
                    BuiltinType::Function { .. } => None,
                    _ => Some(node_id),
                }
            },

            // TODO:
            // Expression::StructFieldAccess { .. } => { ... }
            // Expression::ArrayIndex { .. } => { ... }
            _ => {
                None // TODO: Should probably throw an error there
            },
        }
    }

    fn lower_literal(&mut self, literal: &Literal) -> IrNodeId {
        let constant = match literal {
            Literal::String(s) => IrConstant::String(s.clone()),
            Literal::Integer(i) => IrConstant::I32(*i),
            Literal::Float(f) => IrConstant::F32(*f),
            Literal::Bool(b) => IrConstant::from_bool(*b),
        };

        let constant_id = self.make_node(IrNode::Constant(constant));
        self.globals.push(constant_id);
        constant_id
    }

    fn lower_binary_op(&mut self, ty: &BuiltinType, op: &BinaryOp, lhs: &IrNodeId, rhs: &IrNodeId) -> Option<IrNodeId> {
        let ty_node_id = self.lower_type(ty);
        let instr = match op {
            BinaryOp::Add => IrNode::Add {
                ty: ty_node_id,
                lhs: *lhs,
                rhs: *rhs,
            },
            BinaryOp::Sub => IrNode::Sub {
                ty: ty_node_id,
                lhs: *lhs,
                rhs: *rhs,
            },
            BinaryOp::Mul => IrNode::Mul {
                ty: ty_node_id,
                lhs: *lhs,
                rhs: *rhs,
            },
            BinaryOp::Div => IrNode::Div {
                ty: ty_node_id,
                lhs: *lhs,
                rhs: *rhs,
            },
            BinaryOp::Mod => IrNode::Mod {
                ty: ty_node_id,
                lhs: *lhs,
                rhs: *rhs,
            },
            BinaryOp::BitAnd => IrNode::BitAnd {
                ty: ty_node_id,
                lhs: *lhs,
                rhs: *rhs,
            },
            BinaryOp::BitXor => IrNode::BitXor {
                ty: ty_node_id,
                lhs: *lhs,
                rhs: *rhs,
            },
            BinaryOp::BitOr => IrNode::BitOr {
                ty: ty_node_id,
                lhs: *lhs,
                rhs: *rhs,
            },
            BinaryOp::CompGreater => IrNode::GreaterThan {
                ty: ty_node_id,
                lhs: *lhs,
                rhs: *rhs,
            },
            BinaryOp::CompLess => IrNode::LessThan {
                ty: ty_node_id,
                lhs: *lhs,
                rhs: *rhs,
            },
            BinaryOp::CompEq => IrNode::Equal {
                ty: ty_node_id,
                lhs: *lhs,
                rhs: *rhs,
            },
            BinaryOp::CompNotEq => IrNode::NotEqual {
                ty: ty_node_id,
                lhs: *lhs,
                rhs: *rhs,
            },
            BinaryOp::CompAnd => IrNode::LogicalAnd {
                ty: ty_node_id,
                lhs: *lhs,
                rhs: *rhs,
            },
            BinaryOp::CompOr => IrNode::LogicalOr {
                ty: ty_node_id,
                lhs: *lhs,
                rhs: *rhs,
            },
            BinaryOp::CompGreaterEq => IrNode::GreaterThanOrEqual {
                ty: ty_node_id,
                lhs: *lhs,
                rhs: *rhs,
            },
            BinaryOp::CompLessEq => IrNode::LessThanOrEqual {
                ty: ty_node_id,
                lhs: *lhs,
                rhs: *rhs,
            },
            BinaryOp::ShiftLeft => IrNode::ShiftLeft {
                ty: ty_node_id,
                lhs: *lhs,
                rhs: *rhs,
            },
            BinaryOp::ShiftRight => IrNode::ShiftRight {
                ty: ty_node_id,
                lhs: *lhs,
                rhs: *rhs,
            },
        };

        self.emit_instr(instr)
    }
}
