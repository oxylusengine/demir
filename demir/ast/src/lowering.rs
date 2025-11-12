use core::{symbol_table::SymbolTable, types::BuiltinType};
use std::collections::HashMap;

use ir::{IrConstant, IrNode, IrNodeId};

use crate::{AST, AssignmentKind, BinaryOp, Expression, ExpressionId, FunctionParam, Identifier, Literal, Statement};

struct IrModuleBuilder<'a> {
    ast: &'a AST,
    nodes: Vec<IrNode>,
    functions: Vec<IrNodeId>,
    globals: Vec<IrNodeId>,
    types: HashMap<BuiltinType, IrNodeId>,
    current_block: Option<IrNodeId>,
    current_function: Option<IrNodeId>,
    symbols: SymbolTable<Identifier, IrNodeId>,
}

impl<'a> IrModuleBuilder<'a> {
    fn new(ast: &'a AST) -> Self {
        Self {
            ast,
            nodes: Vec::new(),
            functions: Vec::new(),
            globals: Vec::new(),
            types: HashMap::new(),
            current_block: None,
            current_function: None,
            symbols: SymbolTable::new(),
        }
    }

    fn make_node(&mut self, node: IrNode) -> IrNodeId {
        let node_id = self.nodes.len();
        self.nodes.push(node);

        node_id
    }

    fn get_node_mut(&mut self, node_id: IrNodeId) -> Option<&mut IrNode> { self.nodes.get_mut(node_id) }

    fn define_builtin(&mut self, identifier: &str, ty: BuiltinType) {
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

    fn lower_stmt(&mut self, stmt: &Statement) {
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
                        starter_block: IrNodeId::MAX,
                    })
                } else {
                    self.make_node(IrNode::ExternalFunction { ty: return_ty_id })
                };

                self.current_function = Some(func_node_id);
                self.functions.push(func_node_id);
                self.symbols.define(identifier.clone(), func_node_id);
                self.symbols.push_scope();

                params
                    .iter()
                    .for_each(|FunctionParam(param_identifier, param_type_expr)| {
                        let (_, param_ty) = self.ast.get_expr_with_ty(param_type_expr).unwrap();
                        let param_ty_id = self.lower_type(param_ty);
                        let param_id = self.make_node(IrNode::FunctionParam { ty: param_ty_id });
                        self.symbols.define(param_identifier.clone(), param_id);
                    });

                if !external {
                    let starter_block_id = self.make_block();
                    self.set_current_block(starter_block_id);

                    if let Some(IrNode::Function { starter_block, .. }) = self.get_node_mut(func_node_id) {
                        *starter_block = starter_block_id;
                    }

                    if let Some(body) = body {
                        self.lower_stmt(body);
                    }
                    // Implicit return - only if there is still an active block
                    self.terminate_current_block(IrNode::Return(None));
                }

                self.symbols.pop_scope();
            },

            Statement::DeclVar {
                identifier,
                type_expr,
                initial_expr,
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
        }
    }

    fn lower_expr(&mut self, expr_id: &ExpressionId) -> Option<IrNodeId> {
        let (expr, ty) = self.ast.get_expr_with_ty(expr_id)?;
        match expr {
            Expression::Literal(literal) => Some(self.lower_literal(literal)),
            Expression::Identifier(identifier) => {
                let node_id = *self.symbols.lookup(identifier)?;

                match ty {
                    BuiltinType::Function { .. } => {
                        // Functions are direct references (no load needed)
                        Some(node_id)
                    },
                    _ => {
                        // Variables/parameters must be loaded
                        let ty_node_id = self.lower_type(ty);
                        self.emit_instr(IrNode::Load {
                            ty: ty_node_id,
                            variable: node_id,
                        })
                    },
                }
            },
            Expression::Assign {
                kind,
                lhs_expr,
                rhs_expr,
            } => {
                let lhs_instr_id = self.lower_expr(lhs_expr)?;
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

    fn lower_literal(&mut self, literal: &Literal) -> IrNodeId {
        let constant = match literal {
            Literal::String(s) => IrConstant::String(s.clone()),
            Literal::Integer(i) => IrConstant::I32(*i),
            Literal::Float(f) => IrConstant::F32(*f),
            Literal::Bool(b) => IrConstant::from_bool(*b),
        };

        self.make_node(IrNode::Constant(constant))
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
            BinaryOp::RightExclusiveRange => todo!(),
            BinaryOp::RightInclusiveRange => todo!(),
        };

        self.emit_instr(instr)
    }
}

pub fn lower_ast(ast: AST) -> IrNode {
    let mut module_builder = IrModuleBuilder::new(&ast);
    module_builder.define_builtin("i8", BuiltinType::I8);
    module_builder.define_builtin("u8", BuiltinType::U8);
    module_builder.define_builtin("i16", BuiltinType::I16);
    module_builder.define_builtin("u16", BuiltinType::U16);
    module_builder.define_builtin("i32", BuiltinType::I32);
    module_builder.define_builtin("u32", BuiltinType::U32);
    module_builder.define_builtin("i64", BuiltinType::I64);
    module_builder.define_builtin("u64", BuiltinType::U64);
    module_builder.define_builtin("f32", BuiltinType::F32);
    module_builder.define_builtin("f64", BuiltinType::F64);
    module_builder.define_builtin("bool", BuiltinType::Bool);
    module_builder.define_builtin("str", BuiltinType::String);
    module_builder.lower_stmt(&ast.root);

    IrNode::Module {
        nodes: module_builder.nodes,
        functions: module_builder.functions,
        globals: module_builder.globals,
    }
}
