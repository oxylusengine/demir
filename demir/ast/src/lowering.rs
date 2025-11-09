use std::collections::HashMap;

use ir::{IrConstant, IrNode, IrNodeId, IrType};

use crate::{Expression, Identifier, Literal, Statement};

#[derive(Debug)]
struct IrModuleBuilder {
    nodes: Vec<IrNode>,
    globals: Vec<IrNodeId>,
    types: HashMap<IrType, IrNodeId>,
    // symbols: SymbolMap<Identifier, IrNodeId, IrNodeId>,
    current_block: Option<IrNodeId>,
    current_function: Option<IrNodeId>,
}

impl IrModuleBuilder {
    fn make_node(&mut self, node: IrNode) -> IrNodeId {
        let node_id = self.nodes.len();
        self.nodes.push(node);

        node_id
    }

    fn get_node(&self, node_id: IrNodeId) -> Option<&IrNode> { self.nodes.get(node_id) }

    fn get_node_mut(&mut self, node_id: IrNodeId) -> Option<&mut IrNode> { self.nodes.get_mut(node_id) }

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

    fn lookup_identifier(&self, identifier: &Identifier) -> Option<&IrNodeId> {
        // self.symbols.lookup(identifier, None)
        None
    }

    fn lower_type(&mut self, ty: IrType) -> IrNodeId {
        if let Some(&id) = self.types.get(&ty) {
            return id;
        }

        let node_id = self.make_node(IrNode::Type(ty.clone()));
        self.types.insert(ty, node_id);

        node_id
    }

    fn lower_stmt(&mut self, stmt: Statement) {
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
            } => todo!(),
            Statement::DeclVar {
                identifier,
                type_expr,
                initial_expr,
            } => todo!(),
            Statement::Expression(expression) => {
                self.lower_expr(expression);
            },
        }
    }

    fn lower_expr(&mut self, expr: Expression) -> IrNodeId {
        match expr {
            Expression::Literal(literal) => self.lower_literal(literal),
            Expression::Identifier(identifier) => *self.lookup_identifier(&identifier).unwrap(),
            Expression::Assign {
                kind,
                lhs_expr,
                rhs_expr,
            } => todo!(),
            Expression::Binary { op, lhs_expr, rhs_expr } => todo!(),
            Expression::CallFunction { callee, parameters } => todo!(),
        }
    }

    fn lower_literal(&mut self, literal: Literal) -> IrNodeId {
        let constant = match literal {
            Literal::String(s) => IrConstant::String(s),
            Literal::Integer(i) => IrConstant::Int(i),
            Literal::Float(f) => IrConstant::Float(f),
            Literal::Bool(b) => IrConstant::from_bool(b),
        };

        self.make_node(IrNode::Constant(constant))
    }
}

pub fn lower_ast(statement: Statement) -> IrNode { todo!() }
