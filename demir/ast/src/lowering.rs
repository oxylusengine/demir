use ir::{IrNode, IrNodeId};

use crate::Statement;

#[derive(Debug)]
pub struct IrModuleBuilder {
    nodes: Vec<IrNode>,
    globals: Vec<IrNodeId>,
    active_basic_block_builder: Option<BasicBlockBuilder>,
}

#[derive(Debug)]
pub struct BasicBlockBuilder {
    label: IrNodeId,
    instructions: Vec<IrNodeId>,
}

impl IrModuleBuilder {
    fn make_node(&mut self, node: IrNode) -> IrNodeId {
        let node_id = self.nodes.len();
        self.nodes.push(node);

        node_id
    }

    fn get_node(&self, node_id: IrNodeId) -> Option<&IrNode> { self.nodes.get(node_id) }

    fn make_block(&mut self) -> IrNodeId {
        self.make_node(IrNode::Label {
            instructions: Vec::new(),
            terminator: None,
        })
    }
}

pub fn lower_ast(statement: Statement) -> IrNode { todo!() }
