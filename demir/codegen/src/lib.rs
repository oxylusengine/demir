pub mod disasm;
pub mod opcode;

use core::types::BuiltinType;
use std::collections::{HashMap, HashSet};

use ir::{IrConstant, IrNode, IrNodeId};

use crate::opcode::Op;

#[derive(Debug, Clone)]
pub struct Module {
    pub magic: [u8; 4],
    pub version: u16,
    pub strings: Vec<String>,
    pub functions: Vec<CompiledFunction>,
    pub code: Vec<u8>,
}

#[derive(Debug, Clone)]
pub struct CompiledFunction {
    pub id: u16,
    pub address: u32,
    pub param_count: u8,
    pub local_count: u16,
}

struct MarkedJump {
    offset: u32,
    dst_block_id: IrNodeId,
}

struct FunctionGenerator {
    param_count: u8,
    local_slots: HashMap<IrNodeId, u16>,
    next_local_slot: u16,
    ssa_stack: HashSet<IrNodeId>,
}

impl FunctionGenerator {
    fn mark_pushed(&mut self, id: &IrNodeId) { self.ssa_stack.insert(*id); }

    fn mark_popped(&mut self, id: &IrNodeId) { self.ssa_stack.remove(id); }

    fn is_on_stack(&self, id: &IrNodeId) -> bool { self.ssa_stack.contains(id) }
}

pub struct CodeGenerator {
    functions: Vec<CompiledFunction>,
    func_slots: HashMap<IrNodeId, u16>,
    marked_jumps: Vec<MarkedJump>,
    label_offsets: HashMap<IrNodeId, u32>,
    strings: Vec<String>,
    code: Vec<u8>,
}

impl CodeGenerator {
    pub fn new() -> Self {
        Self {
            functions: Vec::new(),
            func_slots: HashMap::new(),
            marked_jumps: Vec::new(),
            label_offsets: HashMap::new(),
            strings: Vec::new(),
            code: Vec::new(),
        }
    }

    pub fn generate(&mut self, ir_module: &IrNode) -> Module {
        match ir_module {
            IrNode::Module { nodes, functions, .. } => {
                for (next_func_slot, func_id) in (0_u16..).zip(functions.iter()) {
                    self.func_slots.insert(*func_id, next_func_slot);
                }

                for func_id in functions {
                    self.generate_function(nodes, func_id);
                }

                self.resolve_jumps();

                Module {
                    magic: *b"EMIR",
                    version: 1,
                    strings: self.strings.clone(),
                    functions: self.functions.clone(),
                    code: self.code.clone(),
                }
            },
            _ => panic!(),
        }
    }

    fn generate_function(&mut self, nodes: &[IrNode], func_id: &IrNodeId) {
        let address = self.code.len() as u32;

        let func_node = &nodes[*func_id];
        match func_node {
            IrNode::Function { starter_block, .. } => {
                let mut func_generator = FunctionGenerator {
                    param_count: 0,
                    local_slots: HashMap::new(),
                    next_local_slot: 0,
                    ssa_stack: HashSet::new(),
                };

                // LOCAL SLOT RESERVATION ORDER:
                // - Function parameters
                // - Variables
                // - Temporaries

                self.reserve_parameter_locals(nodes, func_id, &mut func_generator);
                self.reserve_variable_locals(nodes, starter_block, &mut func_generator);

                self.generate_block(nodes, starter_block, &mut func_generator);

                self.functions.push(CompiledFunction {
                    id: *self.func_slots.get(func_id).unwrap_or(&u16::MAX),
                    address,
                    param_count: func_generator.param_count,
                    local_count: func_generator.next_local_slot,
                });
            },
            IrNode::ExternalFunction { .. } => {
                let mut param_count = 0;
                let mut current_id = func_id + 1;
                while current_id < nodes.len() {
                    match &nodes[current_id] {
                        IrNode::FunctionParam { .. } => {
                            param_count += 1;
                            current_id += 1;
                        },
                        _ => break,
                    }
                }

                self.functions.push(CompiledFunction {
                    id: *self.func_slots.get(func_id).unwrap_or(&u16::MAX),
                    address,
                    param_count,
                    local_count: 0,
                });
            },
            _ => panic!(),
        }
    }

    fn reserve_parameter_locals(&self, nodes: &[IrNode], func_id: &IrNodeId, generator: &mut FunctionGenerator) {
        let mut current_id = func_id + 1;

        while current_id < nodes.len() {
            match &nodes[current_id] {
                IrNode::FunctionParam { .. } => {
                    generator.local_slots.insert(current_id, generator.next_local_slot);
                    generator.next_local_slot += 1;
                    generator.param_count += 1;
                    current_id += 1;
                },
                _ => break,
            }
        }
    }

    fn reserve_variable_locals(&self, nodes: &[IrNode], block_id: &IrNodeId, generator: &mut FunctionGenerator) {
        let block_node = &nodes[*block_id];

        if let IrNode::Label(instructions) = block_node {
            for &inst_id in instructions {
                let inst = &nodes[inst_id];

                if let IrNode::Variable { .. } = inst {
                    generator.local_slots.insert(inst_id, generator.next_local_slot);
                    generator.next_local_slot += 1;
                }
            }
        }
    }

    fn generate_block(&mut self, nodes: &[IrNode], block_id: &IrNodeId, generator: &mut FunctionGenerator) {
        let block_node = &nodes[*block_id];

        if let IrNode::Label(instructions) = block_node {
            self.label_offsets.insert(*block_id, self.code.len() as u32);
            for inst_id in instructions {
                self.generate_instr(nodes, inst_id, generator);
            }
        }
    }

    fn generate_instr(&mut self, nodes: &[IrNode], instr_id: &IrNodeId, generator: &mut FunctionGenerator) {
        let instr = &nodes[*instr_id];

        if generator.is_on_stack(instr_id) {
            return;
        }

        match instr {
            IrNode::Constant(ir_constant) => {
                self.emit_constant(ir_constant);
                generator.mark_pushed(instr_id);
            },
            IrNode::Load { variable, .. } => {
                let slot = *generator.local_slots.get(variable).unwrap();
                self.emit_load_local(slot);
                generator.mark_pushed(instr_id);
            },
            IrNode::Store { src, dst } => {
                if !generator.is_on_stack(src) {
                    self.generate_instr(nodes, src, generator);
                }

                generator.mark_popped(src);

                match &nodes[*dst] {
                    IrNode::Variable { .. } => {
                        let slot = *generator.local_slots.get(dst).unwrap();
                        self.emit_store_local(slot);
                    },
                    IrNode::Load { ty: _, variable } => {
                        let slot = *generator.local_slots.get(variable).unwrap();
                        self.emit_store_local(slot);
                    },
                    _ => panic!(),
                }
            },
            IrNode::Add { ty, lhs, rhs } => {
                self.generate_instr(nodes, lhs, generator);
                self.generate_instr(nodes, rhs, generator);
                self.emit_add(nodes, ty);
                generator.mark_popped(lhs);
                generator.mark_popped(rhs);
                generator.mark_pushed(instr_id);
            },
            IrNode::Sub { ty, lhs, rhs } => {
                self.generate_instr(nodes, lhs, generator);
                self.generate_instr(nodes, rhs, generator);
                self.emit_sub(nodes, ty);
                generator.mark_popped(lhs);
                generator.mark_popped(rhs);
                generator.mark_pushed(instr_id);
            },
            IrNode::Mul { ty, lhs, rhs } => {
                self.generate_instr(nodes, lhs, generator);
                self.generate_instr(nodes, rhs, generator);
                self.emit_mul(nodes, ty);
                generator.mark_popped(lhs);
                generator.mark_popped(rhs);
                generator.mark_pushed(instr_id);
            },
            IrNode::Div { ty, lhs, rhs } => {
                self.generate_instr(nodes, lhs, generator);
                self.generate_instr(nodes, rhs, generator);
                self.emit_div(nodes, ty);
                generator.mark_popped(lhs);
                generator.mark_popped(rhs);
                generator.mark_pushed(instr_id);
            },
            IrNode::Return(value) => {
                if let Some(val_id) = value {
                    self.generate_instr(nodes, val_id, generator);
                    self.emit(Op::RetValue);
                } else {
                    self.emit(Op::Ret);
                }
            },
            IrNode::Call { callee, args } => {
                for arg_id in args {
                    self.generate_instr(nodes, arg_id, generator);
                }

                let callee_id = *self.func_slots.get(callee).unwrap();
                self.emit_call(callee_id, args.len() as u8);
                generator.mark_pushed(instr_id);
            },
            IrNode::Mod { ty, lhs, rhs } => todo!(),
            IrNode::BitAnd { ty, lhs, rhs } => todo!(),
            IrNode::BitOr { ty, lhs, rhs } => todo!(),
            IrNode::BitXor { ty, lhs, rhs } => todo!(),
            IrNode::BitNot(_) => todo!(),
            IrNode::ShiftLeft { ty, lhs, rhs } => todo!(),
            IrNode::ShiftRight { ty, lhs, rhs } => todo!(),
            IrNode::Equal { ty, lhs, rhs } => todo!(),
            IrNode::NotEqual { ty, lhs, rhs } => todo!(),
            IrNode::GreaterThan { ty, lhs, rhs } => todo!(),
            IrNode::GreaterThanOrEqual { ty, lhs, rhs } => todo!(),
            IrNode::LessThan { ty, lhs, rhs } => todo!(),
            IrNode::LessThanOrEqual { ty, lhs, rhs } => todo!(),
            IrNode::LogicalAnd { ty, lhs, rhs } => todo!(),
            IrNode::LogicalOr { ty, lhs, rhs } => todo!(),
            IrNode::LogicalNot(_) => todo!(),
            IrNode::Branch(dst_block_id) => {
                self.mark_jump(Op::Jump, *dst_block_id);
            },
            IrNode::ConditionalBranch {
                condition,
                true_block,
                false_block,
            } => todo!(),
            IrNode::Variable { .. }
            | IrNode::ExternalFunction { .. }
            | IrNode::Function { .. }
            | IrNode::FunctionParam { .. }
            | IrNode::Module { .. }
            | IrNode::Type { .. }
            | IrNode::Label { .. } => {
                // These are handled previously
            },
        }
    }

    fn emit_add(&mut self, nodes: &[IrNode], ty_id: &IrNodeId) {
        let ty = &nodes[*ty_id];
        match ty {
            IrNode::Type(BuiltinType::I32) => self.emit(Op::AddI32),
            IrNode::Type(BuiltinType::I64) => self.emit(Op::AddI64),
            IrNode::Type(BuiltinType::F32) => self.emit(Op::AddF32),
            IrNode::Type(BuiltinType::F64) => self.emit(Op::AddF64),
            _ => panic!(),
        }
    }

    fn emit_sub(&mut self, nodes: &[IrNode], ty_id: &IrNodeId) {
        let ty = &nodes[*ty_id];
        match ty {
            IrNode::Type(BuiltinType::I32) => self.emit(Op::SubI32),
            IrNode::Type(BuiltinType::I64) => self.emit(Op::SubI64),
            IrNode::Type(BuiltinType::F32) => self.emit(Op::SubF32),
            IrNode::Type(BuiltinType::F64) => self.emit(Op::SubF64),
            _ => panic!(),
        }
    }

    fn emit_mul(&mut self, nodes: &[IrNode], ty_id: &IrNodeId) {
        let ty = &nodes[*ty_id];
        match ty {
            IrNode::Type(BuiltinType::I32) => self.emit(Op::MulI32),
            IrNode::Type(BuiltinType::I64) => self.emit(Op::MulI64),
            IrNode::Type(BuiltinType::F32) => self.emit(Op::MulF32),
            IrNode::Type(BuiltinType::F64) => self.emit(Op::MulF64),
            _ => panic!(),
        }
    }

    fn emit_div(&mut self, nodes: &[IrNode], ty_id: &IrNodeId) {
        let ty = &nodes[*ty_id];
        match ty {
            IrNode::Type(BuiltinType::I32) => self.emit(Op::DivI32),
            IrNode::Type(BuiltinType::I64) => self.emit(Op::DivI64),
            IrNode::Type(BuiltinType::F32) => self.emit(Op::DivF32),
            IrNode::Type(BuiltinType::F64) => self.emit(Op::DivF64),
            _ => panic!(),
        }
    }

    fn emit_constant(&mut self, constant: &IrConstant) {
        match constant {
            IrConstant::Null => self.emit(Op::PushNull),
            IrConstant::True => self.emit(Op::PushTrue),
            IrConstant::False => self.emit(Op::PushFalse),
            IrConstant::I32(0) => self.emit(Op::PushZero),
            IrConstant::I32(1) => self.emit(Op::PushOne),
            IrConstant::I32(val) => {
                self.emit(Op::PushI32);
                self.code.extend(val.to_le_bytes());
            },
            IrConstant::U32(val) => {
                self.emit(Op::PushI32);
                self.code.extend(val.to_le_bytes());
            },
            IrConstant::F32(val) => {
                self.emit(Op::PushF32);
                self.code.extend(val.to_le_bytes());
            },
            IrConstant::String(s) => {
                let str_id = self.strings.len() as u16;
                self.strings.push(s.clone());

                self.emit(Op::PushString);
                self.code.extend(str_id.to_le_bytes());
            },
        }
    }

    fn emit(&mut self, op: Op) { self.code.push(op as u8); }

    fn emit_load_local(&mut self, slot: u16) {
        self.emit(Op::LoadLocal);
        self.code.extend(slot.to_le_bytes());
    }

    fn emit_store_local(&mut self, slot: u16) {
        self.emit(Op::StoreLocal);
        self.code.extend(slot.to_le_bytes());
    }

    fn emit_call(&mut self, func_id: u16, arg_count: u8) {
        self.emit(Op::Call);
        self.code.extend(func_id.to_le_bytes());
        self.code.push(arg_count);
    }

    fn mark_jump(&mut self, jump_kind: Op, dst_block_id: IrNodeId) {
        self.emit(jump_kind);

        self.marked_jumps.push(MarkedJump {
            offset: self.code.len() as u32,
            dst_block_id,
        });

        self.code.extend(&[0u8; 4]);
    }

    fn resolve_jumps(&mut self) {
        for jump in &self.marked_jumps {
            let target_offset = *self.label_offsets.get(&jump.dst_block_id).unwrap();
            let jump_distance = (target_offset as i32 - jump.offset as i32) as u32;
            let byte_offset = jump.offset as usize;
            self.code[byte_offset..byte_offset + 4].copy_from_slice(&jump_distance.to_le_bytes());
        }
    }
}

impl Default for CodeGenerator {
    fn default() -> Self { Self::new() }
}
