use core::types::BuiltinType;
use std::collections::HashSet;

use ast::lowering::{self};
use codegen::{CodeGenerator, disasm};
use ir::{IrConstant, IrNode, IrNodeId};
use parser::{self};
use sema::{self};
use vm::VM;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let input = r#"
fn main() -> i32 {
    var i = 0;
    while true {
        i = i + 1;

        if i > 4 {
            break;
        }

        i = i + 1;
    }

    return i;
}
"#;

    let mut ast = parser::parse(input)?;
    ast = sema::analyze(ast)?;

    println!("=== IR ===");
    let ir_module = lowering::lower_ast(ast);
    if let IrNode::Module {
        nodes,
        functions,
        globals,
    } = ir_module.clone()
    {
        for instr in globals {
            print_ir(&nodes, &instr);
        }

        for instr in functions {
            print_ir(&nodes, &instr);
        }
    }

    println!("=== BC ===");
    let mut codegen = CodeGenerator::new();
    let module = codegen.generate(&ir_module);
    match disasm::print_code(&module.code) {
        Ok(_) => {},
        Err(e) => panic!("{}", e),
    }

    println!("=== VM ===");
    let mut vm = VM::new(module);
    let value = match vm.execute_function(0) {
        Ok(value) => value,
        Err(e) => panic!("{}", e),
    };

    println!("VM eval: {:?}", value);

    Ok(())
}

fn print_ir(nodes: &[IrNode], first_node_id: &IrNodeId) {
    let mut node_stack = Vec::new();
    let mut visited_nodes = HashSet::new();

    node_stack.push(*first_node_id);

    while let Some(node_id) = node_stack.pop() {
        if visited_nodes.contains(&node_id) {
            continue;
        }

        visited_nodes.insert(node_id);

        let node = &nodes[node_id];
        match node {
            IrNode::Module { functions, globals, .. } => {
                println!("Module");
                println!("  Functions: {:?}", functions);
                println!("  Globals: {:?}", globals);
            },

            IrNode::Constant(c) => {
                print!("%{} = Constant ", node_id);
                match c {
                    IrConstant::Null => println!("null"),
                    IrConstant::True => println!("true"),
                    IrConstant::False => println!("false"),
                    IrConstant::I32(s) => println!("i32 {}", s),
                    IrConstant::U32(s) => println!("u32 {}", s),
                    IrConstant::F32(s) => println!("f32 {}", s),
                    IrConstant::String(s) => println!("str {}", s),
                }
            },

            IrNode::Type(ty) => {
                print!("%{} = Type ", node_id);
                match ty {
                    BuiltinType::I8 => println!("i8"),
                    BuiltinType::U8 => println!("u8"),
                    BuiltinType::I16 => println!("i16"),
                    BuiltinType::U16 => println!("u16"),
                    BuiltinType::I32 => println!("i32"),
                    BuiltinType::U32 => println!("u32"),
                    BuiltinType::I64 => println!("i64"),
                    BuiltinType::U64 => println!("u64"),
                    BuiltinType::F32 => println!("f32"),
                    BuiltinType::F64 => println!("f64"),
                    BuiltinType::Bool => println!("bool"),
                    BuiltinType::String => println!("str"),
                    BuiltinType::Never => println!("never"),
                    BuiltinType::Unit => println!("unit"),
                    BuiltinType::Function { params, return_ty } => {
                        print!("function %{}", return_ty);
                        for param in params {
                            print!(" %{}", param)
                        }
                        println!()
                    },
                }
            },

            IrNode::Variable { ty } => {
                println!("%{} = Variable %{}", node_id, ty);
            },

            IrNode::Load { ty, variable } => {
                println!("%{} = Load %{} %{}", node_id, ty, variable);
            },

            IrNode::Store { src, dst } => {
                println!("%{} = Store %{} %{}", node_id, src, dst);
            },

            IrNode::Add { ty, lhs, rhs } => {
                println!("%{} = IAdd %{} %{} %{}", node_id, ty, lhs, rhs);
            },

            IrNode::Sub { ty, lhs, rhs } => {
                println!("%{} = ISub %{} %{} %{}", node_id, ty, lhs, rhs);
            },

            IrNode::Mul { ty, lhs, rhs } => {
                println!("%{} = IMul %{} %{} %{}", node_id, ty, lhs, rhs);
            },

            IrNode::Div { ty, lhs, rhs } => {
                println!("%{} = SDiv %{} %{} %{}", node_id, ty, lhs, rhs);
            },

            IrNode::Mod { ty, lhs, rhs } => {
                println!("%{} = SMod %{} %{} %{}", node_id, ty, lhs, rhs);
            },

            IrNode::BitAnd { ty, lhs, rhs } => {
                println!("%{} = BitwiseAnd %{} %{} %{}", node_id, ty, lhs, rhs);
            },

            IrNode::BitOr { ty, lhs, rhs } => {
                println!("%{} = BitwiseOr %{} %{} %{}", node_id, ty, lhs, rhs);
            },

            IrNode::BitXor { ty, lhs, rhs } => {
                println!("%{} = BitwiseXor %{} %{} %{}", node_id, ty, lhs, rhs);
            },

            IrNode::BitNot(a) => {
                println!("%{} = Not %{}", node_id, a);
            },

            IrNode::ShiftLeft { ty, lhs, rhs } => {
                println!("%{} = ShiftLeftLogical %{} %{} %{}", node_id, ty, lhs, rhs);
            },

            IrNode::ShiftRight { ty, lhs, rhs } => {
                println!("%{} = ShiftRightArithmetic %{} %{} %{}", node_id, ty, lhs, rhs);
            },

            IrNode::Equal { ty, lhs, rhs } => {
                println!("%{} = IEqual %{} %{} %{}", node_id, ty, lhs, rhs);
            },

            IrNode::NotEqual { ty, lhs, rhs } => {
                println!("%{} = INotEqual %{} %{} %{}", node_id, ty, lhs, rhs);
            },

            IrNode::GreaterThan { ty, lhs, rhs } => {
                println!("%{} = SGreaterThan %{} %{} %{}", node_id, ty, lhs, rhs);
            },

            IrNode::GreaterThanOrEqual { ty, lhs, rhs } => {
                println!("%{} = SGreaterThanEqual %{} %{} %{}", node_id, ty, lhs, rhs);
            },

            IrNode::LessThan { ty, lhs, rhs } => {
                println!("%{} = SLessThan %{} %{} %{}", node_id, ty, lhs, rhs);
            },

            IrNode::LessThanOrEqual { ty, lhs, rhs } => {
                println!("%{} = SLessThanEqual %{} %{} %{}", node_id, ty, lhs, rhs);
            },

            IrNode::LogicalAnd { ty, lhs, rhs } => {
                println!("%{} = LogicalAnd %{} %{} %{}", node_id, ty, lhs, rhs);
            },

            IrNode::LogicalOr { ty, lhs, rhs } => {
                println!("%{} = LogicalOr %{} %{} %{}", node_id, ty, lhs, rhs);
            },

            IrNode::LogicalNot(a) => {
                println!("%{} = LogicalNot %{}", node_id, a);
            },

            IrNode::Label(instructions) => {
                println!("; Block marker");
                println!("%{} = Label", node_id);
                instructions.iter().rev().for_each(|instr| node_stack.push(*instr));
            },

            IrNode::Return(val) => {
                if let Some(v) = val {
                    println!("%{} = ReturnValue %{}", node_id, v);
                } else {
                    println!("%{} = Return", node_id);
                }
                println!("; Ongoing block terminated");
            },

            IrNode::Branch(target) => {
                println!("%{node_id} = Branch %{}", target);
                println!("; Ongoing block terminated");
                node_stack.push(*target);
            },

            IrNode::ConditionalBranch {
                condition,
                true_block,
                false_block,
            } => {
                println!(
                    "%{node_id} = BranchConditional %{} %{} %{}",
                    condition, true_block, false_block
                );
                println!("; Ongoing block terminated");
                node_stack.push(*false_block);
                node_stack.push(*true_block);
            },

            IrNode::ExternalFunction { ty } => {
                println!("%{} = ExternalFunction %{}", node_id, ty);
            },

            IrNode::Function { ty, starter_block } => {
                println!("%{} = Function %{}", node_id, ty);
                node_stack.push(*starter_block);
            },

            IrNode::FunctionParam { ty } => {
                println!("%{} = FunctionParameter %{}", node_id, ty);
            },

            IrNode::Call { callee, args } => {
                print!("%{} = FunctionCall %{}", node_id, callee);
                for arg in args {
                    print!(" %{}", arg);
                }
                println!();
            },
        }
    }
}
