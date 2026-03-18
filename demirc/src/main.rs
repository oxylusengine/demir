use std::path::PathBuf;

use ast::{self};
use codegen::{self, CodeGenerator};
use ir::{self, IrNode};
use sema::{self};
use vm::{VM, Value};

fn examples_path(relative: &str) -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .unwrap()
        .join(relative)
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let input = std::fs::read_to_string(examples_path("examples/hello_world.oxl")).unwrap();
    let mut ast = ast::parse(&input)?;
    ast = sema::analyze(ast)?;

    println!("=== IR ===");
    let ir_module = ir::lower_ast(ast);
    if let IrNode::Module {
        nodes,
        functions,
        globals,
    } = ir_module.clone()
    {
        for instr in globals {
            ir::disasm::print_ir(&nodes, &instr);
        }

        for instr in functions {
            ir::disasm::print_ir(&nodes, &instr);
        }
    }

    println!("=== BC ===");
    let mut codegen = CodeGenerator::new();
    let module = codegen.generate(&ir_module);
    match codegen::disasm::print_code(&module.code) {
        Ok(_) => {},
        Err(e) => panic!("{}", e),
    }

    println!("=== VM ===");
    let mut vm = VM::new(module);
    vm.define_external("printf", |stack| {
        let value = stack.pop()?;
        if let Value::String(str_id) = value {
            println!("'{}'", stack.string(str_id).expect("Cannot find string id"));
        }

        Ok(())
    });

    if let Err(e) = vm.execute_function(vm.find_function("main").unwrap()) {
        panic!("{}", e)
    }

    Ok(())
}
