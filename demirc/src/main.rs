use parser::Parser;
use sema::{self};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let input = r#"
fn func(param1: i32) -> i32 {
    var bar = param1;
}

fn main() {
    var foo = 2;
    foo = func(foo);
}

"#;

    let mut parser = Parser::new(input);
    let root_statement = parser.parse()?;

    sema::analyze(root_statement)?;

    Ok(())
}
