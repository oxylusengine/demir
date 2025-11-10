use parser::Parser;
use sema::{self};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let input = r#"
fn func(foo: i32) -> i32 {
}

fn main() {
    var real: f32;
    var foo = 2;
    foo = func(real);
}

"#;

    let mut parser = Parser::new(input);
    let root_statement = parser.parse()?;

    sema::analyze(root_statement)?;

    Ok(())
}
