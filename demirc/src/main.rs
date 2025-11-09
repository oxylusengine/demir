use parser::Parser;
use sema::{self};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let input = r#"
fn main() {
    var foo = 2;
}
"#;

    let mut parser = Parser::new(input);
    let root_statement = parser.parse()?;

    println!("{:?}", root_statement);

    sema::analyze(root_statement)?;

    Ok(())
}
