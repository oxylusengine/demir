use parser::{self};
use sema::{self};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let input = r#"
fn func() -> i32 {}
fn main() {
    var foo = func();
}

"#;

    let mut ast = parser::parse(input)?;
    ast = sema::analyze(ast)?;

    Ok(())
}
