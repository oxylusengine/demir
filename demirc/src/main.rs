use parser::Parser;

fn main() {
    let input = r#"
fn main() {
    var foo = 2;
    foo = foo + 2;
    println("hello world");
}
"#;

    let mut parser = Parser::new(input);
    let root_statement = parser.parse();
    match root_statement {
        Ok(_) => println!("{:?}", root_statement),
        Err(e) => println!("{}", e),
    };
}
