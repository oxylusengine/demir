use lexer::tokenize;

fn main() {
    let input = r#"
fn main() {
    println("hello
    multiline strings");
}
"#;

    let tokens = tokenize(input);
    tokens.iter().for_each(|x| {
        println!("{:?}", x);
    });
}
