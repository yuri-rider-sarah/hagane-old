mod error;
mod lexer;
mod parser;

fn main() {
    let mut chars: Vec<_> = std::fs::read_to_string("/dev/stdin").unwrap().chars().collect();
    chars.reverse();
    let mut tokens = lexer::read_file_tokens(chars).unwrap();
    println!("{:#?}", parser::read_expr(&mut tokens).unwrap());
}
