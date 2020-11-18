mod error;
mod file;
mod lexer;

fn main() {
    let chars = file::read_file_chars("/dev/stdin").unwrap();
    let tokens = lexer::read_file_tokens(chars).unwrap();
}
