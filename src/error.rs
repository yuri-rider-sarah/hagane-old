#[derive(Debug)]
pub enum Error {
    UnexpectedChar(Option<char>),
    UnexpectedToken(Option<crate::lexer::Token>),
}

pub type Result<T> = std::result::Result<T, Error>;
