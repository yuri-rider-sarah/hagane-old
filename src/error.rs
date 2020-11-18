#[derive(Debug)]
pub enum Error {
    UnexpectedChar(Option<char>),
}

pub type Result<T> = std::result::Result<T, Error>;
