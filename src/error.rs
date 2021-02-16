use crate::lexer::Token;
use crate::parser::Type;

#[derive(Debug)]
pub enum Error {
    UnexpectedChar(Option<char>),
    UnexpectedToken(Token),
    InvalidExpr,
    UnboundVariable(String),
    UnboundType(String),
    ConflictingType(Type, Type),
    AmbiguousType,
    MismatchedArgs(Vec<Type>, Vec<Type>),
    MismatchedParamsNum(usize, usize),
    NotAFunction(Type),
    AssignToExpr,
    InconsistentIndentation(char, char),
    Unimplemented,
}

pub type Result<T> = std::result::Result<T, Error>;
