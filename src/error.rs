use crate::lexer::Token;
use crate::parser::Expr;
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
    AssignToConst,
    InconsistentIndentation(char, char),
    InvalidType(Expr),
    InvalidPattern(Expr),
    NonParametrizableType(Type),
    MismatchedTypeArgsNum(usize, usize),
    NotPolymorphic,
    PolymorphicMut,
    ConflictingPatternType,
    NotAConstructor(String),
    TypeNotInFFI(Type),
}

pub type Result<T> = std::result::Result<T, Error>;
