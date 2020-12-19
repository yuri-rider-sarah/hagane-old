use crate::error::*;
use crate::lexer::Token;

#[derive(Debug)]
pub enum UExpr {
    IntLiteral(i64),
    Ident(String),
    Tuple(Vec<Expr>),
    Call(Box<Expr>, Vec<Expr>),
    Let(Box<Expr>, Box<Expr>),
    Set(Box<Expr>, Box<Expr>),
    Match(Box<Expr>, Vec<(Expr, Vec<Expr>)>),
    If(Box<Expr>, Vec<Expr>, Vec<Expr>),
    While(Box<Expr>, Vec<Expr>),
    Do(Vec<Expr>),
    Lambda(Vec<Expr>, Vec<Expr>),
}

#[derive(Debug)]
pub struct Expr(pub UExpr, pub Option<Type>);

#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    Named(String),
    Tuple(Vec<Type>),
    Function(Vec<Type>, Box<Type>),
}

fn read_token(tokens: &mut Vec<Token>) -> Result<Token> {
    tokens.pop().ok_or(Error::UnexpectedToken(None))
}

pub fn read_expr(tokens: &mut Vec<Token>) -> Result<Expr> {
    Ok(match read_token(tokens)? {
        Token::IntLiteral(n) => Expr(
            UExpr::IntLiteral(n),
            read_type_signature(tokens)?,
        ),
        Token::Ident(n) => Expr(
            UExpr::Ident(n),
            read_type_signature(tokens)?,
        ),
        Token::Let => Expr(
            UExpr::Let(Box::new(read_expr(tokens)?), Box::new(read_expr(tokens)?)),
            None,
        ),
        Token::Set => Expr(
            UExpr::Set(Box::new(read_expr(tokens)?), Box::new(read_expr(tokens)?)),
            None,
        ),
        Token::Match => Expr(
            UExpr::Match(Box::new(read_expr(tokens)?), read_match_body(tokens)?),
            None,
        ),
        Token::If => {
            let cond = read_expr(tokens)?;
            let then_token = read_token(tokens)?;
            if then_token != Token::Then {
                return Err(Error::UnexpectedToken(Some(then_token)));
            }
            let then = read_block(tokens)?;
            let else_token = read_token(tokens)?;
            if else_token != Token::Else {
                return Err(Error::UnexpectedToken(Some(else_token)));
            }
            let else_ = read_block(tokens)?;
            Expr(
                UExpr::If(Box::new(cond), then, else_),
                read_type_signature(tokens)?,
            )
        },
        Token::While => Expr(
            UExpr::While(Box::new(read_expr(tokens)?), read_block(tokens)?),
            None,
        ),
        Token::Do => Expr(
            UExpr::Do(read_block(tokens)?),
            None,
        ),
        Token::LParen => Expr(
            UExpr::Call(Box::new(read_expr(tokens)?), read_arg_list_r(tokens)?),
            read_type_signature(tokens)?,
        ),
        Token::Lambda => Expr(
            UExpr::Lambda(read_arg_list(tokens)?, read_block(tokens)?),
            read_type_signature(tokens)?,
        ),
        Token::Hash => Expr(
            UExpr::Tuple(read_arg_list(tokens)?),
            read_type_signature(tokens)?,
        ),
        t => return Err(Error::UnexpectedToken(Some(t))),
    })
}

fn read_match_branch(tokens: &mut Vec<Token>) -> Result<(Expr, Vec<Expr>)> {
    let pattern = read_expr(tokens)?;
    let middle = read_token(tokens)?;
    if middle != Token::Colon {
        return Err(Error::UnexpectedToken(Some(middle)));
    }
    let body = read_block(tokens)?;
    Ok((pattern, body))
}

fn read_bracketed_list_r<T>(
    tokens: &mut Vec<Token>,
    is_right: fn(&Token) -> bool,
    read_element: fn(&mut Vec<Token>,
) -> Result<T>) -> Result<Vec<T>> {
    let mut v = Vec::new();
    loop {
        let t = read_token(tokens)?;
        if is_right(&t) {
            return Ok(v);
        } else {
            tokens.push(t);
            v.push(read_element(tokens)?);
        }
    }
}

fn read_bracketed_list<T>(
    tokens: &mut Vec<Token>,
    is_left: fn(&Token) -> bool,
    is_right: fn(&Token) -> bool,
    read_element: fn(&mut Vec<Token>,
) -> Result<T>) -> Result<Vec<T>> {
    let t = read_token(tokens)?;
    if !is_left(&t) {
        return Err(Error::UnexpectedToken(Some(t)));
    }
    read_bracketed_list_r(tokens, is_right, read_element)
}

fn read_block(tokens: &mut Vec<Token>) -> Result<Vec<Expr>> {
    read_bracketed_list(tokens, |t| *t == Token::LBrace, |t| *t == Token::RBrace, read_expr)
}

fn read_match_body(tokens: &mut Vec<Token>) -> Result<Vec<(Expr, Vec<Expr>)>> {
    read_bracketed_list(tokens, |t| *t == Token::LBrace, |t| *t == Token::RBrace, read_match_branch)
}

fn read_arg_list(tokens: &mut Vec<Token>) -> Result<Vec<Expr>> {
    read_bracketed_list(tokens, |t| *t == Token::LParen, |t| *t == Token::RParen, read_expr)
}

fn read_arg_list_r(tokens: &mut Vec<Token>) -> Result<Vec<Expr>> {
    read_bracketed_list_r(tokens, |t| *t == Token::RParen, read_expr)
}

fn read_type_signature(tokens: &mut Vec<Token>) -> Result<Option<Type>> {
    Ok(match tokens.last() {
        Some(Token::Apostrophe) => {
            let _ = read_token(tokens)?;
            Some(read_type(tokens)?)
        },
        _ => None,
    })
}

fn read_type(tokens: &mut Vec<Token>) -> Result<Type> {
    Ok(match read_token(tokens)? {
        Token::Ident(name) => Type::Named(name),
        Token::Hash => Type::Tuple(read_func_type_param_list(tokens)?),
        Token::LParen => {
            let params = read_func_type_param_list_rc(tokens)?;
            let ret = read_type(tokens)?;
            let right = read_token(tokens)?;
            if right != Token::RParen {
                return Err(Error::UnexpectedToken(Some(right)));
            }
            Type::Function(params, Box::new(ret))
        },
        t => return Err(Error::UnexpectedToken(Some(t))),
    })
}

fn read_func_type_param_list(tokens: &mut Vec<Token>) -> Result<Vec<Type>> {
    read_bracketed_list(tokens, |t| *t == Token::LParen, |t| *t == Token::RParen, read_type)
}

fn read_func_type_param_list_rc(tokens: &mut Vec<Token>) -> Result<Vec<Type>> {
    read_bracketed_list_r(tokens, |t| *t == Token::Colon, read_type)
}
