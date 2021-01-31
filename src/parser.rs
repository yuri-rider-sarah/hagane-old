use crate::error::*;
use crate::lexer::*;

#[derive(Clone, Debug)]
pub enum UExpr {
    IntLiteral(i64),
    Ident(String),
    Tuple(Vec<Expr>),
    Call(Box<Expr>, Vec<Expr>),
    Let(Box<Expr>, Box<Expr>),
    Set(Box<Expr>, Box<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    While(Box<Expr>, Box<Expr>),
    Do(Vec<Expr>),
    Lambda(Vec<Expr>, Box<Expr>),
}

#[derive(Clone, Debug)]
pub struct Expr(pub UExpr, pub Option<Type>);

#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    Named(String),
    Tuple(Vec<Type>),
    Function(Vec<Type>, Box<Type>),
}

fn read_expr(tokens: &mut Tokens) -> Result<Expr> {
    let mut expr = read_simple_expr(tokens)?;
    loop {
        match read_token(tokens)? {
            Token::LParen => expr = Expr(
                UExpr::Call(Box::new(expr), read_arg_list_r(tokens)?),
                read_type_signature(tokens)?,
            ),
            _ => {
                undo_read_token(tokens);
                return Ok(expr);
            },
        }
    }
}

#[derive(Debug)]
enum Clause {
    SecKeyword(String),
    Block(Vec<Expr>),
}

fn uexpr_from_clauses(keyword: &str, clauses: &Vec<Clause>) -> Result<UExpr> {
    use Clause::*;
    Ok(match &keyword[..] {
        "let" => match &clauses[..] {
            [Block(var), Block(val)] => match &var[..] {
                [var] => UExpr::Let(Box::new(var.clone()), Box::new(Expr(UExpr::Do(val.clone()), None))),
                _ => return Err(Error::InvalidExpr),
            },
            _ => return Err(Error::InvalidExpr),
        },
        "set" => match &clauses[..] {
            [Block(var), Block(val)] => match &var[..] {
                [var] => UExpr::Set(Box::new(var.clone()), Box::new(Expr(UExpr::Do(val.clone()), None))),
                _ => return Err(Error::InvalidExpr),
            },
            _ => return Err(Error::InvalidExpr),
        },
        "do" => match &clauses[..] {
            [Block(exprs)] => UExpr::Do(exprs.clone()),
            _ => return Err(Error::InvalidExpr),
        },
        "if" => {
            let (cond, then, else_) = match &clauses[..] {
                [Block(cond), Block(then), Block(else_)] => (cond, then, else_),
                [Block(cond), SecKeyword(then_kw), Block(then), Block(else_)]
                    if then_kw == "then" => (cond, then, else_),
                [Block(cond), Block(then), SecKeyword(else_kw), Block(else_)]
                    if else_kw == "else"=> (cond, then, else_),
                [Block(cond), SecKeyword(then_kw), Block(then), SecKeyword(else_kw), Block(else_)]
                    if then_kw == "then" && else_kw == "else" => (cond, then, else_),
                _ => return Err(Error::InvalidExpr),
            };
            UExpr::If(
                Box::new(Expr(UExpr::Do(cond.clone()), None)),
                Box::new(Expr(UExpr::Do(then.clone()), None)),
                Box::new(Expr(UExpr::Do(else_.clone()), None)),
            )
        },
        "while" => {
            let (cond, body) = match &clauses[..] {
                [Block(cond), Block(body)] => (cond, body),
                [Block(cond), SecKeyword(do_kw), Block(body)] if do_kw == "do" => (cond, body),
                _ => return Err(Error::InvalidExpr),
            };
            UExpr::While(
                Box::new(Expr(UExpr::Do(cond.clone()), None)),
                Box::new(Expr(UExpr::Do(body.clone()), None)),
            )
        },
        "λ" => {
            let (args, body) = match &clauses[..] {
                [Block(args), Block(body)] => (args, body),
                [Block(args), SecKeyword(arrow_kw), Block(body)] if arrow_kw == "⇒" => (args, body),
                _ => return Err(Error::InvalidExpr),
            };
            UExpr::Lambda(args.clone(), Box::new(Expr(UExpr::Do(body.clone()), None)))
        },
        _ => return Err(Error::InvalidExpr),
    })
}

fn read_simple_expr(tokens: &mut Tokens) -> Result<Expr> {
    let uexpr = match read_token(tokens)? {
        Token::IntLiteral(n) => UExpr::IntLiteral(n),
        Token::Ident(n) => UExpr::Ident(n),
        Token::Hash => UExpr::Tuple(read_arg_list(tokens)?),
        Token::PriKeyword(keyword) => {
            let t = read_token(tokens)?;
            if t != Token::LParen {
                return Err(Error::UnexpectedToken(Some(t)));
            }
            let mut clauses = Vec::new();
            loop {
                clauses.push(match read_token_or_indent(tokens)? {
                    Token::SecKeyword(keyword) => Clause::SecKeyword(keyword),
                    Token::LBrace => Clause::Block(read_block_brace_r(tokens)?),
                    Token::LIndent => Clause::Block(read_block_indent_r(tokens)?),
                    Token::RParen => break,
                    _ => {
                        undo_read_token(tokens);
                        Clause::Block(vec![read_expr(tokens)?])
                    },
                });
            }
            uexpr_from_clauses(&keyword, &clauses)?
        },
        t => return Err(Error::UnexpectedToken(Some(t))),
    };
    Ok(Expr(uexpr, read_type_signature(tokens)?))
}

pub fn read_block_expr(tokens: &mut Tokens) -> Result<Expr> {
    Ok(match read_token(tokens)? {
        Token::PriKeyword(keyword) => {
            let mut clauses = Vec::new();
            loop {
                let at_line_start = lexer_at_line_start(tokens);
                clauses.push(match read_token_or_indent(tokens)? {
                    Token::SecKeyword(keyword) => Clause::SecKeyword(keyword),
                    Token::LBrace => Clause::Block(read_block_brace_r(tokens)?),
                    Token::LIndent => Clause::Block(read_block_indent_r(tokens)?),
                    Token::RBrace | Token::RIndent => {
                        undo_read_token(tokens);
                        break;
                    },
                    _ => {
                        undo_read_token(tokens);
                        if at_line_start {
                            break;
                        }
                        Clause::Block(vec![read_expr(tokens)?])
                    },
                });
            }
            Expr(uexpr_from_clauses(&keyword, &clauses)?, None)
        },
        _ => {
            undo_read_token(tokens);
            read_expr(tokens)?
        },
    })
}

fn read_bracketed_list_r<T>(
    tokens: &mut Tokens,
    is_right: fn(&Token) -> bool,
    read_element: fn(&mut Tokens,
) -> Result<T>) -> Result<Vec<T>> {
    let mut v = Vec::new();
    loop {
        let t = read_token(tokens)?;
        if is_right(&t) {
            return Ok(v);
        } else {
            undo_read_token(tokens);
            v.push(read_element(tokens)?);
        }
    }
}

fn read_bracketed_list<T>(
    tokens: &mut Tokens,
    is_left: fn(&Token) -> bool,
    is_right: fn(&Token) -> bool,
    read_element: fn(&mut Tokens,
) -> Result<T>) -> Result<Vec<T>> {
    let t = read_token(tokens)?;
    if !is_left(&t) {
        return Err(Error::UnexpectedToken(Some(t)));
    }
    read_bracketed_list_r(tokens, is_right, read_element)
}

fn read_block_brace_r(tokens: &mut Tokens) -> Result<Vec<Expr>> {
    read_bracketed_list_r(tokens, |t| *t == Token::RBrace, read_block_expr)
}

fn read_block_indent_r(tokens: &mut Tokens) -> Result<Vec<Expr>> {
    read_bracketed_list_r(tokens, |t| *t == Token::RIndent, read_block_expr)
}

fn read_arg_list(tokens: &mut Tokens) -> Result<Vec<Expr>> {
    read_bracketed_list(tokens, |t| *t == Token::LParen, |t| *t == Token::RParen, read_expr)
}

fn read_arg_list_r(tokens: &mut Tokens) -> Result<Vec<Expr>> {
    read_bracketed_list_r(tokens, |t| *t == Token::RParen, read_expr)
}

fn read_type_signature(tokens: &mut Tokens) -> Result<Option<Type>> {
    Ok(match read_token(tokens)? {
        Token::Apostrophe => {
            Some(read_type(tokens)?)
        },
        _ => {
            undo_read_token(tokens);
            None
        },
    })
}

fn read_type(tokens: &mut Tokens) -> Result<Type> {
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

fn read_func_type_param_list(tokens: &mut Tokens) -> Result<Vec<Type>> {
    read_bracketed_list(tokens, |t| *t == Token::LParen, |t| *t == Token::RParen, read_type)
}

fn read_func_type_param_list_rc(tokens: &mut Tokens) -> Result<Vec<Type>> {
    read_bracketed_list_r(tokens, |t| *t == Token::Colon, read_type)
}
