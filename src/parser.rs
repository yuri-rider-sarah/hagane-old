use crate::error::*;
use crate::lexer::*;

#[derive(Clone, Debug)]
pub enum UExpr {
    IntLiteral(i64),
    Ident(String),
    Wildcard,
    Tuple(Vec<Expr>),
    Function(Vec<Expr>, Box<Expr>),
    Call(Box<Expr>, Vec<Expr>),
    Monomorphic(Box<Expr>, Vec<Type>),
    Let(Option<Vec<String>>, Pattern, Vec<Expr>, bool),
    Set(Box<Expr>, Vec<Expr>),
    If(Vec<Expr>, Vec<Expr>, Vec<Expr>),
    While(Vec<Expr>, Vec<Expr>),
    Do(Vec<Expr>),
    Lambda(Vec<Expr>, Vec<Expr>),
    Cond(Vec<(Expr, Vec<Expr>)>),
    Type(String, Option<Vec<String>>, Vec<(String, Vec<Type>)>),
    Match(Box<Expr>, Vec<(Pattern, Vec<Expr>)>),
    List(Vec<Vec<Expr>>),
    Extern(String, Type),
}

#[derive(Clone, Debug)]
pub struct Expr(pub UExpr, pub Option<Type>);

#[derive(Clone, Debug)]
pub enum Pattern {
    IntLiteral(i64),
    Ident(String),
    Wildcard,
    Tuple(Vec<Pattern>),
    Variant(String, Vec<Pattern>),
    List(Vec<Pattern>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    Named(String),
    Applied(Box<Type>, Vec<Type>),
    Tuple(Vec<Type>),
    Function(Vec<Type>, Box<Type>),
    Forall(Vec<String>, Box<Type>),
}

fn read_expr(tokens: &mut Tokens) -> Result<Expr> {
    let mut expr = read_simple_expr(tokens)?;
    loop {
        let state = get_lexer_state(tokens);
        match read_token(tokens)? {
            Token::LParen => expr = Expr(
                UExpr::Call(Box::new(expr), read_arg_list_r(tokens)?),
                read_type_signature(tokens)?,
            ),
            Token::LBracket => expr = Expr(
                UExpr::Monomorphic(Box::new(expr), read_type_param_list_r(tokens)?),
                read_type_signature(tokens)?,
            ),
            _ => {
                restore_lexer_state(tokens, state);
                return Ok(expr);
            },
        }
    }
}

#[derive(Debug)]
enum Clause {
    SecKeyword(String),
    Label(Expr),
    Block(Vec<Expr>),
}

fn uexpr_from_clauses(keyword: &str, clauses: &Vec<Clause>) -> Result<UExpr> {
    use Clause::*;
    Ok(match &keyword[..] {
        "let" => match &clauses[..] {
            [Block(var), Block(val)] => match &var[..] {
                [var] => UExpr::Let(None, expr_to_pattern(var.clone())?, val.clone(), false),
                _ => return Err(Error::InvalidExpr),
            },
            [SecKeyword(mut_kw), Block(var), Block(val)] if mut_kw == "mut" => match &var[..] {
                [var] => UExpr::Let(None, expr_to_pattern(var.clone())?, val.clone(), true),
                _ => return Err(Error::InvalidExpr),
            },
            [SecKeyword(fa_kw), Block(type_param_exprs), Block(var), Block(val)] if fa_kw == "∀" => match &var[..] {
                [var] => {
                    let mut type_params = Vec::new();
                    for Expr(uexpr, stated_type) in type_param_exprs {
                        if *stated_type != None {
                            return Err(Error::InvalidExpr);
                        }
                        match uexpr {
                            UExpr::Ident(param) => type_params.push(param.clone()),
                            _ => return Err(Error::InvalidExpr),
                        }
                    }
                    UExpr::Let(Some(type_params), expr_to_pattern(var.clone())?, val.clone(), false)
                },
                _ => return Err(Error::InvalidExpr),
            },
            _ => return Err(Error::InvalidExpr),
        },
        "set" => match &clauses[..] {
            [Block(var), Block(val)] => match &var[..] {
                [var] => UExpr::Set(Box::new(var.clone()), val.clone()),
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
                cond.clone(),
                then.clone(),
                else_.clone(),
            )
        },
        "while" => {
            let (cond, body) = match &clauses[..] {
                [Block(cond), Block(body)] => (cond, body),
                [Block(cond), SecKeyword(do_kw), Block(body)] if do_kw == "do" => (cond, body),
                _ => return Err(Error::InvalidExpr),
            };
            UExpr::While(
                cond.clone(),
                body.clone(),
            )
        },
        "λ" => {
            let (args, body) = match &clauses[..] {
                [Block(args), Block(body)] => (args, body),
                [Block(args), SecKeyword(arrow_kw), Block(body)] if arrow_kw == "⇒" => (args, body),
                _ => return Err(Error::InvalidExpr),
            };
            UExpr::Lambda(args.clone(), body.clone())
        },
        "cond" => {
            if clauses.len() % 2 != 0 {
                return Err(Error::InvalidExpr);
            }
            let mut cases = Vec::new();
            for i in 0 .. clauses.len() / 2 {
                match (&clauses[2 * i], &clauses[2 * i + 1]) {
                    (Label(cond), Block(body)) => cases.push((cond.clone(), body.clone())),
                    _ => return Err(Error::InvalidExpr),
                }
            }
            UExpr::Cond(cases)
        },
        "type" => match &clauses[..] {
            [Block(name_exprs), Block(variant_exprs)] => {
                let name = match &name_exprs[..] {
                    [Expr(UExpr::Ident(name), None)] => name.clone(),
                    _ => return Err(Error::InvalidExpr),
                };
                let mut variants = Vec::new();
                for variant_expr in variant_exprs {
                    variants.push(match variant_expr {
                        Expr(UExpr::Call(name_expr, arg_exprs), None) => match &**name_expr {
                            Expr(UExpr::Ident(name), None) => {
                                let mut args = Vec::new();
                                for arg_expr in arg_exprs {
                                    args.push(expr_to_type(arg_expr.clone())?);
                                };
                                (name.clone(), args)
                            },
                            _ => return Err(Error::InvalidExpr),
                        },
                        _ => return Err(Error::InvalidExpr),
                    });
                }
                UExpr::Type(name, None, variants)
            },
            [SecKeyword(fa_kw), Block(type_param_exprs), Block(name_exprs), Block(variant_exprs)] if fa_kw == "∀" => {
                let name = match &name_exprs[..] {
                    [Expr(UExpr::Ident(name), None)] => name.clone(),
                    _ => return Err(Error::InvalidExpr),
                };
                let mut type_params = Vec::new();
                for type_param_expr in type_param_exprs {
                    type_params.push(match type_param_expr {
                        Expr(UExpr::Ident(name), None) => name.to_string(),
                        _ => return Err(Error::InvalidExpr),
                    });
                }
                let mut variants = Vec::new();
                for variant_expr in variant_exprs {
                    variants.push(match variant_expr {
                        Expr(UExpr::Call(name_expr, arg_exprs), None) => match &**name_expr {
                            Expr(UExpr::Ident(name), None) => {
                                let mut args = Vec::new();
                                for arg_expr in arg_exprs {
                                    args.push(expr_to_type(arg_expr.clone())?);
                                };
                                (name.clone(), args)
                            },
                            _ => return Err(Error::InvalidExpr),
                        },
                        _ => return Err(Error::InvalidExpr),
                    });
                }
                UExpr::Type(name, Some(type_params), variants)
            },
            _ => return Err(Error::InvalidExpr),
        },
        "match" => {
            if clauses.len() % 2 != 1 {
                return Err(Error::InvalidExpr);
            }
            let val = match &clauses[0] {
                Block(val) => match &val[..] {
                    [val] => val.clone(),
                    _ => return Err(Error::InvalidExpr),
                },
                _ => return Err(Error::InvalidExpr),
            };
            let mut cases = Vec::new();
            for i in 0 .. clauses.len() / 2 {
                match (&clauses[2 * i + 1], &clauses[2 * i + 2]) {
                    (Label(pattern), Block(case)) => cases.push((expr_to_pattern(pattern.clone())?, case.clone())),
                    _ => return Err(Error::InvalidExpr),
                }
            }
            UExpr::Match(Box::new(val), cases)
        },
        "list" => {
            let mut elements = Vec::new();
            for clause in clauses {
                match clause {
                    Block(element) => elements.push(element.clone()),
                    _ => return Err(Error::InvalidExpr),
                }
            }
            UExpr::List(elements)
        },
        "extern" => match &clauses[..] {
            [Block(name_exprs), Block(type_exprs)] => match (&name_exprs[..], &type_exprs[..]) {
                ([Expr(UExpr::Ident(name), None)], [type_expr]) => UExpr::Extern(name.clone(), expr_to_type(type_expr.clone())?),
                _ => return Err(Error::InvalidExpr),
            }
            _ => return Err(Error::InvalidExpr),
        },
        _ => return Err(Error::InvalidExpr),
    })
}

fn read_simple_expr(tokens: &mut Tokens) -> Result<Expr> {
    let uexpr = match read_token(tokens)? {
        Token::IntLiteral(n) => UExpr::IntLiteral(n),
        Token::CharLiteral(c) => UExpr::IntLiteral(c as i64),
        Token::StringLiteral(s) => UExpr::List(s.chars().map(|c| vec![Expr(UExpr::IntLiteral(c as i64), None)]).collect()),
        Token::Ident(n) => UExpr::Ident(n),
        Token::Wildcard => UExpr::Wildcard,
        Token::Hash => UExpr::Tuple(read_arg_list(tokens)?),
        Token::DoubleDagger => {
            let params = read_arg_list_c(tokens)?;
            let ret = read_expr(tokens)?;
            let right = read_token(tokens)?;
            if right != Token::RParen {
                return Err(Error::UnexpectedToken(right));
            }
            UExpr::Function(params, Box::new(ret))
        }
        Token::PriKeyword(keyword) => {
            let indent_depth = lexer_indent_depth(tokens);
            let t = read_token(tokens)?;
            if t != Token::LParen {
                return Err(Error::UnexpectedToken(t));
            }
            let mut clauses = Vec::new();
            loop {
                let state = get_lexer_state(tokens);
                clauses.push(match read_token_or_indent(tokens, indent_depth)? {
                    Token::SecKeyword(keyword) => Clause::SecKeyword(keyword),
                    Token::Period => Clause::Label(read_expr(tokens)?),
                    Token::LBrace => Clause::Block(read_block_brace_r(tokens)?),
                    Token::LIndent => {
                        let state_indent = get_lexer_state(tokens);
                        match read_token(tokens)? {
                            Token::SecKeyword(keyword) => {
                                restore_lexer_state(tokens, state);
                                let _ = read_token(tokens)?;
                                Clause::SecKeyword(keyword)
                            },
                            Token::Period => {
                                restore_lexer_state(tokens, state);
                                let _ = read_token(tokens)?;
                                Clause::Label(read_expr(tokens)?)
                            },
                            Token::LBrace => {
                                restore_lexer_state(tokens, state);
                                let _ = read_token(tokens)?;
                                Clause::Block(read_block_brace_r(tokens)?)
                            },
                            _ => {
                                restore_lexer_state(tokens, state_indent);
                                Clause::Block(read_block_indent_r(tokens)?)
                            },
                        }
                    },
                    Token::RParen => break,
                    _ => {
                        restore_lexer_state(tokens, state);
                        Clause::Block(vec![read_expr(tokens)?])
                    },
                });
            }
            uexpr_from_clauses(&keyword, &clauses)?
        },
        t => return Err(Error::UnexpectedToken(t)),
    };
    Ok(Expr(uexpr, read_type_signature(tokens)?))
}

pub fn read_block_expr(tokens: &mut Tokens) -> Result<Expr> {
    let state = get_lexer_state(tokens);
    Ok(match read_token(tokens)? {
        Token::PriKeyword(keyword) => {
            let state_kw = get_lexer_state(tokens);
            let indent_depth = lexer_indent_depth(tokens);
            match read_token(tokens)? {
                Token::LParen => {
                    restore_lexer_state(tokens, state);
                    read_expr(tokens)?
                },
                _ => {
                    restore_lexer_state(tokens, state_kw);
                    let mut clauses = Vec::new();
                    loop {
                        let state_loop = get_lexer_state(tokens);
                        let at_line_start = lexer_at_line_start(tokens);
                        clauses.push(match read_token_or_indent(tokens, indent_depth)? {
                            Token::SecKeyword(keyword) => Clause::SecKeyword(keyword),
                            Token::Period => Clause::Label(read_expr(tokens)?),
                            Token::LBrace => Clause::Block(read_block_brace_r(tokens)?),
                            Token::LIndent => {
                                let state_indent = get_lexer_state(tokens);
                                match read_token(tokens)? {
                                    Token::SecKeyword(keyword) => {
                                        restore_lexer_state(tokens, state_loop);
                                        let _ = read_token(tokens)?;
                                        Clause::SecKeyword(keyword)
                                    },
                                    Token::Period => {
                                        restore_lexer_state(tokens, state_loop);
                                        let _ = read_token(tokens)?;
                                        Clause::Label(read_expr(tokens)?)
                                    },
                                    Token::LBrace => {
                                        restore_lexer_state(tokens, state_loop);
                                        let _ = read_token(tokens)?;
                                        Clause::Block(read_block_brace_r(tokens)?)
                                    },
                                    _ => {
                                        restore_lexer_state(tokens, state_indent);
                                        Clause::Block(read_block_indent_r(tokens)?)
                                    },
                                }
                            },
                            Token::RBrace | Token::RIndent | Token::Eof => {
                                restore_lexer_state(tokens, state_loop);
                                break;
                            },
                            _ => {
                                restore_lexer_state(tokens, state_loop);
                                if at_line_start {
                                    break;
                                }
                                Clause::Block(vec![read_clause_expr(tokens)?])
                            },
                        });
                    }
                    Expr(uexpr_from_clauses(&keyword, &clauses)?, None)
                },
            }
        },
        _ => {
            restore_lexer_state(tokens, state);
            read_expr(tokens)?
        },
    })
}

fn read_clause_special_expr_clauses(tokens: &mut Tokens, indent_depth: usize) -> Result<Vec<Clause>> {
    let mut past_indent = false;
    let mut clauses = Vec::new();
    loop {
        let state = get_lexer_state(tokens);
        let at_line_start = lexer_at_line_start(tokens);
        clauses.push(match read_token_or_indent(tokens, indent_depth)? {
            Token::SecKeyword(keyword) => {
                if at_line_start && !past_indent {
                    restore_lexer_state(tokens, state);
                    break;
                }
                Clause::SecKeyword(keyword)
            },
            Token::Period => {
                if at_line_start && !past_indent {
                    restore_lexer_state(tokens, state);
                    break;
                }
                Clause::Label(read_expr(tokens)?)
            },
            Token::LBrace => {
                if at_line_start && !past_indent {
                    restore_lexer_state(tokens, state);
                    break;
                }
                Clause::Block(read_block_brace_r(tokens)?)
            },
            Token::LIndent => {
                let state_indent = get_lexer_state(tokens);
                match read_token(tokens)? {
                    Token::SecKeyword(keyword) => {
                        if past_indent {
                            restore_lexer_state(tokens, state);
                            let _ = read_token(tokens)?;
                        } else {
                            past_indent = true;
                        }
                        Clause::SecKeyword(keyword)
                    },
                    Token::Period => {
                        if past_indent {
                            restore_lexer_state(tokens, state);
                            let _ = read_token(tokens)?;
                        } else {
                            past_indent = true;
                        }
                        Clause::Label(read_expr(tokens)?)
                    },
                    Token::LBrace => {
                        if past_indent {
                            restore_lexer_state(tokens, state);
                            let _ = read_token(tokens)?;
                        } else {
                            past_indent = true;
                        }
                        Clause::Block(read_block_brace_r(tokens)?)
                    },
                    _ => {
                        restore_lexer_state(tokens, state_indent);
                        Clause::Block(read_block_indent_r(tokens)?)
                    },
                }
            },
            Token::RBrace | Token::RIndent | Token::Eof => {
                restore_lexer_state(tokens, state);
                break;
            },
            _ => {
                restore_lexer_state(tokens, state);
                if at_line_start {
                    break;
                }
                Clause::Block(vec![read_clause_expr(tokens)?])
            },
        });
    }
    if past_indent {
        match read_token(tokens)? {
            Token::RIndent => (),
            t => return Err(Error::UnexpectedToken(t)),
        }
    }
    Ok(clauses)
}

fn read_clause_expr(tokens: &mut Tokens) -> Result<Expr> {
    let state = get_lexer_state(tokens);
    match read_token(tokens)? {
        Token::PriKeyword(keyword) => {
            let state_kw = get_lexer_state(tokens);
            let indent_depth = lexer_indent_depth(tokens);
            match read_token(tokens)? {
                Token::LParen => {
                    restore_lexer_state(tokens, state);
                    read_expr(tokens)
                },
                _ => {
                    restore_lexer_state(tokens, state_kw);
                    Ok(Expr(uexpr_from_clauses(&keyword, &read_clause_special_expr_clauses(tokens, indent_depth)?)?, None))
                },
            }
        },
        _ => {
            restore_lexer_state(tokens, state);
            read_expr(tokens)
        },
    }
}

fn read_bracketed_list_r<T>(
    tokens: &mut Tokens,
    is_right: fn(&Token) -> bool,
    read_element: fn(&mut Tokens) -> Result<T>
) -> Result<Vec<T>> {
    let mut v = Vec::new();
    loop {
        let state = get_lexer_state(tokens);
        let t = read_token(tokens)?;
        if is_right(&t) {
            return Ok(v);
        } else {
            restore_lexer_state(tokens, state);
            v.push(read_element(tokens)?);
        }
    }
}

fn read_bracketed_list<T>(
    tokens: &mut Tokens,
    is_left: fn(&Token) -> bool,
    is_right: fn(&Token) -> bool,
    read_element: fn(&mut Tokens) -> Result<T>
) -> Result<Vec<T>> {
    let t = read_token(tokens)?;
    if !is_left(&t) {
        return Err(Error::UnexpectedToken(t));
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

fn read_arg_list_c(tokens: &mut Tokens) -> Result<Vec<Expr>> {
    read_bracketed_list(tokens, |t| *t == Token::LParen, |t| *t == Token::Colon, read_expr)
}

fn read_type_param_list_r(tokens: &mut Tokens) -> Result<Vec<Type>> {
    read_bracketed_list_r(tokens, |t| *t == Token::RBracket, |tokens| expr_to_type(read_expr(tokens)?))
}

fn expr_to_type(Expr(uexpr, type_): Expr) -> Result<Type> {
    if type_ != None {
        return Err(Error::InvalidType(Expr(uexpr, type_)));
    }
    Ok(match uexpr {
        UExpr::Ident(name) => Type::Named(name),
        UExpr::Tuple(type_exprs) => {
            let mut types = Vec::new();
            for type_expr in type_exprs {
                types.push(expr_to_type(type_expr)?);
            }
            Type::Tuple(types)
        },
        UExpr::Function(param_exprs, result_expr) => {
            let mut params = Vec::new();
            for param_expr in param_exprs {
                params.push(expr_to_type(param_expr)?);
            }
            Type::Function(params, Box::new(expr_to_type(*result_expr)?))
        },
        UExpr::Call(base_expr, param_exprs) => {
            let mut params = Vec::new();
            for param_expr in param_exprs {
                params.push(expr_to_type(param_expr)?);
            }
            Type::Applied(Box::new(expr_to_type(*base_expr)?), params)
        },
        _ => return Err(Error::InvalidType(Expr(uexpr, type_))),
    })
}

fn expr_to_pattern(Expr(uexpr, type_): Expr) -> Result<Pattern> {
    if type_ != None {
        return Err(Error::InvalidPattern(Expr(uexpr, type_)));
    }
    Ok(match uexpr.clone() {
        UExpr::IntLiteral(n) => Pattern::IntLiteral(n),
        UExpr::Ident(name) => Pattern::Ident(name.clone()),
        UExpr::Wildcard => Pattern::Wildcard,
        UExpr::Tuple(pattern_exprs) => {
            let mut patterns = Vec::new();
            for pattern_expr in pattern_exprs {
                patterns.push(expr_to_pattern(pattern_expr)?);
            }
            Pattern::Tuple(patterns)
        },
        UExpr::Call(ctor_expr, param_exprs) => {
            let mut params = Vec::new();
            for param_expr in param_exprs {
                params.push(expr_to_pattern(param_expr)?);
            }
            let ctor_name = match *ctor_expr {
                Expr(UExpr::Ident(name), None) => name.to_string(),
                _ => return Err(Error::InvalidPattern(*ctor_expr)),
            };
            Pattern::Variant(ctor_name, params)
        },
        UExpr::List(pattern_blocks) => {
            let mut patterns = Vec::new();
            for pattern_block in &pattern_blocks {
                match &pattern_block[..] {
                    [pattern_expr] => patterns.push(expr_to_pattern(pattern_expr.clone())?),
                    _ => return Err(Error::InvalidPattern(Expr(uexpr, type_))),
                };
            }
            Pattern::List(patterns)
        },
        _ => return Err(Error::InvalidPattern(Expr(uexpr, type_))),
    })
}

fn read_type_signature(tokens: &mut Tokens) -> Result<Option<Type>> {
    let state = get_lexer_state(tokens);
    Ok(match read_token(tokens)? {
        Token::Apostrophe => {
            Some(expr_to_type(read_expr(tokens)?)?)
        },
        _ => {
            restore_lexer_state(tokens, state);
            None
        },
    })
}
