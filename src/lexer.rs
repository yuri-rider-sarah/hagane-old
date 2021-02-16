use crate::error::*;
use unic_ucd_category::GeneralCategory;
use unic_ucd_common::is_white_space;

#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    IntLiteral(i64),
    Ident(String),
    PriKeyword(String),
    SecKeyword(String),
    LParen,
    RParen,
    LBracket,
    RBracket,
    LBrace,
    RBrace,
    LIndent,
    RIndent,
    Apostrophe,
    Colon,
    Hash,
    Eof,
}

const NEWLINE_CHARS: [char; 7] = [
    '\n', '\x0B', '\x0C', '\r', '\u{0085}', '\u{2028}', '\u{2029}',
];

const AFTER_IDENT_CHARS: [char; 9] = [
    '(', ')', '[', ']', '{', '}', '\'', ':', '※',
];

const PERMITTED_PUNCTUATION: [char; 30] = [
    '%', '&', '*', '/', '\\',
    '؉', '؊', '٪', '‰', '‱',
    '′', '″', '‴', '‵', '‶', '‷',
    '⁂', '⁗', '⁎', '⁑', '⁕',
    '﹠', '﹡', '﹨', '﹪',
    '％', '＆', '＊', '／', '＼',
];

fn is_ident_character(c: char) -> bool {
    use GeneralCategory::*;
    match GeneralCategory::of(c) {
        UppercaseLetter | LowercaseLetter | TitlecaseLetter | ModifierLetter | OtherLetter => true,
        NonspacingMark | SpacingMark | EnclosingMark => true,
        DecimalNumber | LetterNumber | OtherNumber => true,
        ConnectorPunctuation | DashPunctuation => true,
        OpenPunctuation | ClosePunctuation | InitialPunctuation | FinalPunctuation => false,
        OtherPunctuation => PERMITTED_PUNCTUATION.contains(&c),
        MathSymbol | CurrencySymbol | ModifierSymbol | OtherSymbol => true,
        SpaceSeparator | LineSeparator | ParagraphSeparator => false,
        Control | Format | Surrogate | PrivateUse | Unassigned => false,
    }
}

fn is_starting_ident_character(c: char) -> bool {
    use GeneralCategory::*;
    match GeneralCategory::of(c) {
        NonspacingMark | SpacingMark | EnclosingMark => false,
        DecimalNumber | LetterNumber | OtherNumber => false,
        _ => is_ident_character(c),
    }
}

#[derive(Clone, Debug)]
pub struct LexerState {
    pos: usize,
    indents: Vec<usize>,
    indent_char: Option<char>,
    rindents: usize,
    this_indent_depth: usize,
    at_line_start: bool,
}

pub struct Tokens {
    chars: Vec<char>,
    state: LexerState,
}

impl Tokens {
    pub fn new(chars: Vec<char>) -> Result<Tokens> {
        let mut tokens = Tokens {
            chars,
            state: LexerState {
                pos: 0,
                indents: Vec::new(),
                indent_char: None,
                rindents: 0,
                this_indent_depth: 0,
                at_line_start: true,
            },
        };
        read_indent(&tokens.chars, &mut tokens.state)?;
        Ok(tokens)
    }
}

fn read_char(chars: &Vec<char>, state: &mut LexerState) -> Option<char> {
    state.pos += 1;
    if state.pos - 1 < chars.len() {
        Some(chars[state.pos - 1])
    } else {
        None
    }
}

fn skip_comment(chars: &Vec<char>, state: &mut LexerState) {
    while let Some(c0) = read_char(chars, state) {
        if NEWLINE_CHARS.contains(&c0) {
            break;
        }
    }
}

fn read_indent(chars: &Vec<char>, state: &mut LexerState) -> Result<usize> {
    let mut indent_depth = 0;
    while let Some(c0) = read_char(chars, state) {
        if c0 == '※' {
            skip_comment(chars, state);
            indent_depth = 0;
        } else if NEWLINE_CHARS.contains(&c0) {
            indent_depth = 0;
        } else if is_white_space(c0) {
            match state.indent_char {
                None => {
                    state.indent_char = Some(c0);
                    indent_depth += 1;
                },
                Some(indent_char) => if c0 == indent_char {
                    indent_depth += 1;
                } else {
                    return Err(Error::InconsistentIndentation(indent_char, c0));
                },
            }
        } else {
            break;
        }
    }
    state.pos -= 1;
    state.this_indent_depth = indent_depth;
    Ok(indent_depth)
}

fn read_token_(chars: &Vec<char>, state: &mut LexerState, or_indent: Option<usize>) -> Result<Token> {
    use Token::*;
    if state.rindents > 0 {
        state.rindents -= 1;
        return Ok(RIndent);
    }
    let mut c = read_char(chars, state);
    while let Some(c0) = c {
        if c0 == '※' || NEWLINE_CHARS.contains(&c0) {
            state.at_line_start = true;
            if c0 == '※' {
                skip_comment(chars, state);
            }
            let indent_depth = read_indent(chars, state)?;
            c = read_char(chars, state);
            if c == None {
                state.rindents = state.indents.len();
                if state.rindents > 0 {
                    state.rindents -= 1;
                    return Ok(RIndent);
                } else {
                    return Ok(Eof);
                }
            } else {
                if let Some(last_indent_depth) = or_indent {
                    if *state.indents.last().unwrap_or(&0) < indent_depth && last_indent_depth < indent_depth {
                        state.pos -= 1;
                        state.indents.push(indent_depth);
                        return Ok(LIndent);
                    }
                }
                while let Some(&outer_indent_depth) = state.indents.last() {
                    if indent_depth < outer_indent_depth {
                        state.rindents += 1;
                        state.indents.pop();
                    } else {
                        break;
                    }
                }
                if state.rindents > 0 {
                    state.pos -= 1;
                    state.rindents -= 1;
                    return Ok(RIndent);
                }
                break;
            }
        } else if !is_white_space(c0) {
            break;
        }
        c = read_char(chars, state);
    }
    state.at_line_start = false;
    let c0 = match c {
        Some(c0) => c0,
        None => return Ok(Eof),
    };
    Ok(match c0 {
        '(' => LParen,
        ')' => RParen,
        '[' => LBracket,
        ']' => RBracket,
        '{' => LBrace,
        '}' => RBrace,
        '\'' => Apostrophe,
        ':' => Colon,
        '#' => Hash,
        '0'..='9' => {
            let mut n = 0;
            while let Some(c0) = c {
                if !('0'..='9').contains(&c0) {
                    break;
                }
                n = 10 * n + (c0 as i64 - '0' as i64);
                c = read_char(chars, state);
            }
            if let Some(_) = c {
                state.pos -= 1;
            }
            if c.map_or(false, |c| !(is_white_space(c) || AFTER_IDENT_CHARS.contains(&c))) {
                return Err(Error::UnexpectedChar(c));
            }
            IntLiteral(n)
        },
        _ if is_starting_ident_character(c0) => {
            let mut s = String::new();
            while let Some(c0) = c {
                if !is_ident_character(c0) {
                    break;
                }
                s.push(c0);
                c = read_char(chars, state);
            }
            if c == Some('.') {
                PriKeyword(s)
            } else if c == Some(',') {
                SecKeyword(s)
            } else {
                if let Some(c0) = c {
                    state.pos -= 1;
                    if !(is_white_space(c0) || AFTER_IDENT_CHARS.contains(&c0)) {
                        return Err(Error::UnexpectedChar(c));
                    }
                }
                Ident(s)
            }
        },
        _ => return Err(Error::UnexpectedChar(c)),
    })
}

pub fn read_token(tokens: &mut Tokens) -> Result<Token> {
    read_token_(&tokens.chars, &mut tokens.state, None)
}

pub fn read_token_or_indent(tokens: &mut Tokens, last_indent_depth: usize) -> Result<Token> {
    read_token_(&tokens.chars, &mut tokens.state, Some(last_indent_depth))
}

pub fn get_lexer_state(tokens: &Tokens) -> LexerState {
    tokens.state.clone()
}

pub fn restore_lexer_state(tokens: &mut Tokens, state: LexerState) {
    tokens.state = state
}

pub fn lexer_indent_depth(tokens: &Tokens) -> usize {
    tokens.state.this_indent_depth
}

pub fn lexer_at_line_start(tokens: &Tokens) -> bool {
    if tokens.state.at_line_start {
        return true;
    }
    let mut state = get_lexer_state(tokens);
    while let Some(c) = read_char(&tokens.chars, &mut state) {
        if c == '※' || NEWLINE_CHARS.contains(&c) {
            return true;
        } else if !is_white_space(c) {
            return false;
        }
    }
    return true;
}
