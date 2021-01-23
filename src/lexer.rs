use crate::error::*;
use unic_ucd_category::GeneralCategory;
use unic_ucd_common::is_white_space;

#[derive(Debug, PartialEq)]
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
}

const AFTER_IDENT_CHARS: [char; 8] = [
    '(', ')', '[', ']', '{', '}', '\'', ':',
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

pub struct Tokens {
    chars: Vec<char>,
    indents: Vec<u64>,
    buffer: Vec<Token>,
}

impl Tokens {
    pub fn new(chars: Vec<char>) -> Tokens {
        Tokens {
            chars,
            indents: Vec::new(),
            buffer: Vec::new(),
        }
    }
}

fn read_token_(tokens: &mut Tokens, or_indent: bool) -> Result<Option<Token>> {
    use Token::*;
    if let Some(t) = tokens.buffer.pop() {
        return Ok(Some(t));
    }
    let mut c = tokens.chars.pop();
    while let Some(c0) = c {
        if !is_white_space(c0) {
            break;
        }
        c = tokens.chars.pop();
    }
    let c0 = match c {
        Some(c0) => c0,
        None => return Ok(None),
    };
    Ok(Some(match c0 {
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
                c = tokens.chars.pop();
            }
            if let Some(c0) = c {
                tokens.chars.push(c0);
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
                c = tokens.chars.pop();
            }
            if c == Some('.') {
                PriKeyword(s)
            } else if c == Some(',') {
                SecKeyword(s)
            } else {
                if let Some(c0) = c {
                    tokens.chars.push(c0);
                    if !(is_white_space(c0) || AFTER_IDENT_CHARS.contains(&c0)) {
                        return Err(Error::UnexpectedChar(c));
                    }
                }
                Ident(s)
            }
        },
        _ => return Err(Error::UnexpectedChar(c)),
    }))
}

pub fn read_token_or_eof(tokens: &mut Tokens) -> Result<Option<Token>> {
    read_token_(tokens, false)
}

pub fn read_token_or_indent_or_eof(tokens: &mut Tokens) -> Result<Option<Token>> {
    read_token_(tokens, true)
}

pub fn read_token(tokens: &mut Tokens) -> Result<Token> {
    read_token_(tokens, false)?.ok_or(Error::UnexpectedToken(None))
}

pub fn read_token_or_indent(tokens: &mut Tokens) -> Result<Token> {
    read_token_(tokens, true)?.ok_or(Error::UnexpectedToken(None))
}

pub fn return_token(tokens: &mut Tokens, token: Token) {
    tokens.buffer.push(token)
}
