use crate::error::*;
use unic_ucd_category::GeneralCategory;
use unic_ucd_common::is_white_space;

#[derive(Debug, PartialEq)]
pub enum Token {
    IntLiteral(i64),
    Ident(String),
    Let,
    Set,
    Match,
    While,
    Do,
    Lambda,
    LParen,
    RParen,
    LBracket,
    RBracket,
    LBrace,
    RBrace,
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

fn read_token(chars: &mut Vec<char>) -> Result<Option<Token>> {
    use Token::*;
    let mut c = chars.pop();
    while let Some(c0) = c {
        if !is_white_space(c0) {
            break;
        }
        c = chars.pop();
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
                c = chars.pop();
            }
            if let Some(c0) = c {
                chars.push(c0);
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
                c = chars.pop();
            }
            if c == Some('.') {
                match &s[..] {
                    "let" => Let,
                    "set" => Set,
                    "match" => Match,
                    "while" => While,
                    "do" => Do,
                    "λ" => Lambda,
                    _ => return Err(Error::InvalidKeyword(s)),
                }
            } else {
                if let Some(c0) = c {
                    chars.push(c0);
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

pub fn read_file_tokens(mut chars: Vec<char>) -> Result<Vec<Token>> {
    let mut v = Vec::new();
    while let Some(t) = read_token(&mut chars)? {
        v.push(t);
    }
    v.reverse();
    Ok(v)
}
