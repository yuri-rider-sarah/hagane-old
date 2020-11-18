use crate::error::{Error, Result};
use crate::file::File;
use unic_ucd_category::GeneralCategory;
use unic_ucd_common::is_white_space;

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
    Period,
    Colon,
    Hash,
    Eof,
}

const after_ident_chars: [char; 8] = [
    '(', ')', '[', ']', '{', '}', '.', ':',
];

const permitted_punctuation: [char; 19] = [
    '*', '/', '\\',
    '′', '″', '‴', '‵', '‶', '‷', '⁂', '⁗', '⁎', '⁑', '⁕',
    '﹡', '﹨', '＊', '／', '＼',
];

fn is_ident_character(c: char) -> bool {
    use GeneralCategory::*;
    match GeneralCategory::of(c) {
        UppercaseLetter | LowercaseLetter | TitlecaseLetter | ModifierLetter | OtherLetter => true,
        NonspacingMark | SpacingMark | EnclosingMark => true,
        DecimalNumber | LetterNumber | OtherNumber => true,
        ConnectorPunctuation | DashPunctuation => true,
        OpenPunctuation | ClosePunctuation | InitialPunctuation | FinalPunctuation => false,
        OtherPunctuation => permitted_punctuation.contains(&c),
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

fn read_token(file: &mut File<char>) -> Result<Option<Token>> {
    use Token::*;
    let mut c = file.read().map(|x| *x);
    while let Some(c0) = c {
        if !is_white_space(c0) {
            break;
        }
        c = file.read().map(|x| *x);
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
        '.' => Period,
        ':' => Colon,
        '#' => Hash,
        '0'..='9' => {
            let mut n = 0;
            while let Some(c0) = c {
                if !('0'..='9').contains(&c0) {
                    break;
                }
                n = 10 * n + (c0 as i64 - '0' as i64);
                c = file.read().map(|x| *x);
            }
            file.step_back();
            if c.map_or(false, |c| !(is_white_space(c) || after_ident_chars.contains(&c))) {
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
                c = file.read().map(|x| *x);
            }
            file.step_back();
            if c.map_or(false, |c| !(is_white_space(c) || after_ident_chars.contains(&c))) {
                return Err(Error::UnexpectedChar(c));
            }
            match &s[..] {
                "let" => Let,
                "set" => Set,
                "match" => Match,
                "while" => While,
                "do" => Do,
                "λ" => Lambda,
                _ => Ident(s),
            }
        },
        _ => return Err(Error::UnexpectedChar(c)),
    }))
}

pub fn read_file_tokens(mut file: File<char>) -> Result<File<Token>> {
    let mut v = Vec::new();
    while let Some(t) = read_token(&mut file)? {
        v.push(t);
    }
    Ok(File::new(v.into_boxed_slice()))
}
