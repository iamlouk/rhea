use crate::utils;

pub type Spanned<Tok, Loc, Error> = Result<(Loc, Tok, Loc), Error>;

use std::str::Chars;
use std::iter::Peekable;

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Tok<'input> {
    Id(&'input str),
    Str(&'input str),
    Int(i64),
    Real(f64),
    Bool(bool),

    LeftBracket, RightBracket,
    LeftSquareBracket, RightSquareBracket,
    LeftCurlyBracket, RightCurlyBracket,

    Assign, AssignNew, Dot, DotDotDot,
    Comma, Colon, Arrow, Semicolon,
    Add, Sub, Mul, Div,
    And, Or, Equal, Smaller,

    If, Else, Then, For, Extern, Mut, As, Struct, Type
}

pub struct Lexer<'input> {
    chars: Peekable<Chars<'input>>,
    input: &'input str,
    loc: usize,
    line: usize
}

impl<'input> Lexer<'input> {
    pub fn new(input: &'input str) -> Self {
        Lexer {
            chars: input.chars().peekable(),
            input,
            loc: 0,
            line: 1
        }
    }

    fn skip_whitespace(&mut self) -> Option<char> {
        let c;
        loop {
            self.loc += 1;
            c = match self.chars.next() {
                Some(' ') => continue,
                Some('\t') => continue,
                Some('\n') => continue,
                Some('#') => {
                    if self.chars.peek() == Some(&'*') {
                        self.chars.next();
                        self.loc += 1;
                        while let Some(c) = self.chars.next() {
                            self.loc += 1;
                            if c == '*' && self.chars.peek() == Some(&'#') {
                                self.loc += 1;
                                self.chars.next();
                                break;
                            }
                            if c == '\n' { self.line += 1; }
                        }
                    } else {
                        while let Some(c) = self.chars.next() {
                            self.loc += 1;
                            if c == '\n' { break; }
                        }
                        self.line += 1;
                    }
                    continue;
                },
                Some(c) => c,
                None => return None
            };

            break;
        }
        Some(c)
    }
}

impl<'input> Iterator for Lexer<'input> {
    type Item = Spanned<Tok<'input>, usize, utils::Error>;

    fn next(&mut self) -> Option<Self::Item> {
        let c = match self.skip_whitespace() {
            Some(c) => c,
            None => return None
        };
        match c {
            '(' => Some(Ok((self.loc - 1, Tok::LeftBracket,        self.loc))),
            ')' => Some(Ok((self.loc - 1, Tok::RightBracket,       self.loc))),
            '[' => Some(Ok((self.loc - 1, Tok::LeftSquareBracket,  self.loc))),
            ']' => Some(Ok((self.loc - 1, Tok::RightSquareBracket, self.loc))),
            '{' => Some(Ok((self.loc - 1, Tok::LeftCurlyBracket,   self.loc))),
            '}' => Some(Ok((self.loc - 1, Tok::RightCurlyBracket,  self.loc))),

            '+' => Some(Ok((self.loc - 1, Tok::Add,       self.loc))),
            '*' => Some(Ok((self.loc - 1, Tok::Mul,       self.loc))),
            '/' => Some(Ok((self.loc - 1, Tok::Div,       self.loc))),
            '|' => Some(Ok((self.loc - 1, Tok::Or,        self.loc))),
            '&' => Some(Ok((self.loc - 1, Tok::And,       self.loc))),
            '<' => Some(Ok((self.loc - 1, Tok::Smaller,   self.loc))),
            ',' => Some(Ok((self.loc - 1, Tok::Comma,     self.loc))),
            ';' => Some(Ok((self.loc - 1, Tok::Semicolon, self.loc))),

            ':' => {
                if self.chars.peek() == Some(&'=') {
                    self.chars.next();
                    self.loc += 1;
                    Some(Ok((self.loc - 2, Tok::AssignNew, self.loc)))
                } else {
                    Some(Ok((self.loc - 1, Tok::Colon, self.loc)))
                }
            },
            '-' => {
                if self.chars.peek() == Some(&'>') {
                    self.chars.next();
                    self.loc += 1;
                    Some(Ok((self.loc - 2, Tok::Arrow, self.loc)))
                } else {
                    Some(Ok((self.loc - 1, Tok::Sub, self.loc)))
                }
            },
            '=' => {
                if self.chars.peek() == Some(&'=') {
                    self.chars.next();
                    self.loc += 1;
                    Some(Ok((self.loc - 2, Tok::Equal, self.loc)))
                } else {
                    Some(Ok((self.loc - 1, Tok::Assign, self.loc)))
                }
            },

            '.' => {
                if self.chars.peek() == Some(&'.') {
                    self.chars.next();
                    self.loc += 1;
                    if self.chars.peek() == Some(&'.') {
                        self.chars.next();
                        self.loc += 1;
                        Some(Ok((self.loc - 2, Tok::DotDotDot, self.loc)))
                    } else {
                        panic!()
                    }
                } else {
                    Some(Ok((self.loc - 1, Tok::Dot, self.loc)))
                }
            },

            'A'..='Z' | 'a'..='z' | '_' => {
                let start = self.loc - 1;
                while let Some(c) = self.chars.peek().copied() {
                    match c {
                        'A'..='Z' | 'a'..='z' | '_' | '0'..='9' => {},
                        _ => { break; }
                    }
                    self.loc += 1;
                    self.chars.next();
                }

                let id = &self.input[start..self.loc];
                match id {
                    "extern" => Some(Ok((start, Tok::Extern,      self.loc))),
                    "if"     => Some(Ok((start, Tok::If,          self.loc))),
                    "else"   => Some(Ok((start, Tok::Else,        self.loc))),
                    "then"   => Some(Ok((start, Tok::Then,        self.loc))),
                    "for"    => Some(Ok((start, Tok::For,         self.loc))),
                    "true"   => Some(Ok((start, Tok::Bool(true),  self.loc))),
                    "false"  => Some(Ok((start, Tok::Bool(false), self.loc))),
                    "mut"    => Some(Ok((start, Tok::Mut,         self.loc))),
                    "as"     => Some(Ok((start, Tok::As,          self.loc))),
                    "struct" => Some(Ok((start, Tok::Struct,      self.loc))),
                    "type"   => Some(Ok((start, Tok::Type,        self.loc))),
                    _        => Some(Ok((start, Tok::Id(id),      self.loc)))
                }
            },

            '0'..='9' => {
                let start = self.loc - 1;
                let mut is_real = false;
                while let Some(c) = self.chars.peek().copied() {
                    match c {
                        '0'..='9' => { self.loc += 1; },
                        '.' if !is_real => {
                            is_real = true;
                            self.loc += 1;
                        },
                        _ => { break; }
                    }
                    self.chars.next();
                }

                let num = &self.input[start..self.loc];
                if is_real {
                    match num.parse::<f64>() {
                        Ok(num) => Some(Ok((start, Tok::Real(num), self.loc))),
                        Err(e)  => Some(lexer_err!("parsing {:?} as f64 failed: {}", num, e))
                    }
                } else {
                    match num.parse::<i64>() {
                        Ok(num) => Some(Ok((start, Tok::Int(num), self.loc))),
                        Err(e)  => Some(lexer_err!("parsing {:?} as i64 failed: {}", num, e))
                    }
                }
            },

            '"' | '\'' => {
                let endstringchar = c;
                let start = self.loc;
                while let Some(c) = self.chars.next() {
                    self.loc += 1;
                    if c == endstringchar {
                        let tok = Tok::Str(&self.input[start..(self.loc - 1)]);
                        return Some(Ok((start - 1, tok, self.loc)));
                    } else if c == '\\' {
                        self.loc += 1;
                        match self.chars.next() {
                            Some(c) if c == endstringchar => {
                                self.chars.next();
                                self.loc += 1;
                            },
                            Some('n') | Some('t') => {},
                            _ => panic!()
                        }
                    }
                }
                panic!()
            },

            _ => panic!()
        }
    }

}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn tokens() {
        let input = "( hallo 3.14 +* \"abc \\\" xyz\" / welt)
            123 'test' #blablbalba\n: := ->'hallo'
            #* bla bla bla *# ... end".to_owned();
        let mut lexer = Lexer::new(input.as_ref());
        let mut next_tok = || lexer.next().unwrap().ok().unwrap().1;

        assert_eq!(next_tok(), Tok::LeftBracket);
        assert_eq!(next_tok(), Tok::Id("hallo"));
        assert_eq!(next_tok(), Tok::Real(3.14));
        assert_eq!(next_tok(), Tok::Add);
        assert_eq!(next_tok(), Tok::Mul);
        assert_eq!(next_tok(), Tok::Str("abc \\\" xyz"));
        assert_eq!(next_tok(), Tok::Div);
        assert_eq!(next_tok(), Tok::Id("welt"));
        assert_eq!(next_tok(), Tok::RightBracket);
        assert_eq!(next_tok(), Tok::Int(123));
        assert_eq!(next_tok(), Tok::Str("test"));
        assert_eq!(next_tok(), Tok::Colon);
        assert_eq!(next_tok(), Tok::AssignNew);
        assert_eq!(next_tok(), Tok::Arrow);
        assert_eq!(next_tok(), Tok::Str("hallo"));
        assert_eq!(next_tok(), Tok::DotDotDot);
        assert_eq!(next_tok(), Tok::Id("end"));
        assert!(lexer.next().is_none());
    }
}
