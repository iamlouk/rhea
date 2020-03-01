use std::collections::HashMap;

#[derive(Clone, Debug)]
pub enum Error {
    LexerErr(String),
    Unspecified(&'static str),
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Error::LexerErr(str) => write!(f, "The Lexer failed: {}", str),
            Error::Unspecified(str) => write!(f, "unspecified error: {}", str)
        }
    }
}

#[macro_export]
macro_rules! lexer_err {
    ($x:expr, $($y:expr),*) => {{
        use std::fmt::Write;
        use crate::utils::Error;

        let mut reason = String::new();
        write!(&mut reason, $x, $($y),*).unwrap();

        Err(Error::LexerErr(reason))
    }};
}

#[derive(Debug)]
pub struct Env<'input, T: Clone> {
    scopes: Vec<HashMap<&'input str, T>>
}

impl<'input, T: Clone> Env<'input, T> {
    pub fn new() -> Self {
        let mut env = Env { scopes: vec![] };
        env.push_scope();
        env
    }

    pub fn push_scope(&mut self) { self.scopes.push(HashMap::new()); }

    pub fn pop_scope(&mut self) { self.scopes.pop(); }

    pub fn define(&mut self, name: &'input str, value: T) -> Result<(), Error> {
        if self.scopes.last_mut().unwrap().insert(name, value).is_some() {
            panic!()
        } else {
            Ok(())
        }
    }

    pub fn lookup(&self, name: &'input str) -> Result<T, Error> {
        for scope in self.scopes.iter().rev() {
            match scope.get(name) {
                Some(value) => return Ok(value.clone()),
                None => continue
            }
        }
        panic!(name.to_owned())
    }
}

pub fn unescape_parsed_string(raw: &str) -> Result<String, Error> {
    let mut string = String::with_capacity(raw.len());
    let mut chars = raw.chars();

    while let Some(c) = chars.next() {
        if c == '\\' {
            match chars.next() {
                Some('n') => string.push('\n'),
                Some('t') => string.push('\t'),
                Some('\\') => string.push('\\'),
                Some(_) | None => return Err(Error::Unspecified("illegal string literal"))
            }
        } else {
            string.push(c);
        }
    }

    Ok(string)
}
