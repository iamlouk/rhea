use std::collections::HashMap;

#[derive(Clone, Debug)]
pub enum Error {
    LLVMError(String),
    Unspecified(&'static str)
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Error::LLVMError(str) => write!(f, "LLVM-Error: {}", str),
            Error::Unspecified(str) => write!(f, "unspecified error: {}", str)
        }
    }
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

    pub fn take_bottom_scope(&mut self) -> Option<HashMap<&'input str, T>> {
        if self.scopes.len() == 0 {
            None
        } else {
            Some(self.scopes.remove(0))
        }
    }

    pub fn add_bottom_scope(&mut self, scope: HashMap<&'input str, T>) {
        self.scopes.insert(0, scope);
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

