use std::collections::HashMap;

#[derive(Clone, Debug)]
pub enum Error {
    LLVMError(String)
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Error::LLVMError(str) => write!(f, "LLVM-Error: {}", str)
        }
    }
}

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

    pub fn lookup(&mut self, name: &'input str) -> Result<T, Error> {
        for scope in self.scopes.iter().rev() {
            match scope.get(name) {
                Some(value) => return Ok(value.clone()),
                None => continue
            }
        }
        panic!(name.to_owned())
    }
}
