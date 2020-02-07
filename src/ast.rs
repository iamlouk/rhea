use std::sync::Arc;

use crate::utils;

#[derive(Debug)]
pub struct Program<'input> {
    pub defs: Vec<Definition<'input>>
}

impl<'input> Program<'input> {
    pub fn check_type(&mut self, vartypes: &mut utils::Env<'input,
            Arc<Type<'input>>>, deftypes: &mut utils::Env<'input, Arc<Type<'input>>>)
            -> Result<(), utils::Error> {

        for definition in &self.defs {
            match definition {
                Definition::Extern(name, typ) => vartypes.define(name, typ.clone())?,
                Definition::Function(f) => vartypes.define(f.name.unwrap(), Arc::new(f.get_type()))?
            }
        }

        for definition in &mut self.defs {
            match definition {
                Definition::Function(f) => {
                    f.check_type(vartypes, deftypes)?;
                },
                Definition::Extern(_, _) => {}
            }
        }

        Ok(())
    }
}

#[derive(Debug)]
pub enum Definition<'input> {
    Function(Function<'input>),
    Extern(&'input str, Arc<Type<'input>>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type<'input> {
    Untyped, Void, Int,
    Real, Bool, Char,
    Struct(Vec<(&'input str, Arc<Type<'input>>)>),
    Tuple(Vec<Arc<Type<'input>>>),
    Ptr(Arc<Type<'input>>), Unresolved(&'input str),
    Func(Vec<Arc<Type<'input>>>, Arc<Type<'input>>, bool)
}

#[derive(Debug, Clone)]
pub struct Function<'input> {
    pub name: Option<&'input str>,
    pub args: Vec<(&'input str, Arc<Type<'input>>)>,
    pub rettype: Arc<Type<'input>>,
    pub body: Box<Node<'input, Expr<'input>>>,
    pub is_vararg: bool
}

impl<'input> Function<'input> {
    pub fn get_type(&self) -> Type<'input> {
        Type::Func(self.args.iter().map(|arg| arg.1.clone()).collect(), self.rettype.clone(), self.is_vararg)
    }

    pub fn check_type(&mut self, vartypes: &mut utils::Env<'input,
            Arc<Type<'input>>>, deftypes: &mut utils::Env<'input, Arc<Type<'input>>>)
            -> Result<(), utils::Error> {

        vartypes.push_scope();
        deftypes.push_scope();

        for arg in &self.args {
            vartypes.define(arg.0, arg.1.clone())?;
        }

        let typ = self.body.check_type(vartypes, deftypes)?;
        if typ != self.rettype {
            panic!();
        }

        vartypes.pop_scope();
        deftypes.pop_scope();
        Ok(())
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Operator {
    Add, Sub, Mul, Div,
    And, Or,
    Equal, Smaller
}

#[derive(Debug, Clone)]
pub enum Expr<'input> {
    Identifier(&'input str),
    StringLit(&'input str),
    IntLit(i64),
    RealLit(f64),
    BoolLit(bool),
    BinaryExpr(Box<Node<'input, Expr<'input>>>, Operator, Box<Node<'input, Expr<'input>>>),
    Call(Box<Node<'input, Expr<'input>>>, Vec<Node<'input, Expr<'input>>>),
    If(Box<Node<'input, Expr<'input>>>,
       Box<Node<'input, Expr<'input>>>, Option<Box<Node<'input, Expr<'input>>>>),
    For(Option<Box<Node<'input, Expr<'input>>>>,
        Box<Node<'input, Expr<'input>>>,
        Option<Box<Node<'input, Expr<'input>>>>,
        Box<Node<'input, Expr<'input>>>),
    Block(Vec<Node<'input, Expr<'input>>>)
}

#[derive(Clone)]
pub struct Node<'input, T> {
    pub node: T,
    pub typ: Arc<Type<'input>>
}

lazy_static! {
    static ref UNTYPED: Arc<Type<'static>> = Arc::new(Type::Untyped);
    static ref CHAR_PTR: Arc<Type<'static>> = Arc::new(Type::Ptr(Arc::new(Type::Char)));
    static ref BOOL: Arc<Type<'static>> = Arc::new(Type::Bool);
    static ref INT: Arc<Type<'static>> = Arc::new(Type::Int);
    static ref REAL: Arc<Type<'static>> = Arc::new(Type::Real);
    static ref VOID: Arc<Type<'static>> = Arc::new(Type::Void);
}

impl<'input, T> Node<'input, T> {
    pub fn new(node: T) -> Box<Node<'input, T>> {
        Box::new(Node { node, typ: UNTYPED.clone() })
    }
}

impl<'input> Node<'input, Expr<'input>> {
    pub fn check_type(&mut self, vartypes: &mut utils::Env<'input, Arc<Type<'input>>>,
            deftypes: &mut utils::Env<'input, Arc<Type<'input>>>)
            -> Result<Arc<Type<'input>>, utils::Error> {
        let typ = match &mut self.node {
            Expr::Identifier(id) => vartypes.lookup(id)?,
            Expr::StringLit(_) => CHAR_PTR.clone(),
            Expr::IntLit(_) => INT.clone(),
            Expr::RealLit(_) => REAL.clone(),
            Expr::BoolLit(_) => BOOL.clone(),
            Expr::BinaryExpr(ref mut lhs, op, ref mut rhs) => {
                let lhs = lhs.check_type(vartypes, deftypes)?;
                let rhs = rhs.check_type(vartypes, deftypes)?;
                match (lhs.as_ref(), op, rhs.as_ref()) {
                    (&Type::Int, Operator::Add, &Type::Int) => lhs,
                    (&Type::Int, Operator::Sub, &Type::Int) => lhs,

                    (&Type::Int, Operator::Equal, &Type::Int) => BOOL.clone(),
                    (&Type::Int, Operator::Smaller, &Type::Int) => BOOL.clone(),

                    (&Type::Bool, Operator::Or, &Type::Bool) => lhs,
                    (&Type::Bool, Operator::And, &Type::Bool) => lhs,

                    (a, op, b) => {
                        println!("{:?} {:?} {:?}", a, op, b);
                        panic!();
                    }
                }
            },
            Expr::Call(ref mut callee, ref mut args) => {
                let callee = callee.check_type(vartypes, deftypes)?;
                let (argtypes, rettype, is_vararg) = match *callee.as_ref() {
                    Type::Func(ref argtypes, ref rettype, is_vararg) => (argtypes, rettype, is_vararg),
                    _ => panic!()
                };

                if !is_vararg && args.len() != argtypes.len() {
                    panic!();
                }

                if is_vararg && args.len() < argtypes.len() {
                    panic!();
                }

                let mut argiter = args.iter_mut();
                for (typ, arg) in argtypes.iter().zip(&mut argiter) {
                    let argtyp = arg.check_type(vartypes, deftypes)?;
                    if argtyp.as_ref() != typ.as_ref() {
                        panic!();
                    }
                }

                for arg in argiter {
                    arg.check_type(vartypes, deftypes)?;
                }

                rettype.clone()
            },
            Expr::Block(ref mut stmts) => {
                let mut rettype = VOID.clone();
                vartypes.push_scope();
                for stmt in stmts {
                    rettype = stmt.check_type(vartypes, deftypes)?;
                }
                vartypes.pop_scope();
                rettype
            },
            Expr::If(cond, thenblock, elseblock) => {
                let condtype = cond.check_type(vartypes, deftypes)?;
                if condtype.as_ref() != &Type::Bool {
                    panic!();
                }

                let thenblocktype = thenblock.check_type(vartypes, deftypes)?;
                match elseblock {
                    Some(block) => {
                        let elseblocktype = block.check_type(vartypes, deftypes)?;
                        if thenblocktype != elseblocktype {
                            panic!();
                        }
                    },
                    None => {}
                }
                thenblocktype.clone()
            },
            Expr::For(init, cond, inc, body) => {
                vartypes.push_scope();
                if let Some(init) = init {
                    init.check_type(vartypes, deftypes)?;
                }
                let condtype = cond.check_type(vartypes, deftypes)?;
                if condtype.as_ref() != &Type::Bool {
                    panic!();
                }
                body.check_type(vartypes, deftypes)?;
                if let Some(inc) = inc {
                    inc.check_type(vartypes, deftypes)?;
                }
                vartypes.pop_scope();
                VOID.clone()
            }
        };

        self.typ = typ.clone();
        Ok(typ)
    }
}

impl<'input, T: std::fmt::Debug> std::fmt::Debug for Node<'input, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.node)
    }
}
