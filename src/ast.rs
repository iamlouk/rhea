use std::sync::Arc;

use crate::utils;

#[derive(Debug)]
pub struct Program<'input> {
    pub defs: Vec<Definition<'input>>
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct VarProp {
    pub is_mutable: bool,
    pub is_extern: bool
}

impl VarProp {
    pub fn default() -> Self { VarProp { is_mutable: false, is_extern: false } }
    pub fn mutable() -> Self { VarProp { is_mutable: true, is_extern: false } }
}

impl<'input> Program<'input> {
    pub fn check_type(&mut self, vartypes: &mut utils::Env<'input, (Type<'input>, VarProp)>,
            deftypes: &mut utils::Env<'input, Type<'input>>)
            -> Result<(), utils::Error> {

        for definition in &self.defs {
            match definition {
                Definition::Extern(name, typ) =>
                    vartypes.define(name, (typ.clone(), VarProp {
                        is_mutable: false,
                        is_extern: true
                    }))?,
                Definition::Function(f) =>
                    vartypes.define(f.name.unwrap(), (f.get_type(), VarProp {
                        is_mutable: false,
                        is_extern: false
                    }))?
            }
        }

        for definition in &mut self.defs {
            match definition {
                Definition::Function(f) => { f.check_type(vartypes, deftypes)?; },
                Definition::Extern(_, _) => {}
            }
        }

        Ok(())
    }
}

#[derive(Debug)]
pub enum Definition<'input> {
    Function(Function<'input>),
    Extern(&'input str, Type<'input>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type<'input> {
    Untyped, Void, Int,
    Real, Bool, Char,
    Struct(Arc<Vec<(&'input str, Type<'input>)>>),
    Tuple(Arc<Vec<Type<'input>>>), Ptr(Arc<Type<'input>>),
    Unresolved(&'input str), Named(&'input str, Arc<Type<'input>>),
    Func(Arc<Vec<Type<'input>>>, Arc<Type<'input>>, bool)
}

lazy_static! {
    static ref CHAR_TYPE: Arc<Type<'static>> = Arc::new(Type::Char);
}

#[derive(Debug, Clone)]
pub struct Function<'input> {
    pub name: Option<&'input str>,
    pub args: Vec<(&'input str, Type<'input>)>,
    pub rettype: Type<'input>,
    pub body: Box<Node<'input, Expr<'input>>>,
    pub is_vararg: bool
}

impl<'input> Function<'input> {
    pub fn get_type(&self) -> Type<'input> {
        Type::Func(Arc::new(self.args.iter().map(|arg| arg.1.clone())
            .collect()), Arc::new(self.rettype.clone()), self.is_vararg)
    }

    pub fn check_type(&mut self, vartypes: &mut utils::Env<'input, (Type<'input>, VarProp)>,
            deftypes: &mut utils::Env<'input, Type<'input>>)
            -> Result<(), utils::Error> {

        vartypes.push_scope();
        deftypes.push_scope();

        for arg in &self.args {
            vartypes.define(arg.0, (arg.1.clone(), VarProp {
                is_extern: false,
                is_mutable: false
            }))?;
        }

        let typ = self.body.check_type(vartypes, deftypes)?;
        if typ != self.rettype {
            eprintln!("func: {:?}, expected retrun type: {:?}, got: {:?}",
                self.name.unwrap(), self.rettype, typ);
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
    Identifier(&'input str, VarProp),
    StringLit(&'input str),
    IntLit(i64),
    RealLit(f64),
    BoolLit(bool),
    BinaryExpr(Box<Node<'input, Expr<'input>>>, Operator, Box<Node<'input, Expr<'input>>>),
    Call(Box<Node<'input, Expr<'input>>>, Vec<Node<'input, Expr<'input>>>),
    If(Box<Node<'input, Expr<'input>>>,
       Box<Node<'input, Expr<'input>>>,
       Option<Box<Node<'input, Expr<'input>>>>),
    For(Option<Box<Node<'input, Expr<'input>>>>,
        Box<Node<'input, Expr<'input>>>,
        Option<Box<Node<'input, Expr<'input>>>>,
        Box<Node<'input, Expr<'input>>>),
    Block(Vec<Node<'input, Expr<'input>>>),
    VarDef(&'input str, VarProp, Box<Node<'input, Expr<'input>>>),
    VarAssign(&'input str, VarProp, Box<Node<'input, Expr<'input>>>)
}

#[derive(Clone)]
pub struct Node<'input, T> {
    pub node: T,
    pub typ: Type<'input>
}

impl<'input, T> Node<'input, T> {
    pub fn new(node: T) -> Box<Node<'input, T>> {
        Box::new(Node { node, typ: Type::Untyped })
    }
}

impl<'input> Node<'input, Expr<'input>> {
    pub fn check_type(&mut self, vartypes: &mut utils::Env<'input, (Type<'input>, VarProp)>,
            deftypes: &mut utils::Env<'input, Type<'input>>)
            -> Result<Type<'input>, utils::Error> {
        let typ = match &mut self.node {
            Expr::Identifier(id, ref mut props) => {
                let (typ, varprops) = vartypes.lookup(id)?;
                *props = varprops;
                typ
            },
            Expr::StringLit(_) => Type::Ptr(CHAR_TYPE.clone()),
            Expr::IntLit(_) => Type::Int,
            Expr::RealLit(_) => Type::Real,
            Expr::BoolLit(_) => Type::Bool,
            Expr::BinaryExpr(ref mut lhs, op, ref mut rhs) => {
                let lhs = lhs.check_type(vartypes, deftypes)?;
                let rhs = rhs.check_type(vartypes, deftypes)?;
                match (&lhs, op, rhs) {
                    (&Type::Int, Operator::Add, Type::Int) => lhs,
                    (&Type::Int, Operator::Sub, Type::Int) => lhs,
                    (&Type::Int, Operator::Mul, Type::Int) => lhs,
                    (&Type::Int, Operator::Div, Type::Int) => lhs,

                    (&Type::Int, Operator::Equal, Type::Int) => Type::Bool,
                    (&Type::Int, Operator::Smaller, Type::Int) => Type::Bool,

                    (&Type::Bool, Operator::Or, Type::Bool) => lhs,
                    (&Type::Bool, Operator::And, Type::Bool) => lhs,

                    (a, op, b) => {
                        println!("{:?} {:?} {:?}", a, op, b);
                        panic!();
                    }
                }
            },
            Expr::Call(ref mut callee, ref mut args) => {
                let callee = callee.check_type(vartypes, deftypes)?;
                let (argtypes, rettype, is_vararg) = match callee {
                    Type::Func(ref argtypes, ref rettype, is_vararg) =>
                        (argtypes, rettype, is_vararg),
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
                    if &argtyp != typ {
                        panic!();
                    }
                }

                for arg in argiter {
                    arg.check_type(vartypes, deftypes)?;
                }

                rettype.as_ref().clone()
            },
            Expr::Block(ref mut stmts) => {
                let mut rettype = Type::Void;
                vartypes.push_scope();
                for stmt in stmts {
                    rettype = stmt.check_type(vartypes, deftypes)?;
                }
                vartypes.pop_scope();
                rettype
            },
            Expr::If(cond, thenblock, elseblock) => {
                let condtype = cond.check_type(vartypes, deftypes)?;
                if condtype != Type::Bool {
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
                if condtype != Type::Bool {
                    panic!();
                }
                body.check_type(vartypes, deftypes)?;
                if let Some(inc) = inc {
                    inc.check_type(vartypes, deftypes)?;
                }
                vartypes.pop_scope();
                Type::Void
            },
            Expr::VarDef(name, props, value) => {
                let typ = value.check_type(vartypes, deftypes)?;
                vartypes.define(name, (typ, *props))?;
                Type::Void
            },
            Expr::VarAssign(name, ref mut props, value) => {
                let (typ, varprops) = vartypes.lookup(name)?;
                let valtyp = value.check_type(vartypes, deftypes)?;
                if typ != valtyp {
                    panic!();
                }

                if varprops.is_mutable == false {
                    panic!();
                }

                self.typ = valtyp;
                *props = varprops;
                Type::Void
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
