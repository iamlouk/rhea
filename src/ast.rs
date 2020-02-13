use std::sync::Arc;
use std::collections::HashMap;

use crate::utils;

pub type ExprNode<'input> = Node<'input, Expr<'input>>;

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
                    }))?,
                Definition::Type(name, typ) => deftypes.define(name, typ.clone())?
            }
        }

        let mut types: HashMap<&'input str, Type<'input>> = deftypes.take_bottom_scope().unwrap();
        for (_, typ) in &mut types {
            *typ = typ.resolve_type(deftypes)?;
        }

        deftypes.add_bottom_scope(types);

        for definition in &mut self.defs {
            match definition {
                Definition::Function(f) => {
                    f.check_type(vartypes, deftypes)?;
                },
                Definition::Extern(_, _) => {},
                Definition::Type(_, _) => {}
            }
        }

        Ok(())
    }
}

#[derive(Debug)]
pub enum Definition<'input> {
    Function(Function<'input>),
    Type(&'input str, Type<'input>),
    Extern(&'input str, Type<'input>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type<'input> {
    Untyped, Void, Int,
    Real, Bool, Char,
    Struct(Arc<Vec<(&'input str, Type<'input>)>>),
    Tuple(Arc<Vec<Type<'input>>>), Ptr(Arc<Type<'input>>),
    Unresolved(&'input str), // Named(&'input str, Arc<Type<'input>>),
    Func(Arc<Vec<Type<'input>>>, Arc<Type<'input>>, bool)
}

impl<'input> Type<'input> {
    /*
     * TODO:
     * Wenn man irgendwie den Inhalt eines Arc austauschen
     * koennte (was man vermutlich kann, ich weiß nur nicht wie),
     * dann koennte man diesen Code noch viel performanter machen!
     */
    pub fn resolve_type(&self, types: &utils::Env<'input, Type<'input>>)
            -> Result<Type<'input>, utils::Error> {
        match self {
            Type::Struct(fields) => {
                let mut fields = fields.as_ref().clone();
                for pair in fields.iter_mut() {
                    let newfield = pair.1.resolve_type(types)?;
                    pair.1 = newfield;
                }
                Ok(Type::Struct(Arc::new(fields)))
            },
            Type::Tuple(_) => unimplemented!(),
            Type::Func(args, rettype, attrs) => {
                let mut args = args.as_ref().clone();
                for arg in args.iter_mut() {
                    let newarg = arg.resolve_type(types)?;
                    *arg = newarg;
                }

                let rettype = rettype.as_ref().resolve_type(types)?;
                Ok(Type::Func(Arc::new(args), Arc::new(rettype), *attrs))
            },
            Type::Unresolved(name) => types.lookup(name),
            other => Ok(other.clone())
        }
    }
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

        for arg in &mut self.args {
            arg.1 = arg.1.resolve_type(deftypes)?;

            vartypes.define(arg.0, (arg.1.clone(), VarProp {
                is_extern: false,
                is_mutable: false
            }))?;
        }

        let typ = self.body.check_type(vartypes, deftypes)?;
        self.rettype = self.rettype.resolve_type(deftypes)?;
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
    BinaryExpr(Box<ExprNode<'input>>, Operator, Box<ExprNode<'input>>),
    Call(Box<ExprNode<'input>>, Vec<ExprNode<'input>>),
    If(Box<ExprNode<'input>>,
       Box<ExprNode<'input>>,
       Option<Box<ExprNode<'input>>>),
    For(Option<Box<ExprNode<'input>>>,
        Box<ExprNode<'input>>,
        Option<Box<ExprNode<'input>>>,
        Box<ExprNode<'input>>),
    Block(Vec<ExprNode<'input>>),
    VarDef(&'input str, VarProp, Box<ExprNode<'input>>),
    Assign(Box<ExprNode<'input>>, Box<ExprNode<'input>>),
    Cast(Box<ExprNode<'input>>, Type<'input>),
    PtrDeref(Box<ExprNode<'input>>, u32),
    Nop
}

#[derive(Clone)]
pub struct Node<'input, T> {
    pub node: T,
    typ: Type<'input>
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
                let typ = typ.resolve_type(deftypes)?;
                *props = varprops;
                typ
            },
            Expr::StringLit(_) => Type::Ptr(Arc::new(Type::Char)),
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

                    (&Type::Ptr(_), Operator::Add, Type::Int) => lhs,

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
                        panic!("{:?} != {:?}", argtyp, typ);
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
                if typ == Type::Void {
                    panic!()
                } else {
                    vartypes.define(name, (typ, *props))?;
                    Type::Void
                }
            },
            Expr::Assign(lval, value) => match &mut (*lval).node {
                Expr::Identifier(name, ref mut props) => {
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
                },
                Expr::PtrDeref(lval, _) => {
                    let lvaltyp = lval.check_type(vartypes, deftypes)?;
                    let valtyp = value.check_type(vartypes, deftypes)?;
                    if let Type::Ptr(lvaltyp) = lvaltyp {
                        if valtyp == *lvaltyp && valtyp != Type::Void {
                            Type::Void
                        } else {
                            panic!()
                        }
                    } else {
                        panic!()
                    }
                },
                _ => panic!()
            },
            Expr::Cast(value, typ) => match (value.check_type(vartypes, deftypes)?, typ.resolve_type(deftypes)?) {
                (Type::Ptr(_), Type::Ptr(totyp)) => Type::Ptr(totyp.clone()),
                (a, b) if a == b => a,
                _ => unimplemented!()
            },
            Expr::PtrDeref(ptrvalue, _) => match ptrvalue.check_type(vartypes, deftypes)? {
                Type::Ptr(valtyp) => valtyp.as_ref().clone(),
                _ => unimplemented!()
            },
            Expr::Nop => unimplemented!()
        };

        self.typ = typ.clone();
        Ok(typ)
    }

    pub fn get_type(&self) -> Type { self.typ.clone() }
}

impl<'input, T: std::fmt::Debug> std::fmt::Debug for Node<'input, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({:?}: {:?})", self.node, self.typ)
    }
}
