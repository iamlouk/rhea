use crate::ast::*;
use crate::lexer::*;
use crate::utils;


type BoxedExpr<'input> = Box<ExprNode<'input>>;

enum Res<'input> {
    Ok(Box<ExprNode<'input>>),
    Err(utils::Error),
    Undo(Option<Box<ExprNode<'input>>>)
}

pub struct Parser<'input> {
    tokens: Vec<Tok<'input>>,
    pos: usize
}

impl<'input> Parser<'input> {
    pub fn new(lexer: Lexer<'input>) -> Result<Self, utils::Error> {
        let tokens: Result<Vec<_>, _> = lexer
            .map(|res| res.map(|(_, tok, _)| tok))
            .collect();

        Ok(Parser {
            tokens: tokens?,
            pos: 0
        })
    }

    fn parse_struct_lit(&mut self, structname: BoxedExpr<'input>) -> Res<'input> {
        unimplemented!()
    }

    fn parse_call(&mut self, callee: BoxedExpr<'input>) -> Res<'input> {
        unimplemented!()
    }

    fn parse_expr_lvl5(&mut self) -> Res<'input> {
        self.pos += 1;
        let mut expr = match self.tokens[self.pos - 1] {
            Tok::Id(name) => Node::new(Expr::Identifier(name, VarProp::default())),
            _ => {
                self.pos -= 1;
                return Res::Undo(None);
            }
        };

        loop {
            let mut parsed_something = false;

            let res = self.parse_call(expr);
            expr = match res {
                Res::Ok(expr) => { parsed_something = true; expr },
                Res::Err(e) => return Res::Err(e),
                Res::Undo(expr) => expr.unwrap()
            };

            let res = self.parse_struct_lit(expr);
            expr = match res {
                Res::Ok(expr) => { parsed_something = true; expr },
                Res::Err(e) => return Res::Err(e),
                Res::Undo(expr) => expr.unwrap()
            };

            if !parsed_something { break; }
        }

        Res::Ok(expr)
    }
}



