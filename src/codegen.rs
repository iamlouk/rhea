use crate::ast::{Program, Function, Node, Expr, Definition, Type, Operator};
use crate::utils;

use std::sync::Arc;
use std::collections::HashMap;

use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;

use inkwell::types::{BasicType, BasicTypeEnum};
use inkwell::values::{BasicValueEnum, BasicValue};
use inkwell::basic_block::BasicBlock;

pub struct CodeGen<'ctx, 'input> {
    context: &'ctx Context,
    pub module: Module<'ctx>,
    builder: Builder<'ctx>,
    strings: HashMap<&'input str, inkwell::values::GlobalValue<'ctx>>
}

impl<'ctx, 'input> CodeGen<'ctx, 'input> {
    pub fn new<'a, 'b>(llvm_context: &'a Context, modulename: &str) -> CodeGen<'a, 'b> {
        CodeGen {
            context: llvm_context,
            module: llvm_context.create_module(modulename),
            builder: llvm_context.create_builder(),
            strings: HashMap::new()
        }
    }

    pub fn dump_to_stdout(&self) {
        let modstr = self.module.print_to_string().to_string();
        println!("{}", modstr);
    }

    pub fn gen_code(&mut self, program: &Program<'input>) -> Result<(), utils::Error> {
        let mut env = utils::Env::<'input, BasicValueEnum>::new();
        for def in &program.defs {
            match def {
                Definition::Function(f) => self.gen_code_func(&f, &mut env)?,
                Definition::Extern(name, typ) => match typ.as_ref() {
                    &Type::Func(ref argtypes, ref rettype, is_vararg) => {
                        self.gen_func_def(name, argtypes, rettype, is_vararg)?;
                    },
                    _ => unimplemented!()
                }
            }
        }
        Ok(())
    }

    fn gen_func_def(&mut self, name: &'input str, argtypes: &Vec<Arc<Type<'input>>>,
            rettype: &Type<'input>, is_vararg: bool) -> Result<inkwell::values::FunctionValue<'ctx>, utils::Error> {
        let llvmrettype = self.get_llvm_type(rettype);
        let llvmargtypes: Vec<_> = argtypes.iter().map(|arg| self.get_llvm_type(arg)).collect();
        let llvmfntype = llvmrettype.fn_type(&llvmargtypes, is_vararg);

        Ok(self.module.add_function(name, llvmfntype, None))
    }

    fn gen_code_func(&mut self, function: &Function<'input>,
                env: &mut utils::Env<'input, BasicValueEnum<'ctx>>)
                -> Result<(), utils::Error> {

        let args = function.args.iter().map(|arg| arg.1.clone()).collect();
        let llvmfn = self.gen_func_def(function.name.unwrap(),
            &args, function.rettype.as_ref(), function.is_vararg)?;

        env.push_scope();
        for i in 0..function.args.len() {
            let param = llvmfn.get_nth_param(i as u32).unwrap();
            env.define(function.args[i].0, param)?;
        }

        let bb = self.context.append_basic_block(llvmfn, "start");
        self.builder.position_at_end(&bb);

        let retval = self.gen_code_node(function.body.as_ref(), env, &bb)?;

        self.builder.build_return(Some(&retval));
        env.pop_scope();
        Ok(())
    }

    fn gen_code_node(&mut self, node: &Node<'input, Expr<'input>>,
            env: &mut utils::Env<'input, BasicValueEnum<'ctx>>, bb: &BasicBlock)
            -> Result<BasicValueEnum<'ctx>, utils::Error> {
        match &node.node {
            Expr::Identifier(id) => env.lookup(id),

            Expr::IntLit(num) =>
                Ok(BasicValueEnum::IntValue(self.context.i64_type()
                    .const_int(unsafe { std::mem::transmute(*num) }, false))),

            Expr::StringLit(string) => {
                if let Some(global) = self.strings.get(string) {
                    return Ok(global.clone().as_basic_value_enum())
                }

                let global = self.builder.build_global_string_ptr(string, "global_string");
                Ok(global.as_basic_value_enum())
            },

            Expr::BinaryExpr(lhs, op, rhs) => {
                if lhs.typ.as_ref() == &Type::Bool && rhs.typ.as_ref() == &Type::Bool
                    && (op == &Operator::And || op == &Operator::Or) {
                    let lhsval = self.gen_code_node(lhs, env, bb)?;
                    let func = bb.get_parent().unwrap();
                    let mut bbtrue = self.context.append_basic_block(func, "ssl_lhs_true");
                    let mut bbfalse = self.context.append_basic_block(func, "ssl_lhs_false");
                    let bbdone = self.context.append_basic_block(func, "ssl_done");

                    let bbtrueval;
                    let bbfalseval;

                    self.builder.build_conditional_branch(
                        self.to_int_value(lhsval), &bbtrue, &bbfalse);
                    if op == &Operator::Or {
                        self.builder.position_at_end(&bbtrue);
                        bbtrueval = self.context.bool_type().const_all_ones();
                        self.builder.build_unconditional_branch(&bbdone);
                        bbtrue = self.builder.get_insert_block().unwrap();

                        self.builder.position_at_end(&bbfalse);
                        let tmp = self.gen_code_node(rhs, env, &bbfalse)?;
                        bbfalseval = self.to_int_value(tmp);
                        self.builder.build_unconditional_branch(&bbdone);
                        bbfalse = self.builder.get_insert_block().unwrap();
                    } else if op == &Operator::And {
                        self.builder.position_at_end(&bbfalse);
                        bbfalseval = self.context.bool_type().const_zero();
                        self.builder.build_unconditional_branch(&bbdone);
                        bbtrue = self.builder.get_insert_block().unwrap();

                        self.builder.position_at_end(&bbtrue);
                        let tmp = self.gen_code_node(rhs, env, &bbtrue)?;
                        bbtrueval = self.to_int_value(tmp);
                        self.builder.build_unconditional_branch(&bbdone);
                        bbfalse = self.builder.get_insert_block().unwrap();
                    } else {
                        unimplemented!();
                    }

                    self.builder.position_at_end(&bbdone);
                    let phi = self.builder.build_phi(self.context.bool_type(), "ssl_res");
                    let incomings: [(&dyn BasicValue<'ctx>, &BasicBlock); 2] = [
                        (&bbtrueval, &bbtrue), (&bbfalseval, &bbfalse)
                    ];
                    phi.add_incoming(&incomings);
                    return Ok(phi.as_basic_value());
                }

                let (lhsval, lhstyp) = (self.gen_code_node(lhs, env, bb)?, lhs.typ.as_ref());
                let (rhsval, rhstyp) = (self.gen_code_node(rhs, env, bb)?, rhs.typ.as_ref());
                let res = match (lhstyp, op, rhstyp) {
                    (&Type::Int, Operator::Add, &Type::Int) =>
                        BasicValueEnum::IntValue(
                            self.builder.build_int_add(
                                self.to_int_value(lhsval), self.to_int_value(rhsval), "addres")),
                    (&Type::Int, Operator::Sub, &Type::Int) =>
                        BasicValueEnum::IntValue(
                            self.builder.build_int_sub(
                                self.to_int_value(lhsval), self.to_int_value(rhsval), "subres")),
                    (&Type::Int, Operator::Mul, &Type::Int) =>
                        BasicValueEnum::IntValue(
                            self.builder.build_int_mul(
                                self.to_int_value(lhsval), self.to_int_value(rhsval), "addres")),
                    (&Type::Int, Operator::Div, &Type::Int) =>
                        BasicValueEnum::IntValue(
                            self.builder.build_int_signed_div(
                                self.to_int_value(lhsval), self.to_int_value(rhsval), "subres")),


                    (&Type::Int, Operator::Smaller, &Type::Int) =>
                        BasicValueEnum::IntValue(
                            self.builder.build_int_compare(
                                inkwell::IntPredicate::SLT,
                                self.to_int_value(lhsval), self.to_int_value(rhsval), "cmpres")),
                    (&Type::Int, Operator::Equal, &Type::Int) =>
                        BasicValueEnum::IntValue(
                            self.builder.build_int_compare(
                                inkwell::IntPredicate::EQ,
                                self.to_int_value(lhsval), self.to_int_value(rhsval), "cmpres")),

                    _ => unimplemented!()
                };
                Ok(res)
            },

            Expr::Call(callee, args) => match callee.node {
                Expr::Identifier(fnname) => {
                    let llvmfn = self.module.get_function(fnname).unwrap();
                    let args: Vec<BasicValueEnum<'ctx>> = args.iter().map(|arg| {
                        self.gen_code_node(arg, env, bb)
                    }).collect::<Result<Vec<BasicValueEnum<'ctx>>, utils::Error>>()?;

                    let res = self.builder.build_call(llvmfn, &args, "callres")
                        .try_as_basic_value();
                    match res.left() {
                        Some(val) => Ok(val),
                        None => Ok(self.default_val())
                    }
                },
                _ => unimplemented!()
            },

            Expr::If(cond, thenexpr, Some(elseexpr)) => {
                let cond = match self.gen_code_node(cond, env, bb)? {
                    BasicValueEnum::IntValue(val) => val,
                    _ => panic!()
                };

                let func = bb.get_parent().unwrap();
                let mut thenbb = self.context.append_basic_block(func, "then");
                let mut elsebb = self.context.append_basic_block(func, "else");
                let donebb = self.context.append_basic_block(func, "done");

                self.builder.build_conditional_branch(cond, &thenbb, &elsebb);

                self.builder.position_at_end(&thenbb);
                let thenval = self.gen_code_node(thenexpr, env, &thenbb)?;
                self.builder.build_unconditional_branch(&donebb);
                thenbb = self.builder.get_insert_block().unwrap();

                self.builder.position_at_end(&elsebb);
                let elseval = self.gen_code_node(elseexpr, env, &elsebb)?;
                self.builder.build_unconditional_branch(&donebb);
                elsebb = self.builder.get_insert_block().unwrap();

                self.builder.position_at_end(&donebb);
                let typ = node.typ.as_ref();
                if typ != &Type::Void {
                    let phi = self.builder.build_phi(self.get_llvm_type(typ), "phires");
                    let incomings: [(&dyn BasicValue<'ctx>, &BasicBlock); 2] = [
                        (&thenval, &thenbb),
                        (&elseval, &elsebb)
                    ];
                    phi.add_incoming(&incomings);
                    Ok(phi.as_basic_value())
                } else {
                    Ok(self.default_val())
                }
            },

            Expr::For(init, cond, inc, body) => {
                env.push_scope();
                if let Some(init) = init {
                    self.gen_code_node(init, env, bb)?;
                }

                let func = bb.get_parent().unwrap();
                let loopcondbb = self.context.append_basic_block(func, "loopcond");
                let loopbb = self.context.append_basic_block(func, "loop");
                let donebb = self.context.append_basic_block(func, "done");
                self.builder.build_unconditional_branch(&loopcondbb);

                self.builder.position_at_end(&loopcondbb);
                let cond = self.gen_code_node(cond, env, &loopcondbb)?;
                self.builder.build_conditional_branch(
                    self.to_int_value(cond), &loopbb, &donebb);

                self.builder.position_at_end(&loopbb);
                self.gen_code_node(body, env, &loopbb)?;
                if let Some(inc) = inc {
                    self.gen_code_node(inc, env, &loopbb)?;
                }

                self.builder.build_unconditional_branch(&loopcondbb);
                self.builder.position_at_end(&donebb);
                env.pop_scope();
                Ok(self.default_val())
            },

            Expr::Block(stmts) => {
                let mut retval: BasicValueEnum<'ctx> = self.default_val();
                env.push_scope();
                for stmt in stmts {
                    retval = self.gen_code_node(stmt, env, bb)?;
                }
                env.pop_scope();
                Ok(retval)
            },

            _ => unimplemented!()
        }
    }

    fn default_val(&self) -> BasicValueEnum<'ctx> {
        BasicValueEnum::IntValue(self.context.i64_type().const_zero())
    }

    fn get_llvm_type(&self, typ: &Type) -> BasicTypeEnum<'ctx> {
        match *typ {
            Type::Bool => self.context.bool_type().as_basic_type_enum(),
            Type::Int => self.context.i64_type().as_basic_type_enum(),
            _ => unimplemented!()
        }
    }

    fn to_int_value(&self, val: BasicValueEnum<'ctx>) -> inkwell::values::IntValue<'ctx> {
        match val {
            BasicValueEnum::IntValue(val) => val,
            _ => panic!()
        }
    }
}

