use crate::ast::{Program, Function, Node, Expr, Definition, Type, Operator, VarProp};
use crate::utils;

use std::convert::TryInto;
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
    strings: HashMap<&'input str, inkwell::values::GlobalValue<'ctx>>,
    structs: HashMap<Type<'input>, inkwell::types::StructType<'ctx>>
}

impl<'ctx, 'input> CodeGen<'ctx, 'input> {
    pub fn new<'a, 'b>(llvm_context: &'a Context, modulename: &str) -> CodeGen<'a, 'b> {
        CodeGen {
            context: llvm_context,
            module: llvm_context.create_module(modulename),
            builder: llvm_context.create_builder(),
            strings: HashMap::new(),
            structs: HashMap::new()
        }
    }

    pub fn dump_to_stdout(&self) {
        let modstr = self.module.print_to_string().to_string();
        println!("{}", modstr);
    }

    fn get_struct_type(&mut self, structtyp: &Type<'input>)
            -> inkwell::types::StructType<'ctx> {
        if let Some(res) = self.structs.get(&structtyp) {
            return *res;
        }

        let fields = match structtyp {
            Type::Struct(fields) => fields.as_ref(),
            _ => panic!()
        };

        let fields: Vec<_> = fields
            .iter().map(|(_, typ)| self.get_llvm_type(&typ))
            .collect();

        let llvmstruct = self.context.struct_type(&fields, false);
        self.structs.insert(structtyp.clone(), llvmstruct);
        *self.structs.get(structtyp).unwrap()
    }

    fn get_struct_offset(&self, fields: &[(&'input str, Type<'input>)],
                         field: &'input str) -> Result<u32, utils::Error> {
        let idx = fields.iter()
            .enumerate()
            .find(|(_, (name, _))| name == &field)
            .map(|(i, _)| i).unwrap()
            .try_into().unwrap();

        // println!("{:?}[{:?}] -> {:?}", fields, field, idx);
        Ok(idx)
    }

    pub fn gen_code(&mut self, program: &Program<'input>) -> Result<(), utils::Error> {
        let mut env = utils::Env::<'input, BasicValueEnum>::new();
        for def in &program.defs {
            match def {
                Definition::Function(f) => self.gen_code_func(&f, &mut env)?,
                Definition::Extern(name, ref typ) => match *typ {
                    Type::Func(ref argtypes, ref rettype, is_vararg) => {
                        self.gen_func_def(name, argtypes, rettype, is_vararg)?;
                    },
                    _ => unimplemented!()
                },
                Definition::Type(_, _) => {}
            }
        }
        Ok(())
    }

    fn gen_func_def(&mut self, name: &'input str, argtypes: &[Type<'input>],
            rettype: &Type<'input>, is_vararg: bool)
            -> Result<inkwell::values::FunctionValue<'ctx>, utils::Error> {
        let llvmargtypes: Vec<_> = argtypes.iter().map(|arg| self.get_llvm_type(arg)).collect();

        let llvmfntype = match rettype {
            &Type::Void => self.context.void_type().fn_type(&llvmargtypes, is_vararg),
            rettype => self.get_llvm_type(rettype).fn_type(&llvmargtypes, is_vararg)
        };

        Ok(self.module.add_function(name, llvmfntype, None))
    }

    fn gen_code_func(&mut self, function: &Function<'input>,
                env: &mut utils::Env<'input, BasicValueEnum<'ctx>>)
                -> Result<(), utils::Error> {

        let args: Vec<_> = function.args.iter().map(|arg| arg.1.clone()).collect();
        let llvmfn = self.gen_func_def(function.name.unwrap(),
            &args, &function.rettype, function.is_vararg)?;

        env.push_scope();
        for i in 0..function.args.len() {
            let param = llvmfn.get_nth_param(i as u32).unwrap();
            match param {
                BasicValueEnum::IntValue(val) => val.set_name(function.args[i].0),
                BasicValueEnum::StructValue(val) => val.set_name(function.args[i].0),
                BasicValueEnum::PointerValue(val) => val.set_name(function.args[i].0),
                _ => {}
            }
            env.define(function.args[i].0, param)?;
        }

        let bb = self.context.append_basic_block(llvmfn, "start");
        self.builder.position_at_end(&bb);

        let retval = self.gen_code_node(function.body.as_ref(), env, &bb)?;

        self.builder.build_return(match function.rettype {
            Type::Void => None,
            _ => Some(&retval)
        });
        env.pop_scope();
        Ok(())
    }

    #[allow(clippy::trivially_copy_pass_by_ref)]
    fn gen_code_node(&mut self, node: &Node<'input, Expr<'input>>,
            env: &mut utils::Env<'input, BasicValueEnum<'ctx>>, bb: &BasicBlock)
            -> Result<BasicValueEnum<'ctx>, utils::Error> {
        match &node.node {
            Expr::Identifier(id, VarProp { is_mutable: false, .. }) => env.lookup(id),

            Expr::Identifier(id, VarProp { is_mutable: true, .. }) => {
                let ptr = match env.lookup(id)? {
                    BasicValueEnum::PointerValue(ptr) => ptr,
                    _ => panic!()
                };
                Ok(self.builder.build_load(ptr, id))
            },

            Expr::IntLit(num) =>
                Ok(BasicValueEnum::IntValue(self.context.i64_type()
                    .const_int(unsafe { std::mem::transmute(*num) }, false))),

            Expr::BoolLit(true) => Ok(self.context.bool_type().const_all_ones().as_basic_value_enum()),
            Expr::BoolLit(false) => Ok(self.context.bool_type().const_zero().as_basic_value_enum()),

            Expr::StringLit(string) => {
                if let Some(global) = self.strings.get(string) {
                    return Ok(global.clone().as_basic_value_enum())
                }

                let string = utils::unescape_parsed_string(string)?;
                let global = self.builder.build_global_string_ptr(string.as_ref(), "global_string");
                Ok(global.as_basic_value_enum())
            },

            Expr::StructLit(_, fields) => {
                let typ = node.get_type();
                let structfields = match typ {
                    Type::Struct(ref fields) => fields.as_ref(),
                    _ => panic!()
                };
                /* Wieso funktioniert das nicht?

                let llvmstruct = self.get_struct_type(&typ).const_zero();

                for (field, value) in fields {
                    println!("{:?} -> {:?}", field, value);
                    let value = self.gen_code_node(value, env, bb)?;
                    let offset = self.get_struct_offset(structfields, field)?;
                    self.builder.build_insert_value(llvmstruct, value, offset, "");
                }

                Ok(llvmstruct.as_basic_value_enum())
                */

                let llvmtyp = self.get_struct_type(&typ);
                let ptr = self.builder.build_alloca(llvmtyp, "");

                for (field, value) in fields {
                    let value = self.gen_code_node(value, env, bb)?;
                    let offset = self.get_struct_offset(structfields, field)?;
                    let ptr = unsafe { self.builder.build_struct_gep(ptr, offset, "") };
                    self.builder.build_store(ptr, value);
                }

                Ok(self.builder.build_load(ptr, ""))
            },

            Expr::BinaryExpr(lhs, op, rhs) => {
                if lhs.get_type() == Type::Bool && rhs.get_type() == Type::Bool
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

                let (lhsval, lhstyp) = (self.gen_code_node(lhs, env, bb)?, lhs.get_type());
                let (rhsval, rhstyp) = (self.gen_code_node(rhs, env, bb)?, rhs.get_type());
                let res = match (lhstyp, op, rhstyp) {
                    (Type::Int, Operator::Add, Type::Int) =>
                        BasicValueEnum::IntValue(
                            self.builder.build_int_add(
                                self.to_int_value(lhsval), self.to_int_value(rhsval), "addres")),
                    (Type::Int, Operator::Sub, Type::Int) =>
                        BasicValueEnum::IntValue(
                            self.builder.build_int_sub(
                                self.to_int_value(lhsval), self.to_int_value(rhsval), "subres")),
                    (Type::Int, Operator::Mul, Type::Int) =>
                        BasicValueEnum::IntValue(
                            self.builder.build_int_mul(
                                self.to_int_value(lhsval), self.to_int_value(rhsval), "addres")),
                    (Type::Int, Operator::Div, Type::Int) =>
                        BasicValueEnum::IntValue(
                            self.builder.build_int_signed_div(
                                self.to_int_value(lhsval), self.to_int_value(rhsval), "subres")),

                    (Type::Ptr(_), Operator::Add, Type::Int) => {
                        let (ptr, offset) = match (lhsval, rhsval) {
                            (BasicValueEnum::PointerValue(ptr), BasicValueEnum::IntValue(offset))
                                => (ptr, offset),
                            _ => panic!()
                        };

                        let ptr = unsafe {
                            self.builder.build_gep(ptr, &[offset], "ptradd")
                        };

                        ptr.as_basic_value_enum()
                    },

                    (Type::Int, Operator::Smaller, Type::Int) =>
                        BasicValueEnum::IntValue(
                            self.builder.build_int_compare(
                                inkwell::IntPredicate::SLT,
                                self.to_int_value(lhsval), self.to_int_value(rhsval), "cmpres")),
                    (Type::Int, Operator::Equal, Type::Int) =>
                        BasicValueEnum::IntValue(
                            self.builder.build_int_compare(
                                inkwell::IntPredicate::EQ,
                                self.to_int_value(lhsval), self.to_int_value(rhsval), "cmpres")),
                    (Type::Int, Operator::Greater, Type::Int) =>
                        BasicValueEnum::IntValue(
                            self.builder.build_int_compare(
                                inkwell::IntPredicate::SGT,
                                self.to_int_value(lhsval), self.to_int_value(rhsval), "cmpres")),

                    _ => unimplemented!()
                };
                Ok(res)
            },

            Expr::Not(arg) => match self.gen_code_node(arg, env, bb)? {
                BasicValueEnum::IntValue(val) =>
                    Ok(BasicValueEnum::IntValue(
                        self.builder.build_not(val, "notres"))),
                _ => panic!()
            },

            Expr::Call(callee, args) => match callee.node {
                Expr::Identifier(fnname, _) => {
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
                let typ = node.get_type();
                if typ != Type::Void {
                    let llvmtyp = self.get_llvm_type(&typ);
                    let phi = self.builder.build_phi(llvmtyp, "phires");
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

            Expr::If(cond, thenexpr, None) => {
                let cond = match self.gen_code_node(cond, env, bb)? {
                    BasicValueEnum::IntValue(val) => val,
                    _ => panic!()
                };

                let func = bb.get_parent().unwrap();
                let thenbb = self.context.append_basic_block(func, "then");
                let donebb = self.context.append_basic_block(func, "done");

                self.builder.build_conditional_branch(cond, &thenbb, &donebb);

                self.builder.position_at_end(&thenbb);
                self.gen_code_node(thenexpr, env, &thenbb)?;
                self.builder.build_unconditional_branch(&donebb);

                self.builder.position_at_end(&donebb);
                Ok(self.default_val())
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

            Expr::VarDef(name, VarProp { is_mutable: true, .. }, value) => {
                let valtyp = value.get_type();
                let value = self.gen_code_node(value, env, bb)?;
                let llvmtyp = self.get_llvm_type(&valtyp);
                let ptr = self.builder.build_alloca(llvmtyp, name);
                env.define(name, ptr.as_basic_value_enum())?;
                self.builder.build_store(ptr, value);
                Ok(value)
            },

            Expr::VarDef(name, VarProp { is_mutable: false, .. }, value) => {
                let value = self.gen_code_node(value, env, bb)?;
                env.define(name, value)?;
                Ok(value)
            },

            Expr::Assign(lval, value) => match &(*lval).node {
                Expr::Identifier(name, VarProp { is_mutable: true, .. }) => {
                    let value = self.gen_code_node(value, env, bb)?;
                    let ptr = match env.lookup(name)? {
                        BasicValueEnum::PointerValue(ptr) => ptr,
                        _ => panic!()
                    };
                    self.builder.build_store(ptr, value);
                    Ok(value)
                },
                Expr::PtrDeref(lval, offset) => {
                    let value = self.gen_code_node(value, env, bb)?;
                    let mut ptr = match self.gen_code_node(&lval, env, bb)? {
                        BasicValueEnum::PointerValue(ptr) => ptr,
                        _ => panic!()
                    };

                    if *offset != 0 {
                        let offset = self.context.i64_type().const_int(*offset as u64, false);
                        ptr = unsafe { self.builder.build_gep(ptr, &[offset], "") };
                    }

                    self.builder.build_store(ptr, value);
                    Ok(value)
                },
                Expr::StructFieldAccess(structval, field) => {
                    let value = self.gen_code_node(value, env, bb)?;
                    let typ = structval.get_type();
                    let (fields, viaptr) = match typ {
                        Type::Struct(ref fields) => (fields.as_ref(), false),
                        Type::Ptr(ref typ) => match typ.as_ref() {
                            Type::Struct(ref fields) => (fields.as_ref(), true),
                            _ => panic!()
                        },
                        _ => panic!()
                    };
                    let offset = self.get_struct_offset(fields, field)?;
                    if !viaptr {
                        if let Expr::PtrDeref(ptr, 0) = &structval.node {
                            let ptr = match self.gen_code_node(&ptr, env, bb)? {
                                BasicValueEnum::PointerValue(ptr) => ptr,
                                _ => panic!()
                            };

                            let ptr = unsafe { self.builder.build_struct_gep(ptr, offset, "") };
                            self.builder.build_store(ptr, value);
                        } else {
                            let structval = match self.gen_code_node(structval, env, bb)? {
                                BasicValueEnum::StructValue(structval) => structval,
                                _ => panic!()
                            };
                            self.builder.build_insert_value(structval, value, offset, "").unwrap();
                        }
                    } else {
                        let ptr = match self.gen_code_node(structval, env, bb)? {
                            BasicValueEnum::PointerValue(ptr) => unsafe {
                                self.builder.build_struct_gep(ptr, offset, "")
                            },
                            _ => panic!()
                        };
                        self.builder.build_store(ptr, value);
                    }

                    Ok(value)
                },
                _ => panic!()
            },

            Expr::PtrDeref(lval, offset) => {
                let mut ptr = match self.gen_code_node(lval, env, bb)? {
                    BasicValueEnum::PointerValue(ptr) => ptr,
                    _ => panic!()
                };

                if *offset != 0 {
                    let offset = self.context.i64_type().const_int(*offset as u64, false);
                    ptr = unsafe { self.builder.build_gep(ptr, &[offset], "") };
                }

                Ok(self.builder.build_load(ptr, ""))
            },

            Expr::Cast(value, _) => {
                let valtyp = value.get_type();
                let value = self.gen_code_node(value, env, bb)?;
                let typ = self.get_llvm_type(&node.get_type());
                if let BasicTypeEnum::PointerType(typ) = typ {
                    if let BasicValueEnum::PointerValue(ptr) = value {
                        Ok(self.builder.build_pointer_cast(ptr, typ, "ptrcast")
                           .as_basic_value_enum())
                    } else {
                        panic!()
                    }
                } else if valtyp == node.get_type() {
                    Ok(value)
                } else {
                    panic!()
                }
            },

            Expr::StructFieldAccess(structval, field) => {
                let typ = structval.get_type();
                let (fields, viaptr) = match typ {
                    Type::Struct(ref fields) => (fields.as_ref(), false),
                    Type::Ptr(ref typ) => match typ.as_ref() {
                        Type::Struct(ref fields) => (fields.as_ref(), true),
                        _ => panic!()
                    },
                    _ => panic!()
                };
                let offset = self.get_struct_offset(fields, field)?;
                let structval = self.gen_code_node(structval, env, bb)?;
                if viaptr {
                    let ptr = match structval {
                        BasicValueEnum::PointerValue(ptr) => unsafe {
                            self.builder.build_struct_gep(ptr, offset, "")
                        },
                        _ => panic!()
                    };
                    Ok(self.builder
                       .build_load(ptr, ""))
                    
                } else {
                    let structval = match structval {
                        BasicValueEnum::StructValue(val) => val, _ => panic!()
                    };
                    Ok(self.builder
                        .build_extract_value(structval, offset, "")
                        .unwrap())
                }
            },

            other => unimplemented!("{:?}", other)
        }
    }

    fn default_val(&self) -> BasicValueEnum<'ctx> {
        BasicValueEnum::IntValue(self.context.i64_type().const_zero())
    }

    fn get_llvm_type(&mut self, typ: &Type<'input>) -> BasicTypeEnum<'ctx> {
        match *typ {
            Type::Bool => self.context.bool_type().as_basic_type_enum(),
            Type::Int => self.context.i64_type().as_basic_type_enum(),
            Type::Char => self.context.i8_type().as_basic_type_enum(),
            Type::Ptr(ref typ) => match typ.as_ref() {
                Type::Void => self.context.i8_type()
                    .ptr_type(inkwell::AddressSpace::Generic)
                    .as_basic_type_enum(),
                typ => self.get_llvm_type(typ)
                    .ptr_type(inkwell::AddressSpace::Generic)
                    .as_basic_type_enum()
            },
            Type::Struct(_) => self.get_struct_type(typ).as_basic_type_enum(),
            _ => unimplemented!("{:?}", typ)
        }
    }

    fn to_int_value(&self, val: BasicValueEnum<'ctx>) -> inkwell::values::IntValue<'ctx> {
        match val {
            BasicValueEnum::IntValue(val) => val,
            _ => panic!()
        }
    }
}
