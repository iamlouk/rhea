#![feature(box_syntax)]
#![allow(unused_parens)]

use std::io::Read;
use std::convert::TryInto;

#[macro_use]
extern crate lalrpop_util;
extern crate inkwell;

extern crate clap;

mod lexer;
mod ast;
mod utils;
mod codegen;
lalrpop_mod!(pub parser);

fn run_optimizations(module: &inkwell::module::Module) -> bool {
    use inkwell::OptimizationLevel::Aggressive;
    use inkwell::passes::{PassManager, PassManagerBuilder};
    use inkwell::targets::{InitializationConfig, Target};

    let config = InitializationConfig::default();
    Target::initialize_native(&config).unwrap();

    let pass_manager_builder = PassManagerBuilder::create();
    pass_manager_builder.set_optimization_level(Aggressive);

    let pm = PassManager::create(());
    pass_manager_builder.populate_module_pass_manager(&pm);

    pm.run_on(module)
}

fn run_jit<'input>(module: &inkwell::module::Module,
                   vartypes: &utils::Env<'input, (ast::Type<'input>, ast::VarProp)>,
                   deftypes: &utils::Env<'input, ast::Type<'input>>) -> i64 {
    use inkwell::OptimizationLevel;

    match vartypes.lookup("main") {
        Ok((ast::Type::Func(args, rettyp, is_vararg), _))
            if is_vararg == false && args.len() == 0
            && rettyp.as_ref().resolve_type(&deftypes).unwrap() == ast::Type::Int => (),
        _ => {
            eprintln!("Internal Error: your code needs a `main: (): Int` \
                       function to run in JIT-mode");
            std::process::exit(5);
        }
    }

    let execution_engine = match module
            .create_jit_execution_engine(OptimizationLevel::Default) {
        Ok(ee) => ee,
        Err(e) => {
            eprintln!("LLVM-Jit-Error: {}", e.to_string());
            std::process::exit(5);
        }
    };

    let mainfn = match unsafe { execution_engine
        .get_function::<unsafe extern "C" fn() -> i64>("main") } {
            Ok(f) => f,
            Err(e) => {
                eprintln!("LLVM-Jit-Error: {}", e.to_string());
                std::process::exit(5);
            }
    };

    unsafe { mainfn.call() }
}

fn main() {
    let matches = clap::App::new("RHEA Programming Language Compiler + JIT")
        .version("0.1.0")
        .author("Lou K. <lou.knauer98@gmail.com>")
        .arg(clap::Arg::with_name("input")
             .short("i")
             .long("input")
             .value_name("file")
             .help("rhea-file to be compiled, default is stdin")
             .takes_value(true)
             .multiple(false))
        .arg(clap::Arg::with_name("output")
             .short("o")
             .long("output")
             .value_name("file")
             .help("path to file where llvm-bitcode should be stored, \
                   optional if `--jit` or `--dump` is selected")
             .required_unless_one(&["dump", "jit"])
             .takes_value(true)
             .multiple(false))
        .arg(clap::Arg::with_name("dump")
             .short("d")
             .long("dump")
             .takes_value(false)
             .help("dump llvm-bitcode in human readable form"))
        .arg(clap::Arg::with_name("optimize")
             .short("O")
             .long("optimize")
             .takes_value(false)
             .help("enable optimization"))
        .arg(clap::Arg::with_name("jit")
             .long("jit")
             .takes_value(false)
             .required(false)
             .conflicts_with("output")
             .help("run `main: (): Int` and print its result"))
        .get_matches();

    let input = match matches.value_of("input") {
        Some(filepath) => std::fs::read_to_string(filepath).unwrap(),
        None => {
            let mut input = String::new();
            std::io::stdin().read_to_string(&mut input).unwrap();
            input
        }
    };

    let lexer = lexer::Lexer::new(&input);
    let mut prog: ast::Program = match parser::ProgramParser::new().parse(&input, lexer) {
        Ok(p) => p,
        Err(e) => {
            eprintln!("Error while parsing: {:?}", e);
            std::process::exit(1);
        }
    };

    let mut vartypes = utils::Env::new();
    let mut deftypes = utils::Env::new();
    if let Err(e) = prog.check_type(&mut vartypes, &mut deftypes) {
        eprintln!("Error while typechecking: {}", e);
        std::process::exit(2);
    }

    let llvm_context = inkwell::context::Context::create();
    let mut codegen = codegen::CodeGen::new(&llvm_context, "stdin");
    if let Err(e) = codegen.gen_code(&prog) {
        eprintln!("Error while generating code: {}", e);
        std::process::exit(3);
    }

    if matches.is_present("optimize") && !run_optimizations(&codegen.module) {
        eprintln!("No optimizations applied :(");
    }

    if matches.is_present("dump") {
        codegen.dump_to_stdout();
    }

    if let Err(e) = codegen.module.verify() {
        eprintln!("LLVM-BitCode-Error: {}", e.to_string());
        std::process::exit(4);
    }

    if let Some(path) = matches.value_of("output") {
        codegen.module.write_bitcode_to_path(std::path::Path::new(path));
    }

    if matches.is_present("jit") {
        let res = run_jit(&codegen.module, &vartypes, &deftypes);
        if matches.is_present("dump") {
            eprintln!("-> {}", res);
        }

        std::process::exit(res.try_into().unwrap_or(1));
    }
}
