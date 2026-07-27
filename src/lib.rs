use crate::compiler::Compiler;
use crate::parser::Parser;

mod bytecode;
mod compiler;
mod errors;
mod lexer;
mod parser;
mod vm;

pub use vm::VM;

pub fn interpret(vm: &mut VM, source: &str) -> miette::Result<String> {
    let mut parser = Parser::new(source);
    // a program is a list of expressions
    // parse each of them at once
    let expressions = parser.parse_program().map_err(miette::Report::new)?;

    // for expression in &expressions {
    //     println!("Parsed: {expression:#?}");
    // }

    let mut compiler = Compiler::new();
    let chunk = compiler
        .compile(&expressions)
        .map_err(miette::Report::new)?;
    // print_chunk(&chunk);

    let result = vm.eval(&chunk).map_err(miette::Report::new)?;

    Ok(result.to_string())
}
