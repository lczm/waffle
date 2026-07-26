use crate::bytecode::print_chunk;
use crate::compiler::Compiler;
use crate::parser::Parser;
use crate::vm::VM;

mod bytecode;
mod compiler;
mod errors;
mod lexer;
mod parser;
mod vm;

pub fn interpret(source: &str) -> miette::Result<String> {
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
    print_chunk(&chunk);

    let mut vm = VM::new();
    let result = vm.eval(&chunk).map_err(miette::Report::new)?;

    println!("{result}");

    Ok(result.to_string())
}
