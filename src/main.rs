use crate::bytecode::print_chunk;
use crate::compiler::Compiler;
use crate::parser::{Expr, Parser};
use crate::vm::VM;

mod bytecode;
mod compiler;
mod errors;
mod lexer;
mod parser;
mod vm;

fn main() -> miette::Result<()> {
    // let source = "(+ 1 (* 2 5))".to_string();
    let source = "(+ 1 2.0)".to_string();
    // let function_source = "(defun add (a b) (+ a b))".to_string();
    // let function_invalid_source = "(defun add (a b) (+ a; b))".to_string();

    let mut parser = Parser::new(&source);
    // a program is a list of expressions
    // parse each of them at once
    let expressions: Vec<Expr> =
        std::iter::from_fn(|| parser.has_more().then(|| parser.parse_expr()))
            .collect::<Result<Vec<Expr>, _>>()
            .map_err(miette::Report::new)?;

    // for expression in expressions {
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

    Ok(())
}
