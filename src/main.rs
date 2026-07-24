use crate::compiler::Compiler;
use crate::parser::{Expr, Parser};

mod compiler;
mod errors;
mod lexer;
mod parser;

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Integer(i64),
    Float(f64),
    Boolean(bool),
    String(String),
    Unit,
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Integer(i) => write!(f, "{i}"),
            Value::Float(n) => write!(f, "{n}"),
            Value::Boolean(b) => write!(f, "{b}"),
            Value::String(s) => write!(f, "\"{s}\""),
            Value::Unit => write!(f, "()"),
        }
    }
}

#[derive(Debug, Clone)]
pub enum OpCode {
    Constant(usize),
    // basic operators
    Add,
    Sub,
    Mul,
    Div,
    // for the vm stack
    Pop,
    // convenience
    Print,
}

// a chunk holds a list of the opcodes and the values
#[derive(Debug, Default)]
pub struct Chunk {
    pub code: Vec<OpCode>,
    pub constants: Vec<Value>,
}

impl Chunk {
    pub fn write(&mut self, op: OpCode) {
        self.code.push(op);
    }

    // when adding a constant via this method
    // get back the index where the value lives
    pub fn add_constant(&mut self, value: Value) -> usize {
        self.constants.push(value);
        self.constants.len() - 1
    }
}

// a function contains a chunk and various information about the function
// like the name and number of arguments
pub struct Function {
    pub name: String,
    pub arity: usize,
    pub chunk: Chunk,
}

// a file compiles to a module, which is just a list of functions
// and some entry point
pub struct Module {
    pub functions: Vec<Function>,
    // index into functions for where the program starts
    pub entry_point: usize,
}

fn print_chunk(chunk: &Chunk) {
    println!("== bytecode ==");

    for (chunk_index, opcode) in chunk.code.iter().enumerate() {
        print!("{chunk_index:04} ");

        match opcode {
            OpCode::Constant(constants_index) => match chunk.constants.get(*constants_index) {
                Some(value) => println!("CONSTANT {constants_index:04} {value}"),
                None => {
                    println!("CONSTANT {constants_index:04} <invalid constant constants_index>")
                }
            },
            OpCode::Add => println!("ADD"),
            OpCode::Sub => println!("SUB"),
            OpCode::Mul => println!("MUL"),
            OpCode::Div => println!("DIV"),
            OpCode::Pop => println!("POP"),
            OpCode::Print => println!("PRINT"),
        }
    }
}

fn main() -> miette::Result<()> {
    let source = "(+ 1 2)".to_string();
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

    Ok(())
}
