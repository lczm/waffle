use crate::parser::{Expr, Parser};

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

pub struct Function {
    pub name: String,
    pub arity: usize,
    pub chunk: Chunk,
}

fn main() -> miette::Result<()> {
    let source = "(defun add (a b) (+ a b))".to_string();
    // let invalid_source = "(defun add (a b) (+ a; b))".to_string();

    let mut parser = Parser::new(&source);
    // a program is a list of expressions
    // parse each of them at once
    let expressions: Vec<Expr> =
        std::iter::from_fn(|| parser.has_more().then(|| parser.parse_expr()))
            .collect::<Result<Vec<Expr>, _>>()
            .map_err(miette::Report::new)?;

    for expression in expressions {
        println!("Parsed: {expression:#?}");
    }

    Ok(())
}
