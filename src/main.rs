use crate::parser::Parser;

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

fn main() -> miette::Result<()> {
    let source = "(defun add (a b) (+ a b))".to_string();
    let invalid_source = "(defun add (a b) (+ a; b))".to_string();

    let mut parser = Parser::new(&source);
    while parser.has_more() {
        match parser.parse_expr() {
            Ok(expr) => println!("Parsed: {expr:#?}"),
            // first error instant break
            // TODO: maybe in the future can do some sort of recovery
            Err(e) => {
                println!("{:?}", miette::Report::new(e));
                break;
            }
        }
    }

    Ok(())
}
