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

impl Value {
    pub fn type_name(&self) -> &'static str {
        match self {
            Value::Integer(_) => "integer",
            Value::Float(_) => "float",
            Value::Boolean(_) => "boolean",
            Value::String(_) => "string",
            Value::Unit => "unit",
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

pub fn print_chunk(chunk: &Chunk) {
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
