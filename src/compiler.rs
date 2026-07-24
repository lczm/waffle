use crate::{
    Chunk,
    OpCode::Constant,
    Value::{Boolean, Float, Integer, String},
    parser::Expr,
};

// todo if there are more states to be stored in the compiler later on
// but otherwise, this could be a function instead of a struct
pub struct Compiler {}

impl Compiler {
    pub fn new() -> Self {
        Self {}
    }

    pub fn compile(&mut self, expressions: &[Expr]) -> Chunk {
        let mut chunk = Chunk::default();
        for expr in expressions {
            self.compile_expr(expr, &mut chunk);
        }
        chunk
    }

    fn compile_expr(&mut self, expr: &Expr, chunk: &mut Chunk) {
        match expr {
            Expr::Symbol(_) => todo!(),
            Expr::Integer(i) => {
                let index = chunk.add_constant(Integer(*i));
                chunk.write(Constant(index));
            }
            Expr::Float(f) => {
                let index = chunk.add_constant(Float(*f));
                chunk.write(Constant(index));
            }
            Expr::Boolean(b) => {
                let index = chunk.add_constant(Boolean(*b));
                chunk.write(Constant(index));
            }
            Expr::String(s) => {
                // originally s is borrowed, we cant move it out of the reference
                // so we can just clone it to own it
                let s = s.clone();
                let index = chunk.add_constant(String(s));
                chunk.write(Constant(index));
            }
            Expr::List(exprs) => todo!(),
        }
    }
}
