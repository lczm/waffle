use crate::{Chunk, parser::Expr};

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

    fn compile_expr(&mut self, expr: &Expr, chunk: &mut Chunk) {}
}
