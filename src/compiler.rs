use crate::{
    Chunk,
    OpCode::Constant,
    Value::{Boolean, Float, Integer, String},
    errors::CompileError,
    parser::Expr,
};

// todo if there are more states to be stored in the compiler later on
// but otherwise, this could be a function instead of a struct
pub struct Compiler {}

impl Compiler {
    pub fn new() -> Self {
        Self {}
    }

    pub fn compile(&mut self, expressions: &[Expr]) -> Result<Chunk, CompileError> {
        let mut chunk = Chunk::default();
        for expr in expressions {
            self.compile_expr(expr, &mut chunk)?;
        }
        Ok(chunk)
    }

    fn compile_expr(&mut self, expr: &Expr, chunk: &mut Chunk) -> Result<(), CompileError> {
        match expr {
            // get local / global variables
            Expr::Symbol(_) => Err(CompileError::UnknownSymbol),
            Expr::Integer(i) => {
                let index = chunk.add_constant(Integer(*i));
                chunk.write(Constant(index));
                Ok(())
            }
            Expr::Float(f) => {
                let index = chunk.add_constant(Float(*f));
                chunk.write(Constant(index));
                Ok(())
            }
            Expr::Boolean(b) => {
                let index = chunk.add_constant(Boolean(*b));
                chunk.write(Constant(index));
                Ok(())
            }
            Expr::String(s) => {
                // originally s is borrowed, we cant move it out of the reference
                // so we can just clone it to own it
                let s = s.clone();
                let index = chunk.add_constant(String(s));
                chunk.write(Constant(index));
                Ok(())
            }
            Expr::List(exprs) => {
                let (head, arguments) = exprs.split_first().ok_or(CompileError::EmptyList)?;

                match head {
                    Expr::Symbol(name) if name == "+" => {
                        if arguments.len() != 2 {
                            return Err(CompileError::IncorrectArgumentCount);
                        }
                        for argument in arguments {
                            self.compile_expr(argument, chunk)?;
                        }
                        chunk.write(crate::OpCode::Add);
                    }
                    _ => todo!(),
                }
                Ok(())
            }
        }
    }
}
