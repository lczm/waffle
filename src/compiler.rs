use crate::{
    bytecode::{
        self, Chunk,
        Value::{Boolean, Float, Integer, String},
    },
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
        for (index, expr) in expressions.iter().enumerate() {
            self.compile_expr(expr, &mut chunk)?;

            // if its not the last expression, then emit a pop
            let has_more_expressions = index + 1 < expressions.len();
            if has_more_expressions {
                chunk.write(bytecode::OpCode::Pop);
            }
        }
        Ok(chunk)
    }

    fn compile_expr(&mut self, expr: &Expr, chunk: &mut Chunk) -> Result<(), CompileError> {
        match expr {
            // get local / global variables
            Expr::Symbol(_) => Err(CompileError::UnknownSymbol),
            Expr::Integer(i) => {
                let index = chunk.add_constant(Integer(*i));
                chunk.write(bytecode::OpCode::Constant(index));
                Ok(())
            }
            Expr::Float(f) => {
                let index = chunk.add_constant(Float(*f));
                chunk.write(bytecode::OpCode::Constant(index));
                Ok(())
            }
            Expr::Boolean(b) => {
                let index = chunk.add_constant(Boolean(*b));
                chunk.write(bytecode::OpCode::Constant(index));
                Ok(())
            }
            Expr::String(s) => {
                // originally s is borrowed, we cant move it out of the reference
                // so we can just clone it to own it
                let s = s.clone();
                let index = chunk.add_constant(String(s));
                chunk.write(bytecode::OpCode::Constant(index));
                Ok(())
            }
            Expr::List(exprs) => {
                let (head, arguments) = exprs.split_first().ok_or(CompileError::EmptyList)?;

                match head {
                    Expr::Symbol(operator) => {
                        match operator.as_str() {
                            "+" => {
                                self.compile_binary(arguments, "+", bytecode::OpCode::Add, chunk)?
                            }
                            "-" => {
                                self.compile_binary(arguments, "-", bytecode::OpCode::Sub, chunk)?
                            }
                            "*" => {
                                self.compile_binary(arguments, "*", bytecode::OpCode::Mul, chunk)?
                            }
                            "/" => {
                                self.compile_binary(arguments, "/", bytecode::OpCode::Div, chunk)?
                            }
                            _ => return Err(CompileError::UnknownSymbol),
                        };
                        Ok(())
                    }
                    _ => Err(CompileError::InvalidCallTarget),
                }
            }
        }
    }

    fn compile_binary(
        &mut self,
        arguments: &[Expr],
        operator: &str,
        opcode: bytecode::OpCode,
        chunk: &mut Chunk,
    ) -> Result<(), CompileError> {
        let expected_argument_count = 2;
        let parsed_argument_count = arguments.len();
        if expected_argument_count != parsed_argument_count {
            return Err(CompileError::IncorrectArgumentCount {
                operator: operator.into(),
                expected_count: expected_argument_count,
                parsed_count: parsed_argument_count,
            });
        }
        for argument in arguments {
            self.compile_expr(argument, chunk)?;
        }
        chunk.write(opcode);
        Ok(())
    }

    fn compile_define() {}
}

#[cfg(test)]
mod tests {
    use std::assert_eq;

    use crate::{
        bytecode::{OpCode, Value::Integer},
        errors::CompileError,
        parser::{Expr, Parser},
    };

    use super::Compiler;

    fn parse_source(source: &str) -> Vec<Expr> {
        let mut parser = Parser::new(source);
        parser.parse_program().unwrap()
    }

    #[test]
    fn compiles_simple_add() {
        let source = "(+ 1 2)";
        let mut compiler = Compiler::new();
        let chunk = compiler.compile(&parse_source(source)).unwrap();

        assert_eq!(chunk.constants, vec![Integer(1), Integer(2)]);
        assert!(matches!(
            chunk.code.as_slice(),
            [
                OpCode::Constant(0), // 1
                OpCode::Constant(1), // 2
                OpCode::Add
            ]
        ));
    }

    #[test]
    fn compiles_simple_sub() {
        let source = "(- 10 2)";
        let mut compiler = Compiler::new();
        let chunk = compiler.compile(&parse_source(source)).unwrap();

        assert_eq!(chunk.constants, vec![Integer(10), Integer(2)]);
        assert!(matches!(
            chunk.code.as_slice(),
            [
                OpCode::Constant(0), // 10
                OpCode::Constant(1), // 2
                OpCode::Sub
            ]
        ));
    }

    #[test]
    fn compiles_simple_mul() {
        let source = "(* 10 2)";
        let mut compiler = Compiler::new();
        let chunk = compiler.compile(&parse_source(source)).unwrap();

        assert_eq!(chunk.constants, vec![Integer(10), Integer(2)]);
        assert!(matches!(
            chunk.code.as_slice(),
            [
                OpCode::Constant(0), // 10
                OpCode::Constant(1), // 2
                OpCode::Mul
            ]
        ));
    }

    #[test]
    fn compiles_simple_div() {
        let source = "(/ 10 5)";
        let mut compiler = Compiler::new();
        let chunk = compiler.compile(&parse_source(source)).unwrap();

        assert_eq!(chunk.constants, vec![Integer(10), Integer(5)]);
        assert!(matches!(
            chunk.code.as_slice(),
            [
                OpCode::Constant(0), // 10
                OpCode::Constant(1), // 5
                OpCode::Div
            ]
        ));
    }

    #[test]
    fn compiles_nested_simple_calculations() {
        let source = "(+ 1 (* 2 5))";
        let mut compiler = Compiler::new();
        let chunk = compiler.compile(&parse_source(source)).unwrap();

        assert_eq!(chunk.constants, vec![Integer(1), Integer(2), Integer(5)]);
        assert!(matches!(
            chunk.code.as_slice(),
            [
                OpCode::Constant(0), // 1
                OpCode::Constant(1), // 2
                OpCode::Constant(2), // 5
                // the multiply will do pop 2 * 5 and push 10
                OpCode::Mul,
                // then it'll be 1 + 10
                OpCode::Add
            ]
        ));
    }

    #[test]
    fn error_on_invalid_call_target() {
        let source = "(1 2)";
        let mut compiler = Compiler::new();
        let error = compiler.compile(&parse_source(source)).unwrap_err();
        assert_eq!(error, CompileError::InvalidCallTarget)
    }
}
