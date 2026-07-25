use crate::{bytecode::Chunk, bytecode::OpCode, bytecode::Value, errors::RuntimeError};

pub struct VM {
    stack: Vec<Value>,
}

impl VM {
    pub fn new() -> Self {
        Self { stack: Vec::new() }
    }

    pub fn eval(&mut self, chunk: &Chunk) -> Result<Value, RuntimeError> {
        let mut ip = 0;
        while let Some(opcode) = chunk.code.get(ip) {
            ip += 1;
            match opcode {
                OpCode::Constant(index) => {
                    let value = chunk
                        .constants
                        .get(*index)
                        .ok_or(RuntimeError::InvalidConstantIndex)?;
                    self.stack.push(value.clone());
                }
                OpCode::Add => {
                    let rhs = self.stack.pop().ok_or(RuntimeError::StackPopEmpty)?;
                    let lhs = self.stack.pop().ok_or(RuntimeError::StackPopEmpty)?;
                    let result = match (lhs, rhs) {
                        (Value::Integer(lhs), Value::Integer(rhs)) => Value::Integer(lhs + rhs),
                        (Value::Float(lhs), Value::Float(rhs)) => Value::Float(lhs + rhs),
                        (lhs, rhs) => {
                            return Err(RuntimeError::TypeError {
                                operator: "+".into(),
                                received_type_lhs: lhs.type_name().into(),
                                received_type_rhs: rhs.type_name().into(),
                            });
                        }
                    };
                    self.stack.push(result);
                }
                OpCode::Sub => {
                    let rhs = self.stack.pop().ok_or(RuntimeError::StackPopEmpty)?;
                    let lhs = self.stack.pop().ok_or(RuntimeError::StackPopEmpty)?;
                    let result = match (lhs, rhs) {
                        (Value::Integer(lhs), Value::Integer(rhs)) => Value::Integer(lhs - rhs),
                        (Value::Float(lhs), Value::Float(rhs)) => Value::Float(lhs - rhs),
                        (lhs, rhs) => {
                            return Err(RuntimeError::TypeError {
                                operator: "-".into(),
                                received_type_lhs: lhs.type_name().into(),
                                received_type_rhs: rhs.type_name().into(),
                            });
                        }
                    };
                    self.stack.push(result);
                }
                OpCode::Mul => {
                    let rhs = self.stack.pop().ok_or(RuntimeError::StackPopEmpty)?;
                    let lhs = self.stack.pop().ok_or(RuntimeError::StackPopEmpty)?;
                    let result = match (lhs, rhs) {
                        (Value::Integer(lhs), Value::Integer(rhs)) => Value::Integer(lhs * rhs),
                        (Value::Float(lhs), Value::Float(rhs)) => Value::Float(lhs * rhs),
                        (lhs, rhs) => {
                            return Err(RuntimeError::TypeError {
                                operator: "*".into(),
                                received_type_lhs: lhs.type_name().into(),
                                received_type_rhs: rhs.type_name().into(),
                            });
                        }
                    };
                    self.stack.push(result);
                }
                OpCode::Div => {
                    let rhs = self.stack.pop().ok_or(RuntimeError::StackPopEmpty)?;
                    let lhs = self.stack.pop().ok_or(RuntimeError::StackPopEmpty)?;
                    let result = match (lhs, rhs) {
                        (Value::Integer(lhs), Value::Integer(0)) => {
                            return Err(RuntimeError::DivideByZero {
                                numerator: Value::Integer(lhs),
                            });
                        }
                        (Value::Integer(lhs), Value::Integer(rhs)) => Value::Integer(lhs / rhs),
                        (Value::Float(lhs), Value::Float(0.0)) => {
                            return Err(RuntimeError::DivideByZero {
                                numerator: Value::Float(lhs),
                            });
                        }
                        (Value::Float(lhs), Value::Float(rhs)) => Value::Float(lhs / rhs),
                        (lhs, rhs) => {
                            return Err(RuntimeError::TypeError {
                                operator: "/".into(),
                                received_type_lhs: lhs.type_name().into(),
                                received_type_rhs: rhs.type_name().into(),
                            });
                        }
                    };
                    self.stack.push(result);
                }
                OpCode::Pop => todo!(),
                OpCode::Print => todo!(),
            }
        }

        let top = self.stack.pop().ok_or(RuntimeError::StackPopEmpty)?;
        Ok(top)
    }
}

#[cfg(test)]
mod tests {
    use std::assert_eq;

    use crate::{
        bytecode::{Chunk, Value},
        compiler::Compiler,
        errors::RuntimeError,
        parser::{Expr, Parser},
        vm::VM,
    };

    fn parse_source(source: &str) -> Vec<Expr> {
        let mut parser = Parser::new(source);
        let expressions: Vec<Expr> =
            std::iter::from_fn(|| parser.has_more().then(|| parser.parse_expr()))
                .collect::<Result<Vec<Expr>, _>>()
                .unwrap();
        expressions
    }

    fn compile_source(source: &str) -> Chunk {
        let mut compiler = Compiler::new();
        compiler.compile(&parse_source(source)).unwrap()
    }

    fn eval_source(source: &str) -> Result<Value, RuntimeError> {
        let chunk = compile_source(source);
        let mut vm = VM::new();
        vm.eval(&chunk)
    }

    #[test]
    fn eval_simple_add() {
        let source = "(+ 1 2)";
        assert_eq!(eval_source(source).unwrap(), Value::Integer(3))
    }

    #[test]
    fn eval_simple_sub() {
        let source = "(- 10 2)";
        assert_eq!(eval_source(source).unwrap(), Value::Integer(8))
    }

    #[test]
    fn eval_simple_sub2() {
        let source = "(- 10 100)";
        assert_eq!(eval_source(source).unwrap(), Value::Integer(-90))
    }

    #[test]
    fn eval_simple_mul() {
        let source = "(* 5 5)";
        assert_eq!(eval_source(source).unwrap(), Value::Integer(25))
    }

    #[test]
    fn eval_simple_div() {
        let source = "(/ 10 2)";
        assert_eq!(eval_source(source).unwrap(), Value::Integer(5))
    }

    #[test]
    fn eval_simple_div_by_zero() {
        let source = "(/ 10 0)";
        let error = eval_source(source).unwrap_err();
        assert_eq!(
            error,
            RuntimeError::DivideByZero {
                numerator: Value::Integer(10)
            }
        )
    }
}
