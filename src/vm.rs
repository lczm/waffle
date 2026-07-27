use std::collections::HashMap;

use crate::{bytecode::Chunk, bytecode::OpCode, bytecode::Value, errors::RuntimeError};

pub struct VM {
    stack: Vec<Value>,
    globals: HashMap<String, Value>,
}

impl VM {
    pub fn new() -> Self {
        Self {
            stack: Vec::new(),
            globals: HashMap::new(),
        }
    }

    pub fn eval(&mut self, chunk: &Chunk) -> Result<Value, RuntimeError> {
        // original stack length
        let original_stack_length = self.stack.len();
        let result = self.run(chunk);
        // whatever the result is, before we return the Result<>
        // we truncate the entire stack back to the original length, to remove all leftovers
        self.stack.truncate(original_stack_length);
        result
    }

    fn run(&mut self, chunk: &Chunk) -> Result<Value, RuntimeError> {
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
                        (Value::Integer(lhs), Value::Integer(rhs)) => {
                            let result =
                                lhs.checked_add(rhs).ok_or(RuntimeError::IntegerOverflow {
                                    operator: "+".into(),
                                    lhs,
                                    rhs,
                                })?;
                            Value::Integer(result)
                        }
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
                        (Value::Integer(lhs), Value::Integer(rhs)) => {
                            let result =
                                lhs.checked_sub(rhs).ok_or(RuntimeError::IntegerOverflow {
                                    operator: "-".into(),
                                    lhs,
                                    rhs,
                                })?;
                            Value::Integer(result)
                        }
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
                        (Value::Integer(lhs), Value::Integer(rhs)) => {
                            let result =
                                lhs.checked_mul(rhs).ok_or(RuntimeError::IntegerOverflow {
                                    operator: "*".into(),
                                    lhs,
                                    rhs,
                                })?;
                            Value::Integer(result)
                        }
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
                        (Value::Integer(lhs), Value::Integer(rhs)) => {
                            let result =
                                lhs.checked_div(rhs).ok_or(RuntimeError::IntegerOverflow {
                                    operator: "/".into(),
                                    lhs,
                                    rhs,
                                })?;
                            Value::Integer(result)
                        }
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
                OpCode::DefineGlobal(name) => {
                    let value = self.stack.pop().ok_or(RuntimeError::StackPopEmpty)?;
                    self.globals.insert(name.into(), value);
                    // always push unit onto the stack so that if its the last, the vm pops off unit
                    self.stack.push(Value::Unit);
                }
                OpCode::GetGlobal(name) => {
                    let value = self.globals.get(name).cloned().ok_or_else(|| {
                        RuntimeError::GlobalVariableDoesNotExist { key: name.into() }
                    })?;
                    self.stack.push(value);
                }
                OpCode::Pop => {
                    self.stack.pop().ok_or(RuntimeError::StackPopEmpty)?;
                }
                OpCode::Print => todo!(),
            }
        }

        let top = self.stack.pop().ok_or(RuntimeError::StackPopEmpty)?;
        Ok(top)
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        bytecode::{Chunk, Value},
        compiler::Compiler,
        errors::RuntimeError,
        parser::{Expr, Parser},
        vm::VM,
    };

    fn parse_source(source: &str) -> Vec<Expr> {
        let mut parser = Parser::new(source);
        parser.parse_program().unwrap()
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
    fn eval_discards_non_final_top_level_values() {
        let chunk = compile_source("1 2");
        let mut vm = VM::new();

        // pops and returns 2
        assert_eq!(vm.eval(&chunk).unwrap(), Value::Integer(2));
        // and that there isnt a leftover 1 left within the stack
        assert!(vm.stack.is_empty());
    }

    #[test]
    fn eval_simple_sub() {
        let source = "(- 10 2)";
        assert_eq!(eval_source(source).unwrap(), Value::Integer(8))
    }

    #[test]
    fn eval_simple_sub_with_negative_result() {
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

    #[test]
    fn eval_restores_stack_after_runtime_error() {
        let source = "(+ 1 (/ 2 0))";
        let chunk = compile_source(source);
        let mut vm = VM::new();
        let error = vm.eval(&chunk).unwrap_err();

        // the stack must be empty, after division by zero
        assert!(vm.stack.is_empty());
        assert_eq!(
            error,
            RuntimeError::DivideByZero {
                numerator: Value::Integer(2)
            }
        )
    }

    #[test]
    fn errors_on_integer_addition_overflow() {
        let source = format!("(+ {} 1)", i64::MAX);
        let error = eval_source(&source).unwrap_err();
        assert_eq!(
            error,
            RuntimeError::IntegerOverflow {
                operator: "+".into(),
                lhs: i64::MAX,
                rhs: 1,
            }
        );
    }
}
