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
