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
                    // pop the top 2
                    let rhs = self.stack.pop().ok_or(RuntimeError::StackPopEmpty)?;
                    let lhs = self.stack.pop().ok_or(RuntimeError::StackPopEmpty)?;
                    // todo : add a catch all runtimeerror for invalid types
                    // error should show what types they are
                    let result = match (rhs, lhs) {
                        (Value::Integer(rhs), Value::Integer(lhs)) => Value::Integer(lhs + rhs),
                        (Value::Float(rhs), Value::Float(lhs)) => Value::Float(lhs + rhs),
                        _ => todo!(),
                    };
                    self.stack.push(result);
                }
                OpCode::Sub => todo!(),
                OpCode::Mul => todo!(),
                OpCode::Div => todo!(),
                OpCode::Pop => todo!(),
                OpCode::Print => todo!(),
            }
        }

        // todo : the value popped is whatever is left on the stack
        let top = self.stack.pop().ok_or(RuntimeError::StackPopEmpty)?;
        Ok(top)
    }
}
