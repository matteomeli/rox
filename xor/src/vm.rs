use std::fmt;

use crate::{
    chunk::{Chunk, OpCode},
    compiler,
    value::Value,
};

#[cfg(feature = "trace")]
use crate::debug;

#[derive(Debug, Clone)]
pub enum RuntimeError {
    StackUnderflow,
    TypeError(&'static str, String),
    UnknownOpCode,
}

impl fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::StackUnderflow => write!(f, "Stack underflow."),
            Self::TypeError(t, v) => write!(f, "Expected a {} value but found {}.", t, v),
            Self::UnknownOpCode => write!(f, "Unknonw opcode."),
        }
    }
}

#[derive(Debug, Clone)]
pub enum VMError {
    CompileError,
    RuntimeError(RuntimeError),
}

impl fmt::Display for VMError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::CompileError => write!(f, "Compiler error."),
            Self::RuntimeError(re) => write!(f, "{}", re),
        }
    }
}

type ValueResult = Result<Value, VMError>;
type InterpretResult = Result<(), VMError>;

fn rt(re: RuntimeError) -> InterpretResult {
    Err(VMError::RuntimeError(re))
}

#[derive(Default)]
pub struct VM {
    ip: usize,
    stack: Vec<Value>,
}

impl VM {
    pub fn interpret(&mut self, source: &str) -> InterpretResult {
        compiler::compile(source);
        Ok(())
    }

    #[allow(clippy::single_match)]
    fn run(&mut self, chunk: &Chunk) -> InterpretResult {
        macro_rules! binary_op {
            ($op:tt) => {{
                // TODO: Optimise like NEGATE, removing one pop and apply the operation in place
                let b: f64 = self.pop_stack()?.try_into()?;
                let a: f64 = self.pop_stack()?.try_into()?;
                self.stack.push((a $op b).into());
            }};
        }

        #[cfg(feature = "trace")]
        {
            println!("Execution trace:");
        }

        loop {
            #[cfg(feature = "trace")]
            {
                print!("          ");
                if self.stack.is_empty() {
                    print!("<empty>")
                } else {
                    for v in &self.stack {
                        print!("[ {} ]", v);
                    }
                }
                println!();
                debug::disassemble_instruction(chunk, self.ip);
            }

            let byte = chunk.code[self.ip];
            self.ip += 1;
            match OpCode::try_from(byte) {
                Ok(instruction) => match instruction {
                    OpCode::Return => {
                        let value = self.pop_stack()?;
                        println!("{}", value);
                        return Ok(());
                    }
                    OpCode::Constant => {
                        let constant = self.read_constant(chunk);
                        self.stack.push(constant);
                    }
                    OpCode::ConstantLong => {
                        let constant = self.read_constant_long(chunk);
                        self.stack.push(constant);
                    }
                    OpCode::Negate => {
                        //let n: f64 = self.pop_stack()?.try_into()?;
                        //self.stack.push((-n).into());
                        // Negate the value in place without popping/pushing the values stack
                        let value = self
                            .stack
                            .last_mut()
                            .ok_or(VMError::RuntimeError(RuntimeError::StackUnderflow))?;
                        *value = Value::Number(-value.clone().try_into()?);
                    }
                    OpCode::Add => binary_op!(+),
                    OpCode::Subtract => binary_op!(-),
                    OpCode::Multiply => binary_op!(*),
                    OpCode::Divide => binary_op!(/),
                },
                Err(_) => return rt(RuntimeError::UnknownOpCode),
            }
        }
    }

    fn pop_stack(&mut self) -> ValueResult {
        match self.stack.pop() {
            Some(v) => Ok(v),
            None => Err(VMError::RuntimeError(RuntimeError::StackUnderflow)),
        }
    }

    fn read_constant(&mut self, chunk: &Chunk) -> Value {
        let constant_index = chunk.code[self.ip];
        self.ip += 1;
        chunk.constants[constant_index as usize].clone()
    }

    fn read_constant_long(&mut self, chunk: &Chunk) -> Value {
        let constant_index = u32::from_le_bytes([
            chunk.code[self.ip + 1],
            chunk.code[self.ip + 2],
            chunk.code[self.ip + 3],
            0,
        ]);
        self.ip += 3;
        chunk.constants[constant_index as usize].clone()
    }
}
