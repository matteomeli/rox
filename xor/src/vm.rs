use std::fmt;

use fnv::{FnvHashMap, FnvHashSet};

use crate::{
    chunk::{Chunk, OpCode},
    compiler,
    value::{create_string, InternedString, Value},
};

#[cfg(feature = "trace")]
use crate::debug;

#[derive(Debug, Clone)]
pub enum RuntimeError {
    StackUnderflow,
    TypeError(&'static str, String, bool),
    UnknownOpCode,
    InvalidAddition(String, String),
    UndefinedVariable(String),
}

impl fmt::Display for RuntimeError {
    #[allow(unused_variables)]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::StackUnderflow => write!(f, "Stack underflow."),
            Self::TypeError(expected, actual, is_plural) => {
                #[cfg(not(feature = "lox_errors"))]
                {
                    write!(f, "Expected a {} value but found {}.", t, v)
                }
                #[cfg(feature = "lox_errors")]
                {
                    if *is_plural {
                        write!(f, "Operands must be {}s.", expected)
                    } else {
                        write!(f, "Operand must be a {}.", expected)
                    }
                }
            }
            Self::UnknownOpCode => write!(f, "Unknonw opcode."),
            Self::InvalidAddition(op1, op2) => {
                #[cfg(not(feature = "lox_errors"))]
                {
                    write!(f, "Invalid types for operator +: {}, {}.", op1, op2)
                }
                #[cfg(feature = "lox_errors")]
                {
                    write!(f, "Operands must be two numbers or two strings.")
                }
            }
            Self::UndefinedVariable(name) => write!(f, "Undefined variable '{}'.", name),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum CompileError {
    ParseError,
    TooManyConstants,
    TooManyLocals,
    DuplicateName,
    UninitializedLocal,
    LetReassignment,
    TooManyGlobals,
    TooFarToJump,
    TooFarToLoop,
}

impl fmt::Display for CompileError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::ParseError => write!(f, "Parse error."),
            Self::TooManyConstants => write!(f, "Too many constants in one chunk."),
            Self::TooManyLocals => write!(f, "Too many local variables in a function."),
            Self::DuplicateName => write!(f, "Already a variable with this name in this scope."),
            Self::UninitializedLocal => {
                write!(f, "Can't read local variable in its own initializer.")
            }
            Self::LetReassignment => {
                write!(f, "Can't reassign to a let variable.")
            }
            Self::TooManyGlobals => write!(f, "Too many global variables."),
            Self::TooFarToJump => write!(f, "Too much code to jump over."),
            Self::TooFarToLoop => write!(f, "Loop body too large."),
        }
    }
}

#[derive(Debug, Clone)]
pub enum VMError {
    CompileError(CompileError),
    RuntimeError(RuntimeError),
}

impl fmt::Display for VMError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::CompileError(ce) => write!(f, "{}", ce),
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
    pub(crate) stack: Vec<Value>,
    pub(crate) strings: FnvHashSet<InternedString>,
    pub(crate) globals_indices: FnvHashMap<InternedString, Value>, // Associates an index in 'globals' for each global variable identifier
    pub(crate) globals: Vec<(Value, Value)>, // Packs a (name, value) pair for each global variable
    pub(crate) lets: FnvHashMap<InternedString, bool>, // Stores variables declared by let that can be assigned only once
}

#[allow(dead_code)]
impl VM {
    pub fn interpret(&mut self, source: &str) -> InterpretResult {
        let chunk = compiler::compile(self, source).map_err(VMError::CompileError)?;
        let result = self.run(&chunk);
        if let Err(VMError::RuntimeError(ref re)) = result {
            eprintln!("{}", re);
            if let Some(n) = chunk.get_line(self.ip - 1) {
                eprint!("[line {}] in ", n);
            } else {
                eprint!("[unknown line] in ");
            }
            eprintln!("script");

            self.stack.clear();
        }

        result
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

        self.ip = 0;

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
                debug::disassemble_instruction(self, chunk, self.ip);
            }

            let byte = chunk.code[self.ip];
            self.ip += 1;
            match OpCode::try_from(byte) {
                Ok(instruction) => match instruction {
                    OpCode::DefineGlobal | OpCode::DefineGlobalLong => {
                        let new_value = self.pop_stack()?;
                        let slot = if instruction == OpCode::DefineGlobal {
                            self.read(chunk) as usize
                        } else {
                            self.read_u24(chunk) as usize
                        };
                        let (_, value) = &mut self.globals[slot];
                        *value = new_value;
                    }
                    OpCode::GetGlobal | OpCode::GetGlobalLong => {
                        let slot = if instruction == OpCode::GetGlobal {
                            self.read(chunk) as usize
                        } else {
                            self.read_u24(chunk) as usize
                        };
                        let (name, value) = &self.globals[slot];
                        if let Value::Undefined = value {
                            return rt(RuntimeError::UndefinedVariable(name.clone().try_into()?));
                        }
                        self.stack.push(value.clone())
                    }
                    OpCode::SetGlobal | OpCode::SetGlobalLong => {
                        let new_value = self.peek_stack(0);
                        let slot = if instruction == OpCode::SetGlobal {
                            self.read(chunk) as usize
                        } else {
                            self.read_u24(chunk) as usize
                        };
                        let (name, value) = &mut self.globals[slot];
                        if let Value::Undefined = value {
                            return rt(RuntimeError::UndefinedVariable(name.clone().try_into()?));
                        }
                        *value = new_value;
                    }
                    OpCode::GetLocal | OpCode::GetLocalLong => {
                        let slot = if instruction == OpCode::GetLocal {
                            self.read(chunk) as usize
                        } else {
                            self.read_u24(chunk) as usize
                        };
                        self.stack.push(self.stack[slot as usize].clone());
                    }
                    OpCode::SetLocal | OpCode::SetLocalLong => {
                        let slot = if instruction == OpCode::SetLocal {
                            self.read(chunk) as usize
                        } else {
                            self.read_u24(chunk) as usize
                        };
                        self.stack[slot as usize] = self.peek_stack(0);
                    }
                    OpCode::Pop => {
                        self.pop_stack()?;
                    }
                    OpCode::Print => {
                        let value = self.pop_stack()?;
                        println!("{}", value);
                    }
                    OpCode::Return => {
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
                    OpCode::Nil => self.stack.push(Value::Nil),
                    OpCode::True => self.stack.push(Value::Bool(true)),
                    OpCode::False => self.stack.push(Value::Bool(false)),
                    OpCode::Not => {
                        // TODO: Optimise like NEGATE, removing one pop and apply the operation in place
                        let b = self.pop_stack()?.is_falsey();
                        self.stack.push(Value::Bool(b));
                    }
                    OpCode::Equal => {
                        let a = self.pop_stack()?;
                        let b = self.pop_stack()?;
                        self.stack.push((a == b).into())
                    }
                    OpCode::Greater => binary_op!(>),
                    OpCode::Less => binary_op!(<),
                    OpCode::Negate => {
                        // Negate the value in place without popping/pushing the values stack
                        let value = self
                            .stack
                            .last_mut()
                            .ok_or(VMError::RuntimeError(RuntimeError::StackUnderflow))?;
                        #[cfg(not(feature = "lox_errors"))]
                        {
                            let n: f64 = -value.clone().try_into()?;
                            *value = Value::Number(n);
                        }

                        #[cfg(feature = "lox_errors")]
                        {
                            let n: f64 = value.clone().try_into().map_err(|vme| match vme {
                                VMError::RuntimeError(RuntimeError::TypeError(
                                    expected,
                                    actual,
                                    true,
                                )) => VMError::RuntimeError(RuntimeError::TypeError(
                                    expected, actual, false,
                                )),
                                _ => vme,
                            })?;
                            *value = Value::Number(-n);
                        }
                    }
                    OpCode::Add => {
                        let b = self.pop_stack()?;
                        let a = self.pop_stack()?;
                        match (&b, &a) {
                            (Value::Number(b), Value::Number(a)) => self.stack.push((a + b).into()),
                            (Value::String(b), Value::String(a)) => {
                                let a = &a.upgrade().unwrap().content;
                                let b = &b.upgrade().unwrap().content;
                                let s = create_string(self, &format!("{}{}", a, b));
                                self.stack.push(s.into())
                            }
                            _ => {
                                return rt(RuntimeError::InvalidAddition(
                                    a.to_string(),
                                    b.to_string(),
                                ))
                            }
                        }
                    }
                    OpCode::Subtract => binary_op!(-),
                    OpCode::Multiply => binary_op!(*),
                    OpCode::Divide => binary_op!(/),
                    OpCode::JumpIfFalse => {
                        let offset = self.read_short(chunk) as usize;
                        if self.peek_stack(0).is_falsey() {
                            self.ip += offset;
                        }
                    }
                    OpCode::Jump => {
                        let offset = self.read_short(chunk) as usize;
                        self.ip += offset;
                    }
                    OpCode::Loop => {
                        let offset = self.read_short(chunk) as usize;
                        self.ip -= offset;
                    }
                },
                Err(_) => return rt(RuntimeError::UnknownOpCode),
            }
        }
    }

    fn peek_stack(&self, distance: usize) -> Value {
        // TODO: Handle possible index out of bounds access here
        self.stack[self.stack.len() - 1 - distance].clone()
    }

    fn pop_stack(&mut self) -> ValueResult {
        match self.stack.pop() {
            Some(v) => Ok(v),
            None => Err(VMError::RuntimeError(RuntimeError::StackUnderflow)),
        }
    }

    fn read_constant(&mut self, chunk: &Chunk) -> Value {
        let slot = chunk.code[self.ip];
        self.ip += 1;
        chunk.constants[slot as usize].clone()
    }

    fn read_constant_long(&mut self, chunk: &Chunk) -> Value {
        let slot = u32::from_le_bytes([
            chunk.code[self.ip],
            chunk.code[self.ip + 1],
            chunk.code[self.ip + 2],
            0,
        ]);
        self.ip += 3;
        chunk.constants[slot as usize].clone()
    }

    fn read(&mut self, chunk: &Chunk) -> u8 {
        let byte = chunk.code[self.ip];
        self.ip += 1;
        byte
    }

    fn read_short(&mut self, chunk: &Chunk) -> u16 {
        let slot = u16::from_le_bytes([chunk.code[self.ip], chunk.code[self.ip + 1]]);
        self.ip += 2;
        slot
    }

    fn read_u24(&mut self, chunk: &Chunk) -> u32 {
        let slot = u32::from_le_bytes([
            chunk.code[self.ip],
            chunk.code[self.ip + 1],
            chunk.code[self.ip + 2],
            0,
        ]);
        self.ip += 3;
        slot
    }
}
