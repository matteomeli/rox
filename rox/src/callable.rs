use std::{
    fmt::{Debug, Display},
    time::{SystemTime, UNIX_EPOCH},
};

use crate::{
    ast::Statement,
    environment::Environment,
    interpreter::{ExecutionResult, InterpretResult, Interpreter},
    token::Token,
    types::Type,
};

pub trait Callable: CallableClone {
    fn arity(&self) -> usize;
    fn call(&self, _interpreter: &mut Interpreter, _arguments: Vec<Type>) -> InterpretResult<Type>;
}

pub trait CallableClone: Debug + Display {
    fn callable_clone(&self) -> Box<dyn Callable>;
}

impl Clone for Box<dyn Callable> {
    fn clone(&self) -> Self {
        self.callable_clone()
    }
}

impl<T> CallableClone for T
where
    T: 'static + Callable + Clone,
{
    fn callable_clone(&self) -> Box<dyn Callable> {
        Box::new(self.clone())
    }
}

#[derive(Debug, Clone)]
pub struct Clock;

impl Callable for Clock {
    fn arity(&self) -> usize {
        0
    }

    fn call(&self, _interpreter: &mut Interpreter, _arguments: Vec<Type>) -> InterpretResult<Type> {
        let start = SystemTime::now();
        let since_epoch = start
            .duration_since(UNIX_EPOCH)
            .expect("Time went backwards")
            .as_secs();
        Ok(Type::Number(since_epoch as f64))
    }
}

impl Display for Clock {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<native fn clock>")
    }
}

#[derive(Debug, Clone)]
pub struct Function {
    name: Token,
    params: Vec<Token>,
    body: Vec<Statement>,
    closure: Environment,
}

impl Function {
    pub fn new(
        name: Token,
        params: Vec<Token>,
        body: Vec<Statement>,
        closure: Environment,
    ) -> Self {
        Function {
            name,
            params,
            body,
            closure,
        }
    }
}

impl Callable for Function {
    fn arity(&self) -> usize {
        self.params.len()
    }

    fn call(&self, interpreter: &mut Interpreter, arguments: Vec<Type>) -> InterpretResult<Type> {
        let mut environment = self.closure.child();
        for (token, arg) in self.params.iter().zip(arguments.iter()) {
            environment.define(token.lexeme.clone(), Some(arg.clone()));
        }

        if let ExecutionResult::Return(value) =
            interpreter.execute_block(&self.body, environment)?
        {
            return Ok(value);
        }

        Ok(Type::Nil)
    }
}

impl Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<fn {}>", self.name.lexeme)
    }
}
