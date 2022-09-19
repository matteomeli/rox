use std::{
    fmt::{Debug, Display},
    time::{SystemTime, UNIX_EPOCH},
};

use crate::{
    ast::FunctionDeclaration,
    class::Instance,
    environment::Environment,
    interpreter::{Interpreter, RuntimeOutcome, RuntimeResult},
    token::{Token, TokenType},
    types::Type,
};

pub trait Callable: CallableClone {
    fn arity(&self) -> usize;
    fn call(&self, interpreter: &mut Interpreter, arguments: Vec<Type>) -> RuntimeResult<Type>;
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

    fn call(&self, _interpreter: &mut Interpreter, _arguments: Vec<Type>) -> RuntimeResult<Type> {
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
        write!(f, "<native fn: clock>")
    }
}

#[derive(Debug, Clone)]
pub struct Function {
    declaration: FunctionDeclaration,
    environment: Environment,
    is_initializer: bool,
}

impl Function {
    pub fn new(
        declaration: FunctionDeclaration,
        environment: Environment,
        is_initializer: bool,
    ) -> Self {
        Function {
            declaration,
            environment,
            is_initializer,
        }
    }

    pub fn bind(&self, class_instance: &Instance) -> Self {
        let mut environment = self.environment.child();
        environment.define(
            "this".to_string(),
            Some(Type::Instance(class_instance.clone())),
        );
        Function::new(self.declaration.clone(), environment, self.is_initializer)
    }
}

impl Callable for Function {
    fn arity(&self) -> usize {
        self.declaration.params.len()
    }

    fn call(&self, interpreter: &mut Interpreter, arguments: Vec<Type>) -> RuntimeResult<Type> {
        let mut environment = self.environment.child();
        for (token, arg) in self.declaration.params.iter().zip(arguments.iter()) {
            environment.define(token.lexeme.clone(), Some(arg.clone()));
        }

        let result = interpreter.execute_block(&self.declaration.body, environment)?;

        // If initializer method, ignore result and always return `this`
        if self.is_initializer {
            return Ok(self
                .environment
                .get_at(
                    0,
                    &Token::new(
                        TokenType::This,
                        "this".to_string(),
                        None,
                        self.declaration.name.line,
                    ),
                )
                .flatten()
                .unwrap_or(Type::Nil));
        } else if let RuntimeOutcome::Return(value) = result {
            return Ok(value);
        }

        Ok(Type::Nil)
    }
}

impl Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<fn: {}>", self.declaration.name.lexeme)
    }
}
