use std::fmt::{Debug, Display};

use crate::callable::Callable;

#[derive(Debug, Clone)]
pub enum Literal {
    String(String),
    Number(f64),
    True,
    False,
    Nil,
}

#[derive(Debug, Clone)]
pub enum Type {
    String(String),
    Number(f64),
    Boolean(bool),
    Callable(Box<dyn Callable>),
    Nil,
}

impl Type {
    pub fn is_truthy(&self) -> bool {
        match self {
            Type::Nil => false,
            Type::Boolean(b) => *b,
            _ => true,
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Nil => write!(f, "nil"),
            Self::Boolean(b) => write!(f, "{}", b),
            Self::String(s) => write!(f, "{}", s),
            Self::Number(n) => write!(f, "{}", n),
            Self::Callable(c) => write!(f, "{}", c),
        }
    }
}
