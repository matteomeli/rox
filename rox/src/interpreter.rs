use crate::{
    ast::{Binary, Expr, Grouping, Unary, Visitor},
    token::{Token, TokenType},
    types::{Literal, Type},
};

use std::{
    error::Error,
    fmt::{self, Write},
};

#[derive(Default)]
pub struct Interpreter;

pub type InterpretResult = Result<Type, InterpretError>;

impl Interpreter {
    pub fn interpret(&mut self, expr: &Expr) -> InterpretResult {
        self.evaluate(expr)
    }

    fn evaluate(&mut self, expr: &Expr) -> InterpretResult {
        expr.accept(self)
    }
}

impl Visitor for Interpreter {
    type Result = InterpretResult;

    fn visit_literal_expr(&mut self, literal: &Literal) -> Self::Result {
        let result = match literal {
            Literal::False => Type::Boolean(false),
            Literal::True => Type::Boolean(true),
            Literal::Number(n) => Type::Number(*n),
            Literal::String(s) => Type::String(s.clone()),
            Literal::Nil => Type::Nil,
        };

        Ok(result)
    }

    fn visit_grouping_expr(&mut self, grouping: &Grouping) -> Self::Result {
        self.evaluate(&grouping.0)
    }

    fn visit_unary_expr(&mut self, unary: &Unary) -> Self::Result {
        let right = self.evaluate(&unary.1)?;

        match (unary.0.token_type, right) {
            (TokenType::Minus, Type::Number(n)) => Ok(Type::Number(-n)),
            (TokenType::Bang, Type::Nil) | (TokenType::Bang, Type::Boolean(false)) => {
                Ok(Type::Boolean(true))
            }
            (TokenType::Bang, _) => Ok(Type::Boolean(false)),
            _ => Err(InterpretError {
                token: unary.0.clone(),
                message: "Invalid unary expression.".to_string(),
            }),
        }
    }

    fn visit_binary_expr(&mut self, binary: &Binary) -> Self::Result {
        let left = self.evaluate(&binary.0)?;
        let right = self.evaluate(&binary.2)?;

        match (left, binary.1.token_type, right) {
            (Type::Number(ln), op, Type::Number(rn)) => match op {
                TokenType::Plus => Ok(Type::Number(ln + rn)),
                TokenType::Minus => Ok(Type::Number(ln - rn)),
                TokenType::Slash => Ok(Type::Number(ln / rn)),
                TokenType::Star => Ok(Type::Number(ln * rn)),
                TokenType::Greater => Ok(Type::Boolean(ln > rn)),
                TokenType::GreaterEqual => Ok(Type::Boolean(ln >= rn)),
                TokenType::Less => Ok(Type::Boolean(ln < rn)),
                TokenType::LessEqual => Ok(Type::Boolean(ln <= rn)),
                TokenType::EqualEqual => Ok(Type::Boolean(ln == rn)),
                TokenType::NotEqual => Ok(Type::Boolean(ln != rn)),
                _ => Err(InterpretError {
                    token: binary.1.clone(),
                    message: format!("Invalid operands type for {} operator.", binary.1.lexeme),
                }),
            },
            (Type::String(mut ls), TokenType::Plus, Type::String(rs)) => {
                ls.push_str(&rs);
                Ok(Type::String(ls))
            }
            (Type::String(mut s), TokenType::Plus, Type::Number(n))
            | (Type::Number(n), TokenType::Plus, Type::String(mut s)) => {
                let _ = write!(s, "{}", n);
                Ok(Type::String(s))
            }
            (Type::String(ls), op, Type::String(rs)) => match op {
                TokenType::EqualEqual => Ok(Type::Boolean(ls == rs)),
                TokenType::NotEqual => Ok(Type::Boolean(ls != rs)),
                _ => Err(InterpretError {
                    token: binary.1.clone(),
                    message: format!("Cannot use {} operator for string values.", binary.1.lexeme),
                }),
            },
            (Type::Boolean(lb), op, Type::Boolean(rb)) => match op {
                TokenType::EqualEqual => Ok(Type::Boolean(lb == rb)),
                TokenType::NotEqual => Ok(Type::Boolean(lb != rb)),
                _ => Err(InterpretError {
                    token: binary.1.clone(),
                    message: format!(
                        "Cannot use {} operator for boolean values.",
                        binary.1.lexeme
                    ),
                }),
            },
            (Type::Nil, op, Type::Nil) => match op {
                TokenType::EqualEqual => Ok(Type::Boolean(true)),
                TokenType::NotEqual => Ok(Type::Boolean(false)),
                _ => Err(InterpretError {
                    token: binary.1.clone(),
                    message: format!("Cannot use {} operator for nil values.", binary.1.lexeme),
                }),
            },
            (Type::Nil, TokenType::EqualEqual, _) => Ok(Type::Boolean(false)),
            (Type::Nil, TokenType::NotEqual, _) => Ok(Type::Boolean(true)),
            _ => Err(InterpretError {
                token: binary.1.clone(),
                message: "Invalid binary expresion.".to_string(),
            }),
        }
    }
}

#[derive(Debug, Clone)]
pub struct InterpretError {
    token: Token,
    message: String,
}

impl Error for InterpretError {}

impl fmt::Display for InterpretError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "[line {}] Runtime error: {}",
            self.token.line, self.message
        )
    }
}
