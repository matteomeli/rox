use crate::{
    ast::{Expression, ExpressionVisitor, Statement, StatementVisitor},
    environment::Environment,
    token::{Token, TokenType},
    types::{Literal, Type},
};

use std::{
    error::Error,
    fmt::{self, Write},
};

#[derive(Default)]
pub struct Interpreter {
    environment: Environment,
}

pub type InterpretResult<T> = Result<T, InterpretError>;

impl Interpreter {
    pub fn interpret(&mut self, statements: &[Statement]) -> InterpretResult<()> {
        for statement in statements {
            self.execute(statement)?;
        }

        Ok(())
    }

    fn execute(&mut self, statement: &Statement) -> InterpretResult<()> {
        statement.accept(self)
    }

    fn evaluate(&mut self, expression: &Expression) -> InterpretResult<Type> {
        expression.accept(self)
    }
}

impl ExpressionVisitor for Interpreter {
    type Result = InterpretResult<Type>;

    fn visit_expr(&mut self, expression: &Expression) -> Self::Result {
        match expression {
            Expression::Binary {
                left,
                operator,
                right,
            } => {
                let left = self.evaluate(left)?;
                let right = self.evaluate(right)?;

                match (left, operator.token_type, right) {
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
                            token: operator.clone(),
                            message: format!(
                                "Invalid operands type for {} operator.",
                                operator.lexeme
                            ),
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
                            token: operator.clone(),
                            message: format!(
                                "Cannot use {} operator for string values.",
                                operator.lexeme
                            ),
                        }),
                    },
                    (Type::Boolean(lb), op, Type::Boolean(rb)) => match op {
                        TokenType::EqualEqual => Ok(Type::Boolean(lb == rb)),
                        TokenType::NotEqual => Ok(Type::Boolean(lb != rb)),
                        _ => Err(InterpretError {
                            token: operator.clone(),
                            message: format!(
                                "Cannot use {} operator for boolean values.",
                                operator.lexeme
                            ),
                        }),
                    },
                    (Type::Nil, op, Type::Nil) => match op {
                        TokenType::EqualEqual => Ok(Type::Boolean(true)),
                        TokenType::NotEqual => Ok(Type::Boolean(false)),
                        _ => Err(InterpretError {
                            token: operator.clone(),
                            message: format!(
                                "Cannot use {} operator for nil values.",
                                operator.lexeme
                            ),
                        }),
                    },
                    (Type::Nil, TokenType::EqualEqual, _) => Ok(Type::Boolean(false)),
                    (Type::Nil, TokenType::NotEqual, _) => Ok(Type::Boolean(true)),
                    _ => Err(InterpretError {
                        token: operator.clone(),
                        message: "Invalid binary expresion.".to_string(),
                    }),
                }
            }
            Expression::Grouping { expr } => self.evaluate(expr),
            Expression::Literal { literal } => {
                let result = match literal {
                    Literal::False => Type::Boolean(false),
                    Literal::True => Type::Boolean(true),
                    Literal::Number(n) => Type::Number(*n),
                    Literal::String(s) => Type::String(s.clone()),
                    Literal::Nil => Type::Nil,
                };

                Ok(result)
            }
            Expression::Unary { operator, expr } => {
                let right = self.evaluate(expr)?;

                match (operator.token_type, right) {
                    (TokenType::Minus, Type::Number(n)) => Ok(Type::Number(-n)),
                    (TokenType::Bang, Type::Nil) | (TokenType::Bang, Type::Boolean(false)) => {
                        Ok(Type::Boolean(true))
                    }
                    (TokenType::Bang, _) => Ok(Type::Boolean(false)),
                    _ => Err(InterpretError {
                        token: operator.clone(),
                        message: "Invalid unary expression.".to_string(),
                    }),
                }
            }
            Expression::Variable { name } => match self.environment.get(name) {
                Some(value) => Ok(value),
                None => Err(InterpretError {
                    token: name.clone(),
                    message: format!("Undefined variable '{}'.", &name.lexeme),
                }),
            },
            Expression::Assign { name, value } => {
                let value = self.evaluate(value)?;
                if !self.environment.assign(name, value.clone()) {
                    return Err(InterpretError {
                        token: name.clone(),
                        message: format!("Undefined variable '{}'.", &name.lexeme),
                    });
                }
                Ok(value)
            }
        }
    }
}

impl StatementVisitor for Interpreter {
    type Result = InterpretResult<()>;

    fn visit_stmt(&mut self, statement: &Statement) -> Self::Result {
        match statement {
            Statement::Expression(expression) => {
                let result = self.evaluate(expression)?;
                match result {
                    Type::Nil => println!("nil"),
                    Type::Boolean(b) => println!("{}", b),
                    Type::String(s) => println!("{}", s),
                    Type::Number(n) => println!("{}", n),
                }
                Ok(())
            }
            Statement::Print(expression) => {
                let result = self.evaluate(expression)?;
                match result {
                    Type::Nil => println!("nil"),
                    Type::Boolean(b) => println!("{}", b),
                    Type::String(s) => println!("{}", s),
                    Type::Number(n) => println!("{}", n),
                }

                Ok(())
            }
            Statement::Var { name, initializer } => {
                let value = match initializer {
                    Some(expression) => self.evaluate(expression)?,
                    None => Type::Nil,
                };
                self.environment.define(name.lexeme.clone(), value);

                Ok(())
            }
            Statement::Block { statements } => {
                self.environment.push();
                for statement in statements {
                    self.execute(statement)
                        .inspect_err(|_| self.environment.pop())?;
                }
                self.environment.pop();

                Ok(())
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct InterpretError {
    token: Token,
    message: String, // TODO: Use error kind pattern
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
