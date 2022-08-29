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

    pub fn interpret_repl(&mut self, statements: &[Statement]) -> InterpretResult<()> {
        if let Some((last, others)) = statements.split_last() {
            self.interpret(others)?;
            match last {
                Statement::Expression(expr) => {
                    let result = self.evaluate(expr)?;
                    match result {
                        Type::Nil => println!("nil"),
                        Type::Boolean(b) => println!("{}", b),
                        Type::String(s) => println!("{}", s),
                        Type::Number(n) => println!("{}", n),
                    }
                }
                _ => {
                    self.execute(last)?;
                }
            }
        }

        Ok(())
    }

    fn execute(&mut self, statement: &Statement) -> InterpretResult<ExecutionResult> {
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
                            kind: InterpretErrorKind::InvalidOperandsForBinaryOpeator,
                        }),
                    },
                    (Type::String(mut ls), op, Type::String(rs)) => match op {
                        TokenType::EqualEqual => Ok(Type::Boolean(ls == rs)),
                        TokenType::NotEqual => Ok(Type::Boolean(ls != rs)),
                        TokenType::Plus => {
                            ls.push_str(&rs);
                            Ok(Type::String(ls))
                        }
                        _ => Err(InterpretError {
                            token: operator.clone(),
                            kind: InterpretErrorKind::InvalidBinaryOperatorForStringOperands,
                        }),
                    },
                    (Type::String(mut s), TokenType::Plus, Type::Number(n))
                    | (Type::Number(n), TokenType::Plus, Type::String(mut s)) => {
                        let _ = write!(s, "{}", n);
                        Ok(Type::String(s))
                    }
                    (Type::Boolean(lb), op, Type::Boolean(rb)) => match op {
                        TokenType::EqualEqual => Ok(Type::Boolean(lb == rb)),
                        TokenType::NotEqual => Ok(Type::Boolean(lb != rb)),
                        _ => Err(InterpretError {
                            token: operator.clone(),
                            kind: InterpretErrorKind::InvalidBinaryOperatorForBooleanOperands,
                        }),
                    },
                    (Type::Nil, op, Type::Nil) => match op {
                        TokenType::EqualEqual => Ok(Type::Boolean(true)),
                        TokenType::NotEqual => Ok(Type::Boolean(false)),
                        _ => Err(InterpretError {
                            token: operator.clone(),
                            kind: InterpretErrorKind::InvalidBinaryOperatorForNilOperands,
                        }),
                    },
                    (Type::Nil, TokenType::EqualEqual, _) => Ok(Type::Boolean(false)),
                    (Type::Nil, TokenType::NotEqual, _) => Ok(Type::Boolean(true)),
                    _ => Err(InterpretError {
                        token: operator.clone(),
                        kind: InterpretErrorKind::InvalidBinaryExpression,
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
            Expression::Logical {
                left,
                operator,
                right,
            } => {
                let left = self.evaluate(left)?;
                match operator.token_type {
                    TokenType::Or if left.is_truthy() => Ok(left),
                    TokenType::And if !left.is_truthy() => Ok(left),
                    _ => self.evaluate(right),
                }
            }
            Expression::Unary { operator, expr } => {
                let right = self.evaluate(expr)?;

                match (operator.token_type, right) {
                    (TokenType::Minus, Type::Number(n)) => Ok(Type::Number(-n)),
                    (TokenType::Bang, value) => Ok(Type::Boolean(!value.is_truthy())),
                    _ => Err(InterpretError {
                        token: operator.clone(),
                        kind: InterpretErrorKind::InvalidUnaryExpression,
                    }),
                }
            }
            Expression::Variable { name } => match self.environment.get(name) {
                Some(Some(value)) => Ok(value),
                Some(None) => Err(InterpretError {
                    token: name.clone(),
                    kind: InterpretErrorKind::UninitializedVariable,
                }),
                None => Err(InterpretError {
                    token: name.clone(),
                    kind: InterpretErrorKind::UninitializedVariable,
                }),
            },
            Expression::Assign { name, value } => {
                let value = self.evaluate(value)?;
                if !self.environment.assign(name, value.clone()) {
                    return Err(InterpretError {
                        token: name.clone(),
                        kind: InterpretErrorKind::UninitializedVariable,
                    });
                }
                Ok(value)
            }
        }
    }
}

impl StatementVisitor for Interpreter {
    type Result = InterpretResult<ExecutionResult>;

    fn visit_stmt(&mut self, statement: &Statement) -> Self::Result {
        match statement {
            Statement::Block { statements } => {
                self.environment.push();
                for statement in statements {
                    match self.execute(statement) {
                        result @ Err(_)
                        | result @ Ok(ExecutionResult::Break)
                        | result @ Ok(ExecutionResult::Continue) => {
                            self.environment.pop();
                            return result;
                        }
                        _ => (),
                    }
                }
                self.environment.pop();

                Ok(ExecutionResult::Success)
            }
            Statement::Break => Ok(ExecutionResult::Break),
            Statement::Continue => Ok(ExecutionResult::Continue),
            Statement::Expression(expression) => {
                self.evaluate(expression)?;
                Ok(ExecutionResult::Success)
            }
            Statement::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let cond = self.evaluate(condition)?;
                if cond.is_truthy() {
                    self.execute(then_branch)
                } else if let Some(else_branch) = else_branch {
                    self.execute(else_branch)
                } else {
                    Ok(ExecutionResult::Success)
                }
            }
            Statement::Print(expression) => {
                let result = self.evaluate(expression)?;
                match result {
                    Type::Nil => println!("nil"),
                    Type::Boolean(b) => println!("{}", b),
                    Type::String(s) => println!("{}", s),
                    Type::Number(n) => println!("{}", n),
                }

                Ok(ExecutionResult::Success)
            }
            Statement::Var { name, initializer } => {
                let value = match initializer {
                    Some(expression) => Some(self.evaluate(expression)?),
                    None => None,
                };
                self.environment.define(name.lexeme.clone(), value);

                Ok(ExecutionResult::Success)
            }
            Statement::While { condition, body } => {
                while self.evaluate(condition)?.is_truthy() {
                    if let Statement::Block { ref statements } = body.as_ref() {
                        match &statements[..] {
                            // For loops are desugared by the [Parser] as while loops containing an "artificial" enclosing block
                            // which in turn contains two statements: the original for-loop's block plus the increment statement, if any.
                            // This structure is important to consider when implementing `continue` as the increment statememnt has to always run,
                            // even when one of the statements in the for-loop's (desugared as while) block requests to continue to
                            // the next iteration and skip the remainder statements of this one. Below is where this special case is handled.
                            [block @ Statement::Block { .. }, increment @ Statement::Expression(_)] =>
                            {
                                let result = self.execute(block)?;
                                // When executing `block`, any statemement can produce a `continue` or a `break`,
                                // skipping any statement that would follow in the block.
                                // We want to make sure the increment statement is always executed,
                                // so `continue` can safely jump to the next iteration.
                                self.execute(increment)?;
                                // In case of `break` was produced we instead exit the whole while loop.
                                if result == ExecutionResult::Break {
                                    break;
                                }
                            }
                            _ => {
                                let res = self.execute(body)?;
                                if res == ExecutionResult::Break {
                                    break;
                                }
                            }
                        }
                    } else {
                        self.execute(body)?;
                    }
                }

                Ok(ExecutionResult::Success)
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ExecutionResult {
    Break,
    Continue,
    Success,
}

#[derive(Debug, Clone)]
pub struct InterpretError {
    token: Token,
    kind: InterpretErrorKind,
}

impl Error for InterpretError {}

impl fmt::Display for InterpretError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.kind {
            InterpretErrorKind::InvalidOperandsForBinaryOpeator => write!(
                f,
                "[line {}] Runtime error: Invalid operands for binary operator '{}'.",
                self.token.line, self.token.lexeme
            ),
            InterpretErrorKind::InvalidBinaryOperatorForStringOperands => write!(
                f,
                "[line {}] Runtime error: Cannot use operator '{}' for string operands.",
                self.token.line, self.token.lexeme
            ),
            InterpretErrorKind::InvalidBinaryOperatorForBooleanOperands => write!(
                f,
                "[line {}] Runtime error: Cannot use operator '{}' for boolean operands.",
                self.token.line, self.token.lexeme
            ),
            InterpretErrorKind::InvalidBinaryOperatorForNilOperands => write!(
                f,
                "[line {}] Runtime error: Cannot use operator '{}' for nil operands.",
                self.token.line, self.token.lexeme
            ),
            InterpretErrorKind::InvalidBinaryExpression => write!(
                f,
                "[line {}] Runtime error: Invalid binary expression '{}'.",
                self.token.line, self.token.lexeme
            ),
            InterpretErrorKind::InvalidUnaryExpression => write!(
                f,
                "[line {}] Runtime error: Invalid unary expression '{}'.",
                self.token.line, self.token.lexeme
            ),
            InterpretErrorKind::UndefinedVariable => write!(
                f,
                "[line {}] Runtime error: Undefined variable '{}'.",
                self.token.line, self.token.lexeme
            ),
            InterpretErrorKind::UninitializedVariable => write!(
                f,
                "[line {}] Runtime error: Variable '{}' must be initialized before use.",
                self.token.line, self.token.lexeme
            ),
        }
    }
}

#[derive(Debug, Clone)]
pub enum InterpretErrorKind {
    InvalidOperandsForBinaryOpeator,
    InvalidBinaryOperatorForStringOperands,
    InvalidBinaryOperatorForBooleanOperands,
    InvalidBinaryOperatorForNilOperands,
    InvalidBinaryExpression,
    InvalidUnaryExpression,
    UndefinedVariable,
    UninitializedVariable,
}
