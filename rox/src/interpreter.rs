use crate::{
    ast::{Expression, ExpressionKind, ExpressionVisitor, Statement, StatementVisitor},
    callable::{Clock, Function},
    environment::Environment,
    token::{Token, TokenType},
    types::{Literal, Type},
};

use std::{
    collections::HashMap,
    error::Error,
    fmt::{self, Write},
};

pub struct Interpreter {
    globals: Environment,
    environment: Environment,
    locals: HashMap<usize, usize>,
}

impl Default for Interpreter {
    fn default() -> Self {
        let mut globals = Environment::default();
        globals.define("clock".to_string(), Some(Type::Callable(Box::new(Clock))));
        let environment = globals.clone();
        Interpreter {
            globals,
            environment,
            locals: HashMap::new(),
        }
    }
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
                    println!("{}", self.evaluate(expr)?);
                }
                _ => {
                    self.execute(last)?;
                }
            }
        }

        Ok(())
    }

    pub fn execute_block(
        &mut self,
        statements: &[Statement],
        mut environment: Environment,
    ) -> InterpretResult<ExecutionResult> {
        std::mem::swap(&mut self.environment, &mut environment);
        for statement in statements {
            match self.execute(statement) {
                result @ Err(_)
                | result @ Ok(ExecutionResult::Break)
                | result @ Ok(ExecutionResult::Continue)
                | result @ Ok(ExecutionResult::Return(_)) => {
                    std::mem::swap(&mut self.environment, &mut environment);
                    return result;
                }
                _ => (),
            }
        }
        std::mem::swap(&mut self.environment, &mut environment);

        Ok(ExecutionResult::Success)
    }

    pub fn resolve(&mut self, expression_id: usize, scope_distance: usize) {
        self.locals.insert(expression_id, scope_distance);
    }

    fn lookup_variable(&self, name: &Token, expression_id: usize) -> Option<Option<Type>> {
        if let Some(distance) = self.locals.get(&expression_id) {
            self.environment.get_at(*distance, name)
        } else {
            self.globals.get(name)
        }
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
            Expression {
                id,
                kind: ExpressionKind::Assign { name, value },
            } => {
                let value = self.evaluate(value)?;

                let success = if let Some(distance) = self.locals.get(id) {
                    self.environment.assign_at(*distance, name, value.clone())
                } else {
                    self.globals.assign(name, value.clone())
                };
                if !success {
                    return Err(InterpretError::new(
                        name.clone(),
                        InterpretErrorKind::UninitializedVariable,
                    ));
                }

                Ok(value)
            }
            Expression {
                kind:
                    ExpressionKind::Binary {
                        left,
                        operator,
                        right,
                    },
                ..
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
                        _ => Err(InterpretError::new(
                            operator.clone(),
                            InterpretErrorKind::InvalidOperandsForBinaryOpeator,
                        )),
                    },
                    (Type::String(mut ls), op, Type::String(rs)) => match op {
                        TokenType::EqualEqual => Ok(Type::Boolean(ls == rs)),
                        TokenType::NotEqual => Ok(Type::Boolean(ls != rs)),
                        TokenType::Plus => {
                            ls.push_str(&rs);
                            Ok(Type::String(ls))
                        }
                        _ => Err(InterpretError::new(
                            operator.clone(),
                            InterpretErrorKind::InvalidBinaryOperatorForStringOperands,
                        )),
                    },
                    (Type::String(mut s), TokenType::Plus, Type::Number(n))
                    | (Type::Number(n), TokenType::Plus, Type::String(mut s)) => {
                        let _ = write!(s, "{}", n);
                        Ok(Type::String(s))
                    }
                    (Type::Boolean(lb), op, Type::Boolean(rb)) => match op {
                        TokenType::EqualEqual => Ok(Type::Boolean(lb == rb)),
                        TokenType::NotEqual => Ok(Type::Boolean(lb != rb)),
                        _ => Err(InterpretError::new(
                            operator.clone(),
                            InterpretErrorKind::InvalidBinaryOperatorForBooleanOperands,
                        )),
                    },
                    (Type::Nil, op, Type::Nil) => match op {
                        TokenType::EqualEqual => Ok(Type::Boolean(true)),
                        TokenType::NotEqual => Ok(Type::Boolean(false)),
                        _ => Err(InterpretError::new(
                            operator.clone(),
                            InterpretErrorKind::InvalidBinaryOperatorForNilOperands,
                        )),
                    },
                    (Type::Nil, TokenType::EqualEqual, _) => Ok(Type::Boolean(false)),
                    (Type::Nil, TokenType::NotEqual, _) => Ok(Type::Boolean(true)),
                    _ => Err(InterpretError::new(
                        operator.clone(),
                        InterpretErrorKind::InvalidBinaryExpression,
                    )),
                }
            }
            Expression {
                kind:
                    ExpressionKind::Call {
                        callee,
                        paren,
                        arguments,
                    },
                ..
            } => {
                let callee = self.evaluate(callee)?;

                let mut args = Vec::new();
                for argument in arguments {
                    args.push(self.evaluate(argument)?);
                }

                if let Type::Callable(callable) = callee {
                    if args.len() != callable.arity() {
                        Err(InterpretError::with_message(
                            paren.clone(),
                            InterpretErrorKind::InvalidCallableArguments,
                            Some(format!(
                                "Expected {} arguments, but got {}.",
                                callable.arity(),
                                args.len()
                            )),
                        ))
                    } else {
                        callable.call(self, args)
                    }
                } else {
                    Err(InterpretError::new(
                        paren.clone(),
                        InterpretErrorKind::InvalidCallable,
                    ))
                }
            }
            Expression {
                kind: ExpressionKind::Grouping { expr },
                ..
            } => self.evaluate(expr),
            Expression {
                kind: ExpressionKind::Literal { literal },
                ..
            } => {
                let result = match literal {
                    Literal::False => Type::Boolean(false),
                    Literal::True => Type::Boolean(true),
                    Literal::Number(n) => Type::Number(*n),
                    Literal::String(s) => Type::String(s.clone()),
                    Literal::Nil => Type::Nil,
                };

                Ok(result)
            }
            Expression {
                kind:
                    ExpressionKind::Logical {
                        left,
                        operator,
                        right,
                    },
                ..
            } => {
                let left = self.evaluate(left)?;
                match operator.token_type {
                    TokenType::Or if left.is_truthy() => Ok(left),
                    TokenType::And if !left.is_truthy() => Ok(left),
                    _ => self.evaluate(right),
                }
            }
            Expression {
                kind: ExpressionKind::Unary { operator, expr },
                ..
            } => {
                let right = self.evaluate(expr)?;

                match (operator.token_type, right) {
                    (TokenType::Minus, Type::Number(n)) => Ok(Type::Number(-n)),
                    (TokenType::Bang, value) => Ok(Type::Boolean(!value.is_truthy())),
                    _ => Err(InterpretError::new(
                        operator.clone(),
                        InterpretErrorKind::InvalidUnaryExpression,
                    )),
                }
            }
            Expression {
                id,
                kind: ExpressionKind::Variable { name },
            } => match self.lookup_variable(name, *id) {
                Some(Some(value)) => Ok(value),
                Some(None) => Err(InterpretError::new(
                    name.clone(),
                    InterpretErrorKind::UninitializedVariable,
                )),
                None => Err(InterpretError::new(
                    name.clone(),
                    InterpretErrorKind::UninitializedVariable,
                )),
            },
        }
    }
}

impl StatementVisitor for Interpreter {
    type Result = InterpretResult<ExecutionResult>;

    fn visit_stmt(&mut self, statement: &Statement) -> Self::Result {
        match statement {
            Statement::Block { statements } => {
                let environment = self.environment.child();
                self.execute_block(statements, environment)
            }
            Statement::Break => Ok(ExecutionResult::Break),
            Statement::Continue => Ok(ExecutionResult::Continue),
            Statement::Expression(expression) => {
                self.evaluate(expression)?;
                Ok(ExecutionResult::Success)
            }
            Statement::Function(declaration) => {
                let function = Function::new(declaration.clone(), self.environment.clone());
                self.environment.define(
                    declaration.name.lexeme.clone(),
                    Some(Type::Callable(Box::new(function))),
                );
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
                println!("{}", self.evaluate(expression)?);
                Ok(ExecutionResult::Success)
            }
            Statement::Return { value, .. } => {
                if let Some(expression) = value {
                    let result = self.evaluate(expression)?;
                    Ok(ExecutionResult::Return(result))
                } else {
                    Ok(ExecutionResult::Return(Type::Nil))
                }
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
                    // Need to handle special case of for loop desugared into while
                    // Desugared for while loop structure is known and can be destructured as below:
                    let result = if let Statement::Block {
                        statements: outer_statements,
                    } = body.as_ref()
                    {
                        // An outer block wraps an inner block (the actual for-loop block) and the increment expression (if any)
                        if let [Statement::Block { statements }, increment @ Statement::Expression(_)] =
                            &outer_statements[..]
                        {
                            let outer_env = self.environment.child();
                            let inner_env = outer_env.child();
                            let result = self.execute_block(statements, inner_env)?;
                            self.execute_block(&[increment.clone()], outer_env)?;
                            result
                        } else {
                            self.execute(body)?
                        }
                    } else {
                        self.execute(body)?
                    };
                    match result {
                        ExecutionResult::Break => break,
                        ExecutionResult::Return(_) => return Ok(result),
                        _ => (),
                    }
                }

                Ok(ExecutionResult::Success)
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum ExecutionResult {
    Break,
    Continue,
    Return(Type),
    Success,
}

#[derive(Debug, Clone)]
pub struct InterpretError {
    token: Token,
    kind: InterpretErrorKind,
    message: Option<String>,
}

impl InterpretError {
    pub fn new(token: Token, kind: InterpretErrorKind) -> Self {
        InterpretError {
            token,
            kind,
            message: None,
        }
    }

    pub fn with_message(token: Token, kind: InterpretErrorKind, message: Option<String>) -> Self {
        InterpretError {
            token,
            kind,
            message,
        }
    }
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
            InterpretErrorKind::InvalidCallable => write!(
                f,
                "[line {}] Runtime error: Can only call functions and classes.",
                self.token.line,
            ),
            InterpretErrorKind::InvalidCallableArguments => write!(
                f,
                "[line {}] Runtime error: {}.",
                self.token.line,
                self.message
                    .as_deref()
                    .unwrap_or("Invalid number of arguments to callable")
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
    InvalidCallable,
    InvalidCallableArguments,
}
