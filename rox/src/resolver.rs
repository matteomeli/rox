use std::{collections::HashMap, error::Error, fmt};

use crate::{
    ast::{
        Expression, ExpressionKind, ExpressionVisitor, FunctionDeclaration, Statement,
        StatementVisitor,
    },
    interpreter::Interpreter,
    token::Token,
};

#[derive(Clone, Copy, PartialEq, Eq)]
enum FunctionType {
    None,
    Function,
}

pub struct Resolver<'a> {
    interpreter: &'a mut Interpreter,
    scopes: Vec<HashMap<String, bool>>,
    current_function_type: FunctionType,
}

impl<'a> Resolver<'a> {
    pub fn new(interpreter: &'a mut Interpreter) -> Self {
        Resolver {
            interpreter,
            scopes: Vec::new(),
            current_function_type: FunctionType::None,
        }
    }

    pub fn resolve(&mut self, statements: &[Statement]) -> ResolveResult<()> {
        for statement in statements {
            self.resolve_stmt(statement)?;
        }
        Ok(())
    }

    fn resolve_stmt(&mut self, statement: &Statement) -> ResolveResult<()> {
        statement.accept(self)?;
        Ok(())
    }

    fn begin_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn end_scope(&mut self) {
        self.scopes.pop();
    }

    fn declare(&mut self, name: &Token) -> ResolveResult<()> {
        if let Some(scope) = self.scopes.last_mut() {
            if scope.insert(name.lexeme.clone(), false).is_some() {
                return Err(ResolveError::CannotReadLocalVariableInOwnInitializer);
            } else {
                return Ok(());
            }
        }
        Ok(())
    }

    fn define(&mut self, name: &Token) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name.lexeme.clone(), true);
        }
    }

    fn resolve_expr(&mut self, expression: &Expression) -> ResolveResult<()> {
        expression.accept(self)?;
        Ok(())
    }

    fn resolve_local(&mut self, expression_id: usize, name: &Token) {
        for (index, scope) in self.scopes.iter().enumerate().rev() {
            if scope.contains_key(&name.lexeme) {
                self.interpreter
                    .resolve(expression_id, self.scopes.len() - index - 1);
                return;
            }
        }
    }

    fn resolve_function(
        &mut self,
        function: &FunctionDeclaration,
        function_type: FunctionType,
    ) -> ResolveResult<()> {
        let enclosing_function_type = self.current_function_type;
        self.current_function_type = function_type;

        self.begin_scope();
        for param in function.params.iter() {
            self.declare(param)?;
            self.define(param);
        }
        self.resolve(&function.body)?;
        self.end_scope();

        self.current_function_type = enclosing_function_type;

        Ok(())
    }
}

pub type ResolveResult<T> = Result<T, ResolveError>;

impl<'a> ExpressionVisitor for Resolver<'a> {
    type Result = ResolveResult<()>;

    fn visit_expr(&mut self, expression: &Expression) -> Self::Result {
        match expression {
            Expression {
                id,
                kind: ExpressionKind::Assign { name, value },
                ..
            } => {
                self.resolve_expr(value)?;
                self.resolve_local(*id, name);
                Ok(())
            }
            Expression {
                kind: ExpressionKind::Binary { left, right, .. },
                ..
            } => {
                self.resolve_expr(left)?;
                self.resolve_expr(right)?;
                Ok(())
            }
            Expression {
                kind:
                    ExpressionKind::Call {
                        callee, arguments, ..
                    },
                ..
            } => {
                self.resolve_expr(callee)?;
                for arg in arguments {
                    self.resolve_expr(arg)?;
                }

                Ok(())
            }
            Expression {
                kind: ExpressionKind::Grouping { expr },
                ..
            } => {
                self.resolve_expr(expr)?;
                Ok(())
            }
            Expression {
                kind: ExpressionKind::Literal { .. },
                ..
            } => Ok(()),
            Expression {
                kind: ExpressionKind::Logical { left, right, .. },
                ..
            } => {
                self.visit_expr(left)?;
                self.visit_expr(right)?;
                Ok(())
            }
            Expression {
                kind: ExpressionKind::Unary { expr, .. },
                ..
            } => {
                self.resolve_expr(expr)?;
                Ok(())
            }
            Expression {
                id,
                kind: ExpressionKind::Variable { name },
            } => {
                if !self.scopes.is_empty() {
                    if let Some(scope) = self.scopes.last() {
                        if let Some(false) = scope.get(&name.lexeme) {
                            return Err(ResolveError::CannotReadLocalVariableInOwnInitializer);
                        }
                    }
                }

                self.resolve_local(*id, name);
                Ok(())
            }
        }
    }
}

impl<'a> StatementVisitor for Resolver<'a> {
    type Result = ResolveResult<()>;

    fn visit_stmt(&mut self, statement: &Statement) -> Self::Result {
        match statement {
            Statement::Block { statements } => {
                self.begin_scope();
                self.resolve(statements)?;
                self.end_scope();
                Ok(())
            }
            Statement::Var { name, initializer } => {
                self.declare(name)?;
                if let Some(expression) = initializer {
                    self.resolve_expr(expression)?;
                }
                self.define(name);
                Ok(())
            }
            Statement::Expression(expression) => {
                self.resolve_expr(expression)?;
                Ok(())
            }
            Statement::Function(declaration) => {
                self.declare(&declaration.name)?;
                self.define(&declaration.name);

                self.resolve_function(declaration, FunctionType::Function)?;
                Ok(())
            }
            Statement::If {
                condition,
                then_branch,
                else_branch,
            } => {
                self.resolve_expr(condition)?;
                self.resolve_stmt(then_branch)?;
                if let Some(else_stmt) = else_branch {
                    self.resolve_stmt(else_stmt)?;
                }
                Ok(())
            }
            Statement::Print(expression) => {
                self.resolve_expr(expression)?;
                Ok(())
            }
            Statement::Return { value, .. } => {
                if self.current_function_type == FunctionType::None {
                    return Err(ResolveError::CannotReadLocalVariableInOwnInitializer);
                }

                if let Some(expression) = value {
                    self.resolve_expr(expression)?;
                }
                Ok(())
            }
            Statement::While { condition, body } => {
                self.resolve_expr(condition)?;
                self.resolve_stmt(body)?;
                Ok(())
            }
            _ => Ok(()),
        }
    }
}

#[derive(Debug)]
pub enum ResolveError {
    CannotReadLocalVariableInOwnInitializer,
    VariableAlreadyPresentWithThisNameInScope,
    CannotReturnFromTopLevelBlock,
}

impl Error for ResolveError {}

impl fmt::Display for ResolveError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::CannotReadLocalVariableInOwnInitializer => {
                write!(f, "Cannot read local variable in its own initializer.")
            }
            Self::VariableAlreadyPresentWithThisNameInScope => {
                write!(f, "Already a variable with this name in this scope.")
            }
            Self::CannotReturnFromTopLevelBlock => {
                write!(f, "Cannot return from top-level block.")
            }
        }
    }
}
