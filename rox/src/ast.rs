use crate::token::Token;
use crate::types::Literal;

pub trait ExpressionVisitor {
    type Result;

    fn visit_expr(&mut self, expression: &Expression) -> Self::Result;
}

#[derive(Debug, Clone)]
pub struct Expression {
    pub id: usize,
    pub kind: ExpressionKind,
}

impl Expression {
    pub fn new(id: usize, kind: ExpressionKind) -> Self {
        Expression { id, kind }
    }

    pub fn accept<V: ExpressionVisitor>(&self, visitor: &mut V) -> V::Result {
        visitor.visit_expr(self)
    }
}

#[derive(Debug, Clone)]
pub enum ExpressionKind {
    Assign {
        name: Token,
        value: Box<Expression>,
    },
    Binary {
        left: Box<Expression>,
        operator: Token,
        right: Box<Expression>,
    },
    Call {
        callee: Box<Expression>,
        paren: Token,
        arguments: Vec<Expression>,
    },
    Grouping {
        expr: Box<Expression>,
    },
    Literal {
        literal: Literal,
    },
    Logical {
        left: Box<Expression>,
        operator: Token,
        right: Box<Expression>,
    },
    Unary {
        operator: Token,
        expr: Box<Expression>,
    },
    Variable {
        name: Token,
    },
}

impl ExpressionKind {
    pub fn assign(name: Token, value: Box<Expression>) -> Self {
        ExpressionKind::Assign { name, value }
    }

    pub fn binary(left: Box<Expression>, operator: Token, right: Box<Expression>) -> Self {
        ExpressionKind::Binary {
            left,
            operator,
            right,
        }
    }

    pub fn call(callee: Box<Expression>, paren: Token, arguments: Vec<Expression>) -> Self {
        ExpressionKind::Call {
            callee,
            paren,
            arguments,
        }
    }

    pub fn grouping(expr: Box<Expression>) -> Self {
        ExpressionKind::Grouping { expr }
    }

    pub fn literal(literal: Literal) -> Self {
        ExpressionKind::Literal { literal }
    }

    pub fn logical(left: Box<Expression>, operator: Token, right: Box<Expression>) -> Self {
        ExpressionKind::Logical {
            left,
            operator,
            right,
        }
    }

    pub fn unary(operator: Token, expr: Box<Expression>) -> Self {
        ExpressionKind::Unary { operator, expr }
    }

    pub fn variable(name: Token) -> Self {
        ExpressionKind::Variable { name }
    }
}

pub trait StatementVisitor {
    type Result;

    fn visit_stmt(&mut self, statement: &Statement) -> Self::Result;
}

#[derive(Debug, Clone)]
pub enum Statement {
    Block {
        statements: Vec<Statement>,
    },
    Break,
    Continue,
    Expression(Expression),
    Function(FunctionDeclaration),
    If {
        condition: Expression,
        then_branch: Box<Statement>,
        else_branch: Option<Box<Statement>>,
    },
    Print(Expression),
    Return {
        keyword: Token,
        value: Option<Expression>,
    },
    Var {
        name: Token,
        initializer: Option<Expression>,
    },
    While {
        condition: Expression,
        body: Box<Statement>,
    },
}

impl Statement {
    pub fn block(statements: Vec<Statement>) -> Self {
        Statement::Block { statements }
    }

    pub fn r#break() -> Self {
        Statement::Break
    }

    pub fn r#continue() -> Self {
        Statement::Continue
    }

    pub fn expression(expression: Expression) -> Self {
        Statement::Expression(expression)
    }

    pub fn function(name: Token, params: Vec<Token>, body: Vec<Statement>) -> Self {
        Statement::Function(FunctionDeclaration { name, params, body })
    }

    pub fn r#if(
        condition: Expression,
        then_branch: Box<Statement>,
        else_branch: Option<Box<Statement>>,
    ) -> Self {
        Statement::If {
            condition,
            then_branch,
            else_branch,
        }
    }

    pub fn print(expression: Expression) -> Self {
        Statement::Print(expression)
    }

    pub fn r#return(keyword: Token, value: Option<Expression>) -> Self {
        Statement::Return { keyword, value }
    }

    pub fn var(name: Token, initializer: Option<Expression>) -> Self {
        Statement::Var { name, initializer }
    }

    pub fn r#while(condition: Expression, body: Box<Statement>) -> Self {
        Statement::While { condition, body }
    }

    pub fn accept<V: StatementVisitor>(&self, visitor: &mut V) -> V::Result {
        visitor.visit_stmt(self)
    }
}

#[derive(Debug, Clone)]
pub struct FunctionDeclaration {
    pub name: Token,
    pub params: Vec<Token>,
    pub body: Vec<Statement>,
}
