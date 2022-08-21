use crate::token::Token;
use crate::types::Literal;

pub trait ExpressionVisitor {
    type Result;

    fn visit_expr(&mut self, expression: &Expression) -> Self::Result;
}

#[derive(Debug)]
pub enum Expression {
    Assign {
        name: Token,
        value: Box<Expression>,
    },
    Binary {
        left: Box<Expression>,
        operator: Token,
        right: Box<Expression>,
    },
    Grouping {
        expr: Box<Expression>,
    },
    Literal {
        literal: Literal,
    },
    Unary {
        operator: Token,
        expr: Box<Expression>,
    },
    Variable {
        name: Token,
    },
}

impl Expression {
    pub fn assign(name: Token, value: Box<Expression>) -> Self {
        Expression::Assign { name, value }
    }

    pub fn binary(left: Box<Expression>, operator: Token, right: Box<Expression>) -> Self {
        Expression::Binary {
            left,
            operator,
            right,
        }
    }

    pub fn grouping(expr: Box<Expression>) -> Self {
        Expression::Grouping { expr }
    }

    pub fn literal(literal: Literal) -> Self {
        Expression::Literal { literal }
    }

    pub fn unary(operator: Token, expr: Box<Expression>) -> Self {
        Expression::Unary { operator, expr }
    }

    pub fn variable(name: Token) -> Self {
        Expression::Variable { name }
    }

    pub fn accept<V: ExpressionVisitor>(&self, visitor: &mut V) -> V::Result {
        visitor.visit_expr(self)
    }
}

pub trait StatementVisitor {
    type Result;

    fn visit_stmt(&mut self, statement: &Statement) -> Self::Result;
}

#[derive(Debug)]
pub enum Statement {
    Block {
        statements: Vec<Statement>,
    },
    Expression(Expression),
    Print(Expression),
    Var {
        name: Token,
        initializer: Option<Expression>,
    },
}

impl Statement {
    pub fn block(statements: Vec<Statement>) -> Self {
        Statement::Block { statements }
    }

    pub fn expression(expression: Expression) -> Self {
        Statement::Expression(expression)
    }

    pub fn print(expression: Expression) -> Self {
        Statement::Print(expression)
    }

    pub fn var(name: Token, initializer: Option<Expression>) -> Self {
        Statement::Var { name, initializer }
    }

    pub fn accept<V: StatementVisitor>(&self, visitor: &mut V) -> V::Result {
        visitor.visit_stmt(self)
    }
}
