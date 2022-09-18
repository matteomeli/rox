use crate::{
    ast::{Expression, ExpressionKind, ExpressionVisitor},
    token::TokenType,
    types::Literal,
};

pub struct AstPrinter;

impl AstPrinter {
    pub fn print(&mut self, e: &Expression) -> String {
        e.accept(self)
    }
}

impl ExpressionVisitor for AstPrinter {
    type Result = String;

    fn visit_expr(&mut self, expression: &Expression) -> Self::Result {
        match expression {
            Expression {
                kind:
                    ExpressionKind::Binary {
                        left,
                        operator,
                        right,
                    },
                ..
            } => format!(
                "({} {} {})",
                operator.lexeme,
                left.accept(self),
                right.accept(self)
            ),
            Expression {
                kind: ExpressionKind::Grouping { expr },
                ..
            } => format!("(group {})", expr.accept(self)),
            Expression {
                kind: ExpressionKind::Literal { literal },
                ..
            } => match literal {
                Literal::Nil => "nil".to_string(),
                Literal::String(s) => s.to_owned(),
                Literal::Number(n) => n.to_string(),
                Literal::False => "false".to_string(),
                Literal::True => "true".to_string(),
            },
            Expression {
                kind: ExpressionKind::Unary { operator, expr },
                ..
            } => {
                format!("({} {})", operator.lexeme, expr.accept(self))
            }
            Expression {
                kind: ExpressionKind::Variable { name },
                ..
            } => format!("var {}", &name.lexeme),
            Expression {
                kind: ExpressionKind::Assign { .. },
                ..
            } => todo!(),
            Expression {
                kind: ExpressionKind::Logical { .. },
                ..
            } => todo!(),
            Expression {
                kind: ExpressionKind::Call { .. },
                ..
            } => todo!(),
            Expression {
                kind: ExpressionKind::Get { .. },
                ..
            } => todo!(),
            Expression {
                kind: ExpressionKind::Set { .. },
                ..
            } => todo!(),
            Expression {
                kind: ExpressionKind::This { .. },
                ..
            } => todo!(),
            Expression {
                kind: ExpressionKind::Super { .. },
                ..
            } => todo!(),
        }
    }
}

pub struct RPNPrinter;

impl RPNPrinter {
    pub fn print(&mut self, e: &Expression) -> String {
        e.accept(self)
    }
}

impl ExpressionVisitor for RPNPrinter {
    type Result = String;

    fn visit_expr(&mut self, expression: &Expression) -> Self::Result {
        match expression {
            Expression {
                kind:
                    ExpressionKind::Binary {
                        left,
                        operator,
                        right,
                    },
                ..
            } => format!(
                "{} {} {}",
                left.accept(self),
                right.accept(self),
                operator.lexeme,
            ),
            Expression {
                kind: ExpressionKind::Grouping { expr },
                ..
            } => expr.accept(self),
            Expression {
                kind: ExpressionKind::Literal { literal },
                ..
            } => match literal {
                Literal::Nil => "nil".to_string(),
                Literal::String(s) => s.to_owned(),
                Literal::Number(n) => n.to_string(),
                Literal::False => "false".to_string(),
                Literal::True => "true".to_string(),
            },
            Expression {
                kind: ExpressionKind::Unary { operator, expr },
                ..
            } => {
                let operator = if operator.token_type == TokenType::Minus {
                    "~"
                } else {
                    &operator.lexeme
                };
                format!("{} {}", operator, expr.accept(self))
            }
            Expression {
                kind: ExpressionKind::Variable { name },
                ..
            } => format!("var {}", &name.lexeme),
            Expression {
                kind: ExpressionKind::Assign { .. },
                ..
            } => todo!(),
            Expression {
                kind: ExpressionKind::Logical { .. },
                ..
            } => todo!(),
            Expression {
                kind: ExpressionKind::Call { .. },
                ..
            } => todo!(),
            Expression {
                kind: ExpressionKind::Get { .. },
                ..
            } => todo!(),
            Expression {
                kind: ExpressionKind::Set { .. },
                ..
            } => todo!(),
            Expression {
                kind: ExpressionKind::This { .. },
                ..
            } => todo!(),
            Expression {
                kind: ExpressionKind::Super { .. },
                ..
            } => todo!(),
        }
    }
}
