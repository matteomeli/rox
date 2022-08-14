use rlox::{
    ast::{Binary, Expr, Grouping, Unary, Visitor},
    token::{Literal, Token, TokenType},
};

struct RPNPrinter;

impl RPNPrinter {
    pub fn print(&mut self, e: &Expr) -> String {
        e.accept(self)
    }
}

impl Visitor for RPNPrinter {
    type Result = String;

    fn visit_binary_expr(&mut self, binary: &Binary) -> Self::Result {
        format!(
            "{} {} {}",
            binary.0.accept(self),
            binary.2.accept(self),
            binary.1.lexeme,
        )
    }

    fn visit_grouping_expr(&mut self, grouping: &Grouping) -> Self::Result {
        grouping.0.accept(self)
    }

    fn visit_literal_expr(&mut self, literal: &Literal) -> Self::Result {
        match literal {
            Literal::Nil => "nil".to_string(),
            Literal::String(s) => s.to_owned(),
            Literal::Number(n) => n.to_string(),
            Literal::False => "false".to_string(),
            Literal::True => "true".to_string(),
        }
    }

    fn visit_unary_expr(&mut self, unary: &Unary) -> Self::Result {
        let operator = if unary.0.token_type == TokenType::Minus {
            "~"
        } else {
            &unary.0.lexeme
        };
        format!("{} {}", operator, unary.1.accept(self))
    }
}

fn main() {
    let e = Expr::Binary(Binary(
        Box::new(Expr::Binary(Binary(
            Box::new(Expr::Literal(Literal::Number(1.0))),
            Token::new(TokenType::Plus, "+".to_string(), None, 1),
            Box::new(Expr::Literal(Literal::Number(2.0))),
        ))),
        Token::new(TokenType::Star, "*".to_string(), None, 1),
        Box::new(Expr::Binary(Binary(
            Box::new(Expr::Literal(Literal::Number(4.0))),
            Token::new(TokenType::Minus, "-".to_string(), None, 1),
            Box::new(Expr::Literal(Literal::Number(3.0))),
        ))),
    ));

    println!("{}", RPNPrinter.print(&e));
}
