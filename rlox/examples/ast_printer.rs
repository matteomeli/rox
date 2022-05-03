use rlox::{
    ast::{Binary, Expr, Grouping, Unary, Visitor},
    token::{Literal, Token, TokenType},
};
struct AstPrinter;

impl AstPrinter {
    pub fn print(&mut self, e: &Expr) -> String {
        e.accept(self)
    }
}

impl Visitor for AstPrinter {
    type Result = String;

    fn visit_binary_expr(&mut self, binary: &Binary) -> Self::Result {
        format!(
            "({} {} {})",
            binary.1.lexeme,
            binary.0.accept(self),
            binary.2.accept(self)
        )
    }

    fn visit_grouping_expr(&mut self, grouping: &Grouping) -> Self::Result {
        format!("(group {})", grouping.0.accept(self))
    }

    fn visit_literal_expr(&mut self, literal: &Literal) -> Self::Result {
        match literal {
            Literal::Nil => "nil".to_string(),
            Literal::String(s) => s.to_owned(),
            Literal::Number(n) => n.to_string(),
        }
    }

    fn visit_unary_expr(&mut self, unary: &Unary) -> Self::Result {
        format!("({} {})", unary.0.lexeme, unary.1.accept(self))
    }
}

fn main() {
    let e = Expr::Binary(Binary(
        Box::new(Expr::Unary(Unary(
            Token::new(TokenType::Minus, "-".to_string(), None, 1),
            Box::new(Expr::Literal(Literal::Number(123.0))),
        ))),
        Token::new(TokenType::Star, "*".to_string(), None, 1),
        Box::new(Expr::Grouping(Grouping(Box::new(Expr::Literal(
            Literal::Number(45.67),
        ))))),
    ));

    println!("{}", AstPrinter.print(&e));
}
