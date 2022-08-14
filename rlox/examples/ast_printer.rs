use rlox::{
    ast::{Binary, Expr, Grouping, Unary},
    printers::AstPrinter,
    token::{Literal, Token, TokenType},
};

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
