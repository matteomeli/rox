use rox::{
    ast::Expression,
    printers::RPNPrinter,
    token::{Token, TokenType},
    types::Literal,
};

fn main() {
    let e = Expression::binary(
        Box::new(Expression::binary(
            Box::new(Expression::literal(Literal::Number(1.0))),
            Token::new(TokenType::Plus, "+".to_string(), None, 1),
            Box::new(Expression::literal(Literal::Number(2.0))),
        )),
        Token::new(TokenType::Star, "*".to_string(), None, 1),
        Box::new(Expression::binary(
            Box::new(Expression::literal(Literal::Number(4.0))),
            Token::new(TokenType::Minus, "-".to_string(), None, 1),
            Box::new(Expression::literal(Literal::Number(3.0))),
        )),
    );

    println!("{}", RPNPrinter.print(&e));
}
