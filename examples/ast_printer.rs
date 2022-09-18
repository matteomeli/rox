use rox::{
    ast::Expression,
    printers::AstPrinter,
    token::{Token, TokenType},
    types::Literal,
};

fn main() {
    let e = Expression::binary(
        Box::new(Expression::unary(
            Token::new(TokenType::Minus, "-".to_string(), None, 1),
            Box::new(Expression::literal(Literal::Number(123.0))),
        )),
        Token::new(TokenType::Star, "*".to_string(), None, 1),
        Box::new(Expression::grouping(Box::new(Expression::literal(
            Literal::Number(45.67),
        )))),
    );

    println!("{}", AstPrinter.print(&e));
}
