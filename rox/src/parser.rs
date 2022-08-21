use std::{error::Error, fmt};

use crate::{
    ast::{Expression, Statement},
    token::{Token, TokenType},
    types::Literal,
};

pub type ParseResult<T> = std::result::Result<T, ParseError>;

pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Parser { tokens, current: 0 }
    }

    pub fn parse(&mut self) -> ParseResult<Vec<Statement>> {
        let mut statements = Vec::new();
        while !self.is_at_end() {
            statements.push(self.declaration()?);
        }

        Ok(statements)
    }

    fn declaration(&mut self) -> ParseResult<Statement> {
        let result = if self.matches(&[TokenType::Var]) {
            self.var_declaration()
        } else {
            self.statement()
        };
        result.inspect_err(|err| {
            if err.should_synchronize() {
                self.synchronize()
            }
        })
    }

    fn var_declaration(&mut self) -> ParseResult<Statement> {
        let name = self.consume(TokenType::Identifier, ParseErrorKind::ExpectedVariableName)?;
        let initializer = if self.matches(&[TokenType::Equal]) {
            Some(self.expression()?)
        } else {
            None
        };

        let _ = self.consume(
            TokenType::Semicolon,
            ParseErrorKind::ExpectedSemicolonAfterVarDeclaration,
        );
        Ok(Statement::var(name, initializer))
    }

    fn statement(&mut self) -> ParseResult<Statement> {
        if self.matches(&[TokenType::Print]) {
            self.print_statement()
        } else if self.matches(&[TokenType::LeftBrace]) {
            self.block()
        } else {
            self.expr_statement()
        }
    }

    fn print_statement(&mut self) -> ParseResult<Statement> {
        let expression = self.expression()?;
        self.consume(
            TokenType::Semicolon,
            ParseErrorKind::ExpectedSemicolonAfterValue,
        )
        .map(|_| Statement::print(expression))
    }

    fn block(&mut self) -> ParseResult<Statement> {
        let mut statements = Vec::new();

        while !self.check(TokenType::RightBrace) && !self.is_at_end() {
            statements.push(self.declaration()?);
        }

        self.consume(
            TokenType::RightBrace,
            ParseErrorKind::ExpectedRightBraceAfterBlock,
        )
        .map(|_| Statement::block(statements))
    }

    fn expr_statement(&mut self) -> ParseResult<Statement> {
        let expression = self.expression()?;
        self.consume(
            TokenType::Semicolon,
            ParseErrorKind::ExpectedSemicolonAfterExpression,
        )
        .map(|_| Statement::expression(expression))
    }

    fn expression(&mut self) -> ParseResult<Expression> {
        self.assignment()
    }

    fn assignment(&mut self) -> ParseResult<Expression> {
        let expression = self.equality()?;

        if self.matches(&[TokenType::Equal]) {
            let equals = self.previous();
            let value = self.assignment()?;

            if let Expression::Variable { name } = expression {
                return Ok(Expression::assign(name, Box::new(value)));
            }

            return Err(ParseError {
                token: equals,
                kind: ParseErrorKind::InvalidAssignmentTarget,
            });
        }

        Ok(expression)
    }

    fn equality(&mut self) -> ParseResult<Expression> {
        let mut left = self.comparison()?;

        while self.matches(&[TokenType::NotEqual, TokenType::EqualEqual]) {
            let operator = self.previous();
            let right = self.comparison()?;
            left = Expression::binary(Box::new(left), operator, Box::new(right));
        }

        Ok(left)
    }

    fn comparison(&mut self) -> ParseResult<Expression> {
        let mut left = self.term()?;

        while self.matches(&[
            TokenType::Greater,
            TokenType::GreaterEqual,
            TokenType::Less,
            TokenType::LessEqual,
        ]) {
            let operator = self.previous();
            let right = self.term()?;
            left = Expression::binary(Box::new(left), operator, Box::new(right));
        }

        Ok(left)
    }

    fn term(&mut self) -> ParseResult<Expression> {
        let mut left = self.factor()?;

        while self.matches(&[TokenType::Minus, TokenType::Plus]) {
            let operator = self.previous();
            let right = self.factor()?;
            left = Expression::binary(Box::new(left), operator, Box::new(right));
        }

        Ok(left)
    }

    fn factor(&mut self) -> ParseResult<Expression> {
        let mut left = self.unary()?;

        while self.matches(&[TokenType::Slash, TokenType::Star]) {
            let operator = self.previous();
            let right = self.unary()?;
            left = Expression::binary(Box::new(left), operator, Box::new(right));
        }

        Ok(left)
    }

    fn unary(&mut self) -> ParseResult<Expression> {
        if self.matches(&[TokenType::Bang, TokenType::Minus]) {
            let operator = self.previous();
            let right = self.unary()?;
            return Ok(Expression::unary(operator, Box::new(right)));
        }

        self.primary()
    }

    fn primary(&mut self) -> ParseResult<Expression> {
        if self.matches(&[TokenType::True]) {
            return Ok(Expression::literal(Literal::True));
        }

        if self.matches(&[TokenType::False]) {
            return Ok(Expression::literal(Literal::False));
        }

        if self.matches(&[TokenType::Nil]) {
            return Ok(Expression::literal(Literal::Nil));
        }

        if self.matches(&[TokenType::Number, TokenType::String]) {
            return Ok(Expression::literal(self.previous().literal.unwrap()));
        }

        if self.matches(&[TokenType::Identifier]) {
            return Ok(Expression::variable(self.previous()));
        }

        if self.matches(&[TokenType::LeftParen]) {
            let expression = self.expression()?;
            return self
                .consume(
                    TokenType::RightParen,
                    ParseErrorKind::ExpectedRightParenthesis,
                )
                .map(|_| Expression::grouping(Box::new(expression)));
        }

        Err(ParseError {
            token: self.peek(),
            kind: ParseErrorKind::ExpectedExpression,
        })
    }

    fn matches(&mut self, token_types: &[TokenType]) -> bool {
        for &token_type in token_types.iter() {
            if self.check(token_type) {
                self.advance();
                return true;
            }
        }

        false
    }

    fn check(&self, token_type: TokenType) -> bool {
        if self.is_at_end() {
            return false;
        }

        self.peek().token_type == token_type
    }

    fn is_at_end(&self) -> bool {
        self.peek().token_type == TokenType::Eof
    }

    fn peek(&self) -> Token {
        self.tokens.get(self.current).unwrap().clone()
    }

    fn advance(&mut self) -> Token {
        if !self.is_at_end() {
            self.current += 1;
        }

        self.previous()
    }

    fn previous(&mut self) -> Token {
        self.tokens.get(self.current - 1).unwrap().clone()
    }

    fn consume(&mut self, token_type: TokenType, error_kind: ParseErrorKind) -> ParseResult<Token> {
        if self.check(token_type) {
            return Ok(self.advance());
        }

        Err(ParseError {
            token: self.peek(),
            kind: error_kind,
        })
    }

    fn synchronize(&mut self) {
        self.advance();

        while !self.is_at_end() {
            if self.previous().token_type == TokenType::Semicolon {
                return;
            }

            if [
                TokenType::Class,
                TokenType::Fun,
                TokenType::Var,
                TokenType::For,
                TokenType::If,
                TokenType::While,
                TokenType::Print,
                TokenType::Return,
            ]
            .iter()
            .any(|&token_type| token_type == self.peek().token_type)
            {
                return;
            }

            self.advance();
        }
    }
}

#[derive(Debug, Clone)]
pub struct ParseError {
    token: Token,
    kind: ParseErrorKind,
}

impl ParseError {
    pub fn should_synchronize(&self) -> bool {
        !matches!(self.kind, ParseErrorKind::InvalidAssignmentTarget)
    }
}

impl Error for ParseError {}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.token.token_type == TokenType::Eof {
            write!(
                f,
                "[line {}] Parse error at end: {}",
                self.token.line, self.kind
            )
        } else {
            write!(f, "[line {}] Parse error: {}", self.token.line, self.kind)
        }
    }
}

#[derive(Debug, Clone)]
pub enum ParseErrorKind {
    ExpectedExpression,
    ExpectedRightParenthesis,
    ExpectedSemicolonAfterValue,
    ExpectedSemicolonAfterExpression,
    ExpectedVariableName,
    ExpectedSemicolonAfterVarDeclaration,
    InvalidAssignmentTarget,
    ExpectedRightBraceAfterBlock,
}

impl fmt::Display for ParseErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self {
            Self::ExpectedExpression => write!(f, "Expected expression."),
            Self::ExpectedRightParenthesis => write!(f, "Expected ')' after expression."),
            Self::ExpectedSemicolonAfterValue => write!(f, "Expected ';' after value."),
            Self::ExpectedSemicolonAfterExpression => write!(f, "Expected ';' after expression."),
            Self::ExpectedVariableName => write!(f, "Expected variable name."),
            Self::ExpectedSemicolonAfterVarDeclaration => {
                write!(f, "Expected ';' after variable declaration.")
            }
            Self::InvalidAssignmentTarget => {
                write!(f, "Invalid assignment target.")
            }
            Self::ExpectedRightBraceAfterBlock => write!(f, "Expected '}}' after block."),
        }
    }
}
