use num_enum::{IntoPrimitive, TryFromPrimitive};

use crate::{chunk::OpCode, compiler::Compiler, scanner::TokenType};

#[derive(Default, IntoPrimitive, TryFromPrimitive, PartialEq, Eq, PartialOrd)]
#[repr(u8)]
pub enum Precedence {
    #[default]
    None,
    Assignment, // =
    Or,         // or
    And,        // and
    Equality,   // == !=
    Comparison, // < > <= >=
    Term,       // + -
    Factor,     // * /
    Unary,      // - !
    Call,       // . ()
    Primary,
}

type ParseFn = fn(&mut Compiler);

#[derive(Default)]
pub struct ParseRule {
    pub(crate) prefix: Option<ParseFn>,
    pub(crate) infix: Option<ParseFn>,
    pub(crate) precedence: Precedence,
}

impl ParseRule {
    pub fn from(token_type: TokenType) -> ParseRule {
        match token_type {
            TokenType::LeftParen => ParseRule {
                prefix: Some(grouping),
                ..ParseRule::default()
            },
            TokenType::RightParen => ParseRule::default(),
            TokenType::LeftBrace => ParseRule::default(),
            TokenType::RightBrace => ParseRule::default(),
            TokenType::Comma => ParseRule::default(),
            TokenType::Dot => ParseRule::default(),
            TokenType::Minus => ParseRule {
                prefix: Some(unary),
                infix: Some(binary),
                precedence: Precedence::Term,
            },
            TokenType::Plus => ParseRule {
                infix: Some(binary),
                precedence: Precedence::Term,
                ..ParseRule::default()
            },
            TokenType::Semicolon => ParseRule::default(),
            TokenType::Slash => ParseRule {
                infix: Some(binary),
                precedence: Precedence::Factor,
                ..ParseRule::default()
            },
            TokenType::Star => ParseRule {
                infix: Some(binary),
                precedence: Precedence::Factor,
                ..ParseRule::default()
            },
            TokenType::Bang => ParseRule::default(),
            TokenType::BangEqual => ParseRule::default(),
            TokenType::Equal => ParseRule::default(),
            TokenType::EqualEqual => ParseRule::default(),
            TokenType::Greater => ParseRule::default(),
            TokenType::GreaterEqual => ParseRule::default(),
            TokenType::Less => ParseRule::default(),
            TokenType::LessEqual => ParseRule::default(),
            TokenType::Identifier => ParseRule::default(),
            TokenType::String => ParseRule::default(),
            TokenType::Number => ParseRule {
                prefix: Some(number),
                ..ParseRule::default()
            },
            TokenType::And => ParseRule::default(),
            TokenType::Class => ParseRule::default(),
            TokenType::Else => ParseRule::default(),
            TokenType::False => ParseRule::default(),
            TokenType::For => ParseRule::default(),
            TokenType::Fun => ParseRule::default(),
            TokenType::If => ParseRule::default(),
            TokenType::Nil => ParseRule::default(),
            TokenType::Or => ParseRule::default(),
            TokenType::Print => ParseRule::default(),
            TokenType::Return => ParseRule::default(),
            TokenType::Super => ParseRule::default(),
            TokenType::This => ParseRule::default(),
            TokenType::True => ParseRule::default(),
            TokenType::Var => ParseRule::default(),
            TokenType::While => ParseRule::default(),
            TokenType::Break => ParseRule::default(),
            TokenType::Continue => ParseRule::default(),
            TokenType::Eof => ParseRule::default(),
            _ => ParseRule::default(),
        }
    }
}

impl From<TokenType> for ParseRule {
    fn from(token_type: TokenType) -> Self {
        ParseRule::from(token_type)
    }
}

pub fn get_rule(token_type: TokenType) -> ParseRule {
    token_type.into()
}

pub fn number(compiler: &mut Compiler) {
    let n: f64 = compiler
        .previous
        .as_ref()
        .unwrap()
        .lexeme
        .unwrap()
        .parse()
        .unwrap();

    compiler.emit_constant(n.into());
}

pub fn grouping(compiler: &mut Compiler) {
    compiler.expression();
    compiler.consume(TokenType::RightParen, "Expect ')' after expression.");
}

pub fn unary(compiler: &mut Compiler) {
    let operator = compiler.previous.as_ref().unwrap();
    let token_type = operator.token_type;
    let line = operator.line; // Store the line now so it's not affected by the operand

    // Compile unrary operand
    compiler.parse_precedence(Precedence::Unary);

    // Compile negate opcode
    match token_type {
        TokenType::Minus => compiler.emit_byte_with_line(OpCode::Negate.into(), line),
        _ => unreachable!(),
    }
}

pub fn binary(compiler: &mut Compiler) {
    let operator = compiler.previous.as_ref().unwrap();
    let token_type = operator.token_type;

    // Get parse rule for binary token
    let parse_rule: ParseRule = token_type.into();
    let precedence: u8 = parse_rule.precedence.into();
    compiler.parse_precedence(Precedence::try_from(precedence + 1).unwrap());

    match token_type {
        TokenType::Plus => compiler.emit_byte(OpCode::Add.into()),
        TokenType::Minus => compiler.emit_byte(OpCode::Subtract.into()),
        TokenType::Star => compiler.emit_byte(OpCode::Multiply.into()),
        TokenType::Slash => compiler.emit_byte(OpCode::Divide.into()),
        _ => unreachable!(),
    }
}
