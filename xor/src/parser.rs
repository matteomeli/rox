use num_enum::{IntoPrimitive, TryFromPrimitive};

use crate::{
    chunk::OpCode,
    compiler::Compiler,
    scanner::TokenType,
    value::{create_string, InternedString},
    vm::CompileError,
};

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

type ParseFn = fn(&mut Compiler, bool);

#[derive(Default)]
pub struct ParseRule {
    pub prefix: Option<ParseFn>,
    pub infix: Option<ParseFn>,
    pub precedence: Precedence,
}

impl ParseRule {
    pub fn from(token_type: TokenType) -> ParseRule {
        match token_type {
            TokenType::LeftParen => ParseRule {
                prefix: Some(grouping),
                infix: Some(call),
                precedence: Precedence::Call,
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
            TokenType::Bang => ParseRule {
                prefix: Some(unary),
                ..ParseRule::default()
            },
            TokenType::BangEqual => ParseRule {
                infix: Some(binary),
                precedence: Precedence::Equality,
                ..ParseRule::default()
            },
            TokenType::Equal => ParseRule::default(),
            TokenType::EqualEqual => ParseRule {
                infix: Some(binary),
                precedence: Precedence::Equality,
                ..ParseRule::default()
            },
            TokenType::Greater => ParseRule {
                infix: Some(binary),
                precedence: Precedence::Comparison,
                ..ParseRule::default()
            },
            TokenType::GreaterEqual => ParseRule {
                infix: Some(binary),
                precedence: Precedence::Comparison,
                ..ParseRule::default()
            },
            TokenType::Less => ParseRule {
                infix: Some(binary),
                precedence: Precedence::Comparison,
                ..ParseRule::default()
            },
            TokenType::LessEqual => ParseRule {
                infix: Some(binary),
                precedence: Precedence::Comparison,
                ..ParseRule::default()
            },
            TokenType::Identifier => ParseRule {
                prefix: Some(variable),
                ..ParseRule::default()
            },
            TokenType::String => ParseRule {
                prefix: Some(string),
                ..ParseRule::default()
            },
            TokenType::Number => ParseRule {
                prefix: Some(number),
                ..ParseRule::default()
            },
            TokenType::And => ParseRule {
                infix: Some(and),
                precedence: Precedence::And,
                ..ParseRule::default()
            },
            TokenType::Class => ParseRule::default(),
            TokenType::Else => ParseRule::default(),
            TokenType::False => ParseRule {
                prefix: Some(literal),
                ..ParseRule::default()
            },
            TokenType::For => ParseRule::default(),
            TokenType::Fun => ParseRule::default(),
            TokenType::If => ParseRule::default(),
            TokenType::Let => ParseRule::default(),
            TokenType::Nil => ParseRule {
                prefix: Some(literal),
                ..ParseRule::default()
            },
            TokenType::Or => ParseRule {
                infix: Some(or),
                precedence: Precedence::Or,
                ..ParseRule::default()
            },
            TokenType::Print => ParseRule::default(),
            TokenType::Return => ParseRule::default(),
            TokenType::Super => ParseRule::default(),
            TokenType::This => ParseRule::default(),
            TokenType::True => ParseRule {
                prefix: Some(literal),
                ..ParseRule::default()
            },
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

fn number(compiler: &mut Compiler, _can_assign: bool) {
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

fn grouping(compiler: &mut Compiler, _can_assign: bool) {
    compiler.expression();
    compiler.consume(TokenType::RightParen, "Expect ')' after expression.");
}

fn unary(compiler: &mut Compiler, _can_assign: bool) {
    let operator = compiler.previous.as_ref().unwrap();
    let token_type = operator.token_type;
    let line = operator.line; // Store the line now so it's not affected by the operand

    // Compile unrary operand
    compiler.parse_precedence(Precedence::Unary);

    // Compile negate opcode
    match token_type {
        TokenType::Minus => compiler.emit_byte_with_line(OpCode::Negate.into(), line),
        TokenType::Bang => compiler.emit_byte_with_line(OpCode::Not.into(), line),
        _ => unreachable!(),
    }
}

fn binary(compiler: &mut Compiler, _can_assign: bool) {
    let operator = compiler.previous.as_ref().unwrap();
    let token_type = operator.token_type;

    // Get parse rule for binary token
    let parse_rule: ParseRule = token_type.into();
    let precedence: u8 = parse_rule.precedence.into();
    compiler.parse_precedence(Precedence::try_from(precedence + 1).unwrap());

    match token_type {
        TokenType::BangEqual => compiler.emit_bytes(OpCode::Equal.into(), OpCode::Not.into()),
        TokenType::EqualEqual => compiler.emit_byte(OpCode::Equal.into()),
        TokenType::Greater => compiler.emit_byte(OpCode::Greater.into()),
        TokenType::GreaterEqual => compiler.emit_bytes(OpCode::Less.into(), OpCode::Not.into()),
        TokenType::Less => compiler.emit_byte(OpCode::Less.into()),
        TokenType::LessEqual => compiler.emit_bytes(OpCode::Greater.into(), OpCode::Not.into()),
        TokenType::Plus => compiler.emit_byte(OpCode::Add.into()),
        TokenType::Minus => compiler.emit_byte(OpCode::Subtract.into()),
        TokenType::Star => compiler.emit_byte(OpCode::Multiply.into()),
        TokenType::Slash => compiler.emit_byte(OpCode::Divide.into()),
        _ => unreachable!(),
    }
}

fn literal(compiler: &mut Compiler, _can_assign: bool) {
    match compiler.previous.as_ref().unwrap().token_type {
        TokenType::False => compiler.emit_byte(OpCode::False.into()),
        TokenType::Nil => compiler.emit_byte(OpCode::Nil.into()),
        TokenType::True => compiler.emit_byte(OpCode::True.into()),
        _ => unreachable!(),
    }
}

fn string(compiler: &mut Compiler, _can_assign: bool) {
    let vm = &mut compiler.vm;
    let quoted_content = compiler.previous.as_ref().unwrap().lexeme.unwrap();
    // Remove string quotaion marks from lexeme
    let content = &quoted_content[1..quoted_content.len() - 1];
    let s = create_string(vm, content);
    compiler.emit_constant(s.into());
}

fn variable(compiler: &mut Compiler, can_assign: bool) {
    let name_str = compiler.previous.as_ref().unwrap().lexeme.unwrap();
    let name = compiler.previous_identifier();
    match compiler.fc.resolve_local(name_str) {
        Err(e) => {
            compiler.short_error(e);
        }
        Ok(slot) => {
            let (get_op, get_op_long, set_op, set_op_long, arg) = match slot {
                Some(slot) => (
                    OpCode::GetLocal,
                    OpCode::GetLocalLong,
                    OpCode::SetLocal,
                    OpCode::SetLocalLong,
                    Ok(slot),
                ),
                None => (
                    OpCode::GetGlobal,
                    OpCode::GetGlobalLong,
                    OpCode::SetGlobal,
                    OpCode::SetGlobalLong,
                    compiler.identifier_constant(name.clone()),
                ),
            };
            match arg {
                Err(e) => compiler.short_error(e),
                Ok(slot) => {
                    if can_assign && compiler.match_token(TokenType::Equal) {
                        // Disallow assignments to let variables after the first one
                        let interned: InternedString = name.try_into().unwrap();
                        if let Some(was_assigned) = compiler.vm.lets.get_mut(&interned) {
                            if *was_assigned {
                                compiler.short_error(CompileError::LetReassignment);
                                return;
                            }
                            *was_assigned = true;
                        }

                        compiler.expression();
                        compiler.emit_variable(set_op, set_op_long, slot);
                    } else {
                        compiler.emit_variable(get_op, get_op_long, slot);
                    }
                }
            }
        }
    }
}

fn and(compiler: &mut Compiler, _can_assign: bool) {
    let end_jump = compiler.emit_jump(OpCode::JumpIfFalse.into());

    compiler.emit_byte(OpCode::Pop.into());
    compiler.parse_precedence(Precedence::And);

    compiler.patch_jump(end_jump);
}

fn or(compiler: &mut Compiler, _can_assign: bool) {
    let else_jump = compiler.emit_jump(OpCode::JumpIfFalse.into());
    let end_jump = compiler.emit_jump(OpCode::Jump.into());

    compiler.patch_jump(else_jump);

    compiler.emit_byte(OpCode::Pop.into());
    compiler.parse_precedence(Precedence::Or);

    compiler.patch_jump(end_jump);
}

fn call(compiler: &mut Compiler, _can_assign: bool) {
    let arg_count = compiler.argument_list();
    compiler.emit_bytes(OpCode::Call.into(), arg_count as u8);
}
