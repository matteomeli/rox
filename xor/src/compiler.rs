use fnv::FnvHashMap;

use crate::{
    chunk::{Chunk, OpCode, U24_MAX},
    debug,
    parser::{get_rule, Precedence},
    scanner::{Scanner, Token, TokenType},
    value::{create_string, InternedString, Value},
    vm::{CompileError, VM},
};

type CompilerResult = Result<Chunk, CompileError>;

pub struct Local<'src> {
    pub name: &'src str,
    pub depth: Option<usize>,
}

pub struct Compiler<'src, 'vm> {
    pub(crate) vm: &'vm mut VM,
    scanner: Scanner<'src>,
    pub(crate) previous: Option<Token<'src>>,
    current: Option<Token<'src>>,
    first_error: Option<CompileError>,
    panic_mode: bool,
    pub(crate) chunk: Chunk,
    scope_depth: usize,
    pub(crate) locals_indices: FnvHashMap<&'src str, Vec<usize>>,
    pub(crate) locals: Vec<Local<'src>>,
}

impl<'src, 'vm> Compiler<'src, 'vm> {
    pub fn new(vm: &'vm mut VM, scanner: Scanner<'src>) -> Self {
        Compiler {
            vm,
            scanner,
            previous: None,
            current: None,
            first_error: None,
            panic_mode: false,
            chunk: Chunk::default(),
            scope_depth: 0,
            locals_indices: FnvHashMap::default(),
            locals: Vec::default(),
        }
    }

    pub fn advance(&mut self) {
        self.previous = self.current.take();
        loop {
            let token = self.scanner.scan_token();
            let error_message = TokenType::error_message(token.token_type);
            self.current = Some(token);
            match error_message {
                None => break,
                Some(e) => self.error_at_current(e, CompileError::ParseError),
            }
        }
    }

    pub fn expression(&mut self) {
        self.parse_precedence(Precedence::Assignment);
    }

    pub fn parse_precedence(&mut self, precendence: Precedence) {
        self.advance();

        // Can the next rule assing?
        let can_assign = precendence <= Precedence::Assignment;

        // Look up a prefix parser for the current token
        match get_rule(self.previous.as_ref().unwrap().token_type).prefix {
            Some(rule) => rule(self, can_assign),
            _ => {
                self.error("Expect espression", CompileError::ParseError);
                return;
            }
        }

        // Look for an infix parser for the next token
        while precendence <= get_rule(self.current.as_ref().unwrap().token_type).precedence {
            // Only parse if precedence is low enough to permit the infix operator
            self.advance();

            get_rule(self.previous.as_ref().unwrap().token_type)
                .infix
                .unwrap()(self, can_assign);
        }

        if can_assign && self.match_token(TokenType::Equal) {
            self.error("Invalid assignment target.", CompileError::ParseError);
        }
    }

    pub fn match_token(&mut self, token_type: TokenType) -> bool {
        if !self.check_token(token_type) {
            return false;
        }

        self.advance();
        true
    }

    pub fn consume(&mut self, token_type: TokenType, message: &str) {
        if let Some(token) = &self.current {
            if token.token_type == token_type {
                self.advance();
                return;
            }
        }

        self.error_at_current(message, CompileError::ParseError);
    }

    pub fn previous_identifier(&mut self) -> Value {
        let name = &self.previous.as_ref().unwrap().lexeme.unwrap();
        let vm = &mut self.vm;
        create_string(vm, name).into()
    }

    pub fn identifier_constant(&mut self, name: Value) -> Result<usize, CompileError> {
        let interned: InternedString = name.clone().try_into().unwrap();
        match self.vm.globals_indices.get(&interned) {
            Some(Value::Number(slot)) => Ok(*slot as usize),
            None => {
                if self.vm.globals.len() == U24_MAX as usize + 1 {
                    return Err(CompileError::TooManyGlobals);
                }

                let slot = self.vm.globals.len();
                self.vm.globals.push((name, Value::Undefined));
                self.vm
                    .globals_indices
                    .insert(interned, (slot as f64).into());
                Ok(slot)
            }
            _ => unimplemented!(),
        }
    }

    pub fn resolve_local(&mut self, name: &str) -> Result<Option<usize>, CompileError> {
        if let Some(locals_indices) = self.locals_indices.get(name) {
            if let Some(index) = locals_indices.last() {
                let local = &self.locals[*index];
                if local.depth.is_none() {
                    return Err(CompileError::UninitializedLocal);
                }
                return Ok(Some(*index));
            }
        }

        Ok(None)
    }

    pub fn emit_byte(&mut self, byte: u8) {
        let line = self.previous.as_ref().unwrap().line;
        self.chunk.write(byte, line);
    }

    pub fn emit_byte_with_line(&mut self, byte: u8, line: u32) {
        self.chunk.write(byte, line);
    }

    pub fn emit_bytes(&mut self, byte1: u8, byte2: u8) {
        self.emit_byte(byte1);
        self.emit_byte(byte2);
    }

    pub fn emit_return(&mut self) {
        self.emit_byte(OpCode::Return.into());
    }

    pub fn emit_constant(&mut self, value: Value) {
        let line = self.previous.as_ref().unwrap().line;
        if self.chunk.write_constant(value, line).is_err() {
            let message: &str = &format!("{}", CompileError::TooManyConstants);
            self.error(message, CompileError::TooManyConstants);
        }
    }

    pub fn emit_variable(&mut self, op: OpCode, op_long: OpCode, slot: usize) {
        if slot < u8::MAX as usize + 1 {
            self.emit_bytes(op.into(), slot as u8);
        } else {
            // Emit "long" opcode and slot with 24 bit operand
            self.emit_byte(op_long.into());
            // Convert to "u24" from usize with little-endian ordering
            let [a, b, c, ..] = slot.to_le_bytes();
            self.emit_byte(a);
            self.emit_byte(b);
            self.emit_byte(c);
        }
    }

    pub fn emit_jump(&mut self, instruction: u8) -> usize {
        self.emit_byte(instruction);
        self.emit_byte(0xff_u8);
        self.emit_byte(0xff_u8);
        self.chunk.code.len() - 2
    }

    pub fn patch_jump(&mut self, offset: usize) {
        let code = &mut self.chunk.code;
        let jump = code.len() - offset - 2;

        if jump > u16::MAX as usize {
            self.short_error(CompileError::TooFarToJump);
            return;
        }

        let [a, b, ..] = jump.to_le_bytes();
        code[offset] = a;
        code[offset + 1] = b;
    }

    pub fn emit_loop(&mut self, loop_start: usize) {
        self.emit_byte(OpCode::Loop.into());

        let offset = self.chunk.code.len() - loop_start + 2;
        if offset > u16::MAX as usize {
            self.short_error(CompileError::TooFarToJump);
            return;
        }

        let [a, b, ..] = offset.to_le_bytes();
        self.emit_byte(a);
        self.emit_byte(b);
    }

    fn declaration(&mut self) {
        if self.match_token(TokenType::Var) {
            self.var_declaration(false);
        } else if self.match_token(TokenType::Let) {
            self.var_declaration(true);
        } else {
            self.statement();
        }

        if self.panic_mode {
            self.synchronize();
        }
    }

    fn var_declaration(&mut self, is_let: bool) {
        match self.parse_variable("Expect variable name.", is_let) {
            Err(e) => self.error(&format!("{}", e), e),
            Ok(slot) => {
                let identifier = self.previous_identifier();
                if self.match_token(TokenType::Equal) {
                    // Disallow assignments to let variables after the first one
                    let interned: InternedString = identifier.clone().try_into().unwrap();
                    if let Some(was_assigned) = self.vm.lets.get_mut(&interned) {
                        if *was_assigned {
                            self.short_error(CompileError::LetReassignment);
                            return;
                        }
                        *was_assigned = true;
                    }

                    self.expression();
                } else {
                    self.emit_byte(OpCode::Nil.into());
                }

                self.consume(
                    TokenType::Semicolon,
                    "Expect ';' after variable declaration",
                );

                self.define_variable(slot);
            }
        }
    }

    fn statement(&mut self) {
        if self.match_token(TokenType::Print) {
            self.print_statement();
        } else if self.match_token(TokenType::For) {
            self.for_statement();
        } else if self.match_token(TokenType::If) {
            self.if_statement();
        } else if self.match_token(TokenType::While) {
            self.while_statement();
        } else if self.match_token(TokenType::LeftBrace) {
            self.begin_scope();
            self.block();
            self.end_scope();
        } else {
            self.expression_statement();
        }
    }

    fn parse_variable(
        &mut self,
        error_message: &str,
        is_let: bool,
    ) -> Result<Option<usize>, CompileError> {
        self.consume(TokenType::Identifier, error_message);

        let identifier = self.previous_identifier();
        self.declare_variable(identifier.clone(), is_let);
        if self.scope_depth == 0 {
            return self.identifier_constant(identifier).map(Some);
        }

        Ok(None)
    }

    fn declare_variable(&mut self, name: Value, is_let: bool) {
        if self.scope_depth > 0 {
            let name = self.previous.as_ref().unwrap().lexeme.unwrap();
            if let Some(locals_indices) = self.locals_indices.get(name) {
                if let Some(index) = locals_indices.last() {
                    let local = &self.locals[*index];
                    if let Some(depth) = local.depth {
                        if depth == self.scope_depth {
                            self.short_error(CompileError::DuplicateName);
                            return;
                        }
                    }
                }
            }

            self.add_local(name);
        }

        // Mark a variable as let-declared to reason about assignment
        if is_let {
            let name_interned: InternedString = name.clone().try_into().unwrap();
            self.vm.lets.insert(name_interned, false);
        }
    }

    fn add_local(&mut self, name: &'src str) {
        if self.locals.len() == U24_MAX as usize + 1 {
            self.short_error(CompileError::TooManyLocals);
            return;
        }

        self.locals.push(Local { name, depth: None });
        let indices = self
            .locals_indices
            .entry(name)
            .or_insert_with(|| Vec::default());
        indices.push(self.locals.len() - 1);
    }

    fn define_variable(&mut self, slot: Option<usize>) {
        if self.scope_depth > 0 {
            self.mark_initialized();
            return;
        }

        self.emit_variable(
            OpCode::DefineGlobal,
            OpCode::DefineGlobalLong,
            slot.unwrap(),
        );
    }

    fn mark_initialized(&mut self) {
        if self.scope_depth == 0 {
            return;
        }

        self.locals.last_mut().unwrap().depth = Some(self.scope_depth);
    }

    fn print_statement(&mut self) {
        self.expression();
        self.consume(TokenType::Semicolon, "Expect ';' after value.");
        self.emit_byte(OpCode::Print.into());
    }

    fn for_statement(&mut self) {
        self.begin_scope();

        self.consume(TokenType::LeftParen, "Expect '(' after 'for'.");

        // Compile initializer
        if self.match_token(TokenType::Semicolon) {
            // No initializer
        } else if self.match_token(TokenType::Var) {
            self.var_declaration(false);
        } else {
            self.expression_statement();
        }

        let mut loop_start = self.chunk.code.len();

        let exit_jump = if !self.match_token(TokenType::Semicolon) {
            self.expression();
            self.consume(TokenType::Semicolon, "Expect ';' after loop condition.");

            let exit_jump = self.emit_jump(OpCode::JumpIfFalse.into());
            // Pop 'condition' off the stack, before executing the body of the for
            self.emit_byte(OpCode::Pop.into());
            Some(exit_jump)
        } else {
            None
        };

        if !self.match_token(TokenType::RightParen) {
            let body_jump = self.emit_jump(OpCode::Jump.into());

            let increment_start = self.chunk.code.len();

            self.expression();
            self.emit_byte(OpCode::Pop.into());
            self.consume(TokenType::RightParen, "Expect ')' after for clauses.");

            self.emit_loop(loop_start);
            loop_start = increment_start;
            self.patch_jump(body_jump);
        }

        // Compile for body
        self.statement();
        // After the body, add loop instruction to jump back to 'loop_start'
        self.emit_loop(loop_start);

        // If there was a condition, patch the exit jump
        if let Some(exit_jump) = exit_jump {
            self.patch_jump(exit_jump);
            self.emit_byte(OpCode::Pop.into());
        }

        self.end_scope();
    }

    fn if_statement(&mut self) {
        self.consume(TokenType::LeftParen, "Expect '(' after 'if'.");
        self.expression();
        self.consume(TokenType::RightParen, "Expect ')' after condition.");

        // Insert jump after condition to skip to 'else' branch
        let then_jump = self.emit_jump(OpCode::JumpIfFalse.into());

        // Pop if 'condition' off the stack
        self.emit_byte(OpCode::Pop.into());
        // Compile 'then' branch
        self.statement();
        // Insert jump after 'then' branch to skip past 'else' branch
        let else_jump = self.emit_jump(OpCode::Jump.into());

        // Back patch 'then' jump offset, after 'then' branch and 'else' jump code is compiled
        self.patch_jump(then_jump);

        // Pop 'condition' off the stack, in case 'then' branch was jumped
        self.emit_byte(OpCode::Pop.into());
        // Compile 'else' branch, if any
        if self.match_token(TokenType::Else) {
            self.statement();
        }

        // Back patch 'else' jump offset, after 'else' branch code is compiled
        self.patch_jump(else_jump);
    }

    fn while_statement(&mut self) {
        // Marks beginning of while construct
        let loop_start = self.chunk.code.len();

        self.consume(TokenType::LeftParen, "Expect '(' after 'while'.");
        self.expression();
        self.consume(TokenType::RightParen, "Expect ')' after condition.");

        let exit_jump = self.emit_jump(OpCode::JumpIfFalse.into());

        // Pop while 'condition' off the stack
        self.emit_byte(OpCode::Pop.into());
        // Compile while body
        self.statement();
        // After the body, add loop instruction to jump back to 'loop_start'
        self.emit_loop(loop_start);

        // Back patch while 'exit' jump offset
        self.patch_jump(exit_jump);
        // Pop while 'condition' off the stack, when exiting if
        self.emit_byte(OpCode::Pop.into());
    }

    fn block(&mut self) {
        while !self.check_token(TokenType::RightBrace) && !self.check_token(TokenType::Eof) {
            self.declaration();
        }

        self.consume(TokenType::RightBrace, "Expect '}' after block.");
    }

    fn expression_statement(&mut self) {
        self.expression();
        self.consume(TokenType::Semicolon, "Expect ';' after expression.");
        self.emit_byte(OpCode::Pop.into());
    }

    fn begin_scope(&mut self) {
        self.scope_depth += 1;
    }

    fn end_scope(&mut self) {
        self.scope_depth -= 1;

        // Pop all local variables at the scope just ended
        while !self.locals.is_empty() {
            if let Some(last) = self.locals.last() {
                if last.depth.unwrap() > self.scope_depth {
                    self.locals_indices.entry(last.name).and_modify(|indices| {
                        indices.pop();
                    });
                    self.locals.pop();

                    // TODO: Add POPN instruction to pop n elements from the stack
                    self.emit_byte(OpCode::Pop.into());
                } else {
                    break;
                }
            } else {
                break;
            }
        }
    }

    fn check_token(&self, token_type: TokenType) -> bool {
        self.current
            .iter()
            .all(|token| token.token_type == token_type)
    }

    fn synchronize(&mut self) {
        self.panic_mode = false;

        while !self.check_token(TokenType::Eof) {
            if self.previous.as_ref().unwrap().token_type == TokenType::Semicolon {
                return;
            }
            match self.current.as_ref().unwrap().token_type {
                TokenType::Class
                | TokenType::Fun
                | TokenType::Let
                | TokenType::Var
                | TokenType::For
                | TokenType::If
                | TokenType::While
                | TokenType::Print
                | TokenType::Return => return,
                _ => (),
            }

            self.advance();
        }
    }

    fn error_at_current(&mut self, message: &str, error: CompileError) {
        if self.panic_mode {
            return;
        }
        report_error(message, self.current.as_ref().unwrap());
        self.first_error = self.first_error.or(Some(error));
        self.panic_mode = true;
    }

    fn error(&mut self, message: &str, error: CompileError) {
        if self.panic_mode {
            return;
        }
        report_error(message, self.previous.as_ref().unwrap());
        self.first_error = Some(error);
        self.first_error = self.first_error.or(Some(error));
        self.panic_mode = true;
    }

    pub fn short_error(&mut self, error: CompileError) {
        self.error(&error.to_string(), error);
    }

    fn end(mut self) -> CompilerResult {
        self.emit_return();
        #[cfg(feature = "dump")]
        {
            if self.first_error.is_none() {
                debug::disassemble_chunk(self.vm, &self.chunk, "program");
            }
        }
        match self.first_error {
            Some(ce) => Err(ce),
            None => Ok(self.chunk),
        }
    }
}

fn report_error(message: &str, token: &Token) {
    eprint!("[line {}] Error ", token.line);
    match token.token_type {
        TokenType::Eof => eprint!(" at end"),
        tt if TokenType::error_message(tt).is_some() => (),
        _ => eprint!("at '{}'", token.lexeme.unwrap()),
    }
    eprintln!(": {}", message);
}

pub(crate) fn compile(vm: &mut VM, source: &str) -> CompilerResult {
    let scanner = Scanner::new(source);
    let mut compiler = Compiler::new(vm, scanner);
    compiler.advance();
    while !compiler.match_token(TokenType::Eof) {
        compiler.declaration();
    }
    compiler.end()
}
