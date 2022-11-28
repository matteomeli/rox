use fnv::FnvHashMap;

#[allow(unused_imports)]
use crate::{
    chunk::{Chunk, OpCode, U24_MAX},
    debug,
    parser::{get_rule, Precedence},
    scanner::{Scanner, Token, TokenType},
    value::{create_string, format_function_name, Function, FunctionType, InternedString, Value},
    vm::{manage, CompileError, VM},
};

type CompilerResult = Result<Function, CompileError>;

pub struct Local<'src> {
    pub name: &'src str,
    pub depth: Option<usize>,
}

pub struct FunctionCompiler<'src> {
    function: Function,
    function_type: FunctionType,
    pub locals_indices: FnvHashMap<&'src str, Vec<usize>>,
    pub locals: Vec<Local<'src>>,
    scope_depth: usize,
    enclosing: Option<Box<FunctionCompiler<'src>>>,
}

impl<'src> FunctionCompiler<'src> {
    pub fn new(vm: &mut VM, function_type: FunctionType) -> Self {
        let function = Function::new(vm, None, 0);
        let locals = vec![Local {
            name: "",
            depth: Some(0),
        }];
        let mut locals_indices = FnvHashMap::default();
        locals_indices.insert("", vec![0]);

        FunctionCompiler {
            function,
            function_type,
            locals_indices,
            locals,
            scope_depth: 0,
            enclosing: None,
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
}

pub struct Compiler<'src, 'vm> {
    pub vm: &'vm mut VM,
    scanner: Scanner<'src>,
    pub previous: Option<Token<'src>>,
    current: Option<Token<'src>>,
    first_error: Option<CompileError>,
    panic_mode: bool,
    pub fc: FunctionCompiler<'src>,
}

impl<'src, 'vm> Compiler<'src, 'vm> {
    pub fn new(vm: &'vm mut VM, scanner: Scanner<'src>) -> Self {
        let fc = FunctionCompiler::new(vm, FunctionType::Script);
        Compiler {
            vm,
            scanner,
            previous: None,
            current: None,
            first_error: None,
            panic_mode: false,
            fc,
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

    pub fn check_token(&self, token_type: TokenType) -> bool {
        self.current
            .iter()
            .all(|token| token.token_type == token_type)
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
        let name = self.previous.as_ref().unwrap().lexeme.unwrap();
        create_string(self.vm, name).into()
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

    pub fn argument_list(&mut self) -> usize {
        let mut arg_count = 0;
        if !self.check_token(TokenType::RightParen) {
            loop {
                self.expression();
                if arg_count == 255 {
                    self.short_error(CompileError::TooManyArguments)
                }
                arg_count += 1;
                if !self.match_token(TokenType::Comma) {
                    break;
                }
            }
        }

        self.consume(TokenType::RightParen, "Expected ')' after arguments.");

        arg_count
    }

    pub fn emit_byte(&mut self, byte: u8) {
        let line = self.previous.as_ref().unwrap().line;
        self.current_chunk_mut().write(byte, line);
    }

    pub fn emit_byte_with_line(&mut self, byte: u8, line: u32) {
        self.current_chunk_mut().write(byte, line);
    }

    pub fn emit_bytes(&mut self, byte1: u8, byte2: u8) {
        self.emit_byte(byte1);
        self.emit_byte(byte2);
    }

    pub fn emit_return(&mut self) {
        self.emit_byte(OpCode::Nil.into());
        self.emit_byte(OpCode::Return.into());
    }

    pub fn emit_constant(&mut self, value: Value) {
        let line = self.previous.as_ref().unwrap().line;
        if self
            .current_chunk_mut()
            .write_constant(value, line)
            .is_err()
        {
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
        self.current_chunk_mut().code.len() - 2
    }

    pub fn patch_jump(&mut self, offset: usize) {
        let code = &mut self.current_chunk_mut().code;
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

        let offset = self.current_chunk_mut().code.len() - loop_start + 2;
        if offset > u16::MAX as usize {
            self.short_error(CompileError::TooFarToJump);
            return;
        }

        let [a, b, ..] = offset.to_le_bytes();
        self.emit_byte(a);
        self.emit_byte(b);
    }

    pub fn error_at_current(&mut self, message: &str, error: CompileError) {
        if self.panic_mode {
            return;
        }
        report_error(message, self.current.as_ref().unwrap());
        self.first_error = self.first_error.or(Some(error));
        self.panic_mode = true;
    }

    pub fn short_error_at_current(&mut self, error: CompileError) {
        self.error_at_current(&error.to_string(), error);
    }

    pub fn error(&mut self, message: &str, error: CompileError) {
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

    fn declaration(&mut self) {
        if self.match_token(TokenType::Fun) {
            self.fun_declaration();
        } else if self.match_token(TokenType::Var) {
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
            Err(ce) => self.error(&format!("{}", ce), ce),
            Ok(slot) => {
                let identifier = self.previous_identifier();
                if self.match_token(TokenType::Equal) {
                    // Disallow assignments to let variables after the first one
                    let interned: InternedString = identifier.try_into().unwrap();
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
        } else if self.match_token(TokenType::Return) {
            self.return_statement();
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
        if self.fc.scope_depth == 0 {
            return self.identifier_constant(identifier).map(Some);
        }

        Ok(None)
    }

    fn declare_variable(&mut self, name: Value, is_let: bool) {
        if self.fc.scope_depth > 0 {
            let name = self.previous.as_ref().unwrap().lexeme.unwrap();
            if let Some(locals_indices) = self.fc.locals_indices.get(name) {
                if let Some(index) = locals_indices.last() {
                    let local = &self.fc.locals[*index];
                    if let Some(depth) = local.depth {
                        if depth == self.fc.scope_depth {
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
            let name_interned: InternedString = name.try_into().unwrap();
            self.vm.lets.insert(name_interned, false);
        }
    }

    fn add_local(&mut self, name: &'src str) {
        if self.fc.locals.len() == U24_MAX as usize + 1 {
            self.short_error(CompileError::TooManyLocals);
            return;
        }

        self.fc.locals.push(Local { name, depth: None });
        let indices = self
            .fc
            .locals_indices
            .entry(name)
            .or_insert_with(Vec::default);
        indices.push(self.fc.locals.len() - 1);
    }

    fn define_variable(&mut self, slot: Option<usize>) {
        if self.fc.scope_depth > 0 {
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
        if self.fc.scope_depth == 0 {
            return;
        }

        self.fc.locals.last_mut().unwrap().depth = Some(self.fc.scope_depth);
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

        let mut loop_start = self.current_chunk_mut().code.len();

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

            let increment_start = self.current_chunk_mut().code.len();

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
        let loop_start = self.current_chunk_mut().code.len();

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

    fn return_statement(&mut self) {
        if self.fc.function_type == FunctionType::Script {
            self.short_error(CompileError::ReturnAtTopLevel);
            return;
        }

        if self.match_token(TokenType::Semicolon) {
            self.emit_return();
        } else {
            self.expression();
            self.consume(TokenType::Semicolon, "Expect ';' after return value.");
            self.emit_byte(OpCode::Return.into());
        }
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

    fn fun_declaration(&mut self) {
        match self.parse_variable("Expect function name.", false) {
            Err(ce) => self.error(&format!("{}", ce), ce),
            Ok(global) => {
                self.mark_initialized();
                self.function(FunctionType::Function);
                self.define_variable(global);
            }
        }
    }

    fn function(&mut self, function_type: FunctionType) {
        self.begin_compiler(function_type);
        self.begin_scope();

        self.consume(TokenType::LeftParen, "Expect '(' after function name.");
        // TODO: Parse function parameters
        if !self.check_token(TokenType::RightParen) {
            loop {
                self.fc.function.arity += 1;
                if self.fc.function.arity > 255 {
                    self.short_error_at_current(CompileError::TooManyParameters);
                }

                match self.parse_variable("Expect parameter name.", false) {
                    Err(ce) => {
                        self.error(&format!("{}", ce), ce);
                        break;
                    }
                    Ok(constant) => self.define_variable(constant),
                }

                if !self.match_token(TokenType::Comma) {
                    break;
                }
            }
        }
        self.consume(TokenType::RightParen, "Expect ')' after parameters.");
        self.consume(TokenType::LeftBrace, "Expect '{' before function body.");

        self.block();

        let function = self.end_compiler();
        let function_value = manage(self.vm, function).into();
        self.emit_constant(function_value);
    }

    fn begin_compiler(&mut self, function_type: FunctionType) {
        let new_compiler = FunctionCompiler::new(self.vm, function_type);
        let old_compiler = std::mem::replace(&mut self.fc, new_compiler);
        self.fc.enclosing = Some(Box::new(old_compiler));

        if function_type != FunctionType::Script {
            let function_name = self.previous.as_ref().unwrap().lexeme.unwrap();
            self.fc.function.name = Some(create_string(self.vm, function_name));
        }
    }

    fn end_compiler(&mut self) -> Function {
        self.emit_return();

        #[cfg(feature = "dump")]
        {
            if self.first_error.is_none() {
                let function_name = format_function_name(&self.fc.function);
                debug::disassemble_chunk(self.vm, &self.fc.function.chunk, &function_name);
            }
        }

        let new_compiler = *self.fc.enclosing.take().unwrap();
        let old_compiler = std::mem::replace(&mut self.fc, new_compiler);
        old_compiler.function
    }

    fn begin_scope(&mut self) {
        self.fc.scope_depth += 1;
    }

    fn end_scope(&mut self) {
        self.fc.scope_depth -= 1;

        // Pop all local variables at the scope just ended
        while !self.fc.locals.is_empty() {
            if let Some(last) = self.fc.locals.last() {
                if last.depth.unwrap() > self.fc.scope_depth {
                    self.fc
                        .locals_indices
                        .entry(last.name)
                        .and_modify(|indices| {
                            indices.pop();
                        });
                    self.fc.locals.pop();

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

    fn end(mut self) -> CompilerResult {
        self.emit_return();

        #[cfg(feature = "dump")]
        {
            if self.first_error.is_none() {
                let function_name = format_function_name(&self.fc.function);
                debug::disassemble_chunk(self.vm, &self.fc.function.chunk, &function_name);
            }
        }

        match self.first_error {
            Some(ce) => Err(ce),
            None => Ok(self.fc.function),
        }
    }

    fn current_chunk_mut(&mut self) -> &mut Chunk {
        &mut self.fc.function.chunk
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

pub fn compile(vm: &mut VM, source: &str) -> CompilerResult {
    let scanner = Scanner::new(source);
    let mut compiler = Compiler::new(vm, scanner);
    compiler.advance();
    while !compiler.match_token(TokenType::Eof) {
        compiler.declaration();
    }
    compiler.end()
}
