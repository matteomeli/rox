use crate::{
    chunk::{Chunk, OpCode},
    debug,
    parser::{get_rule, Precedence},
    scanner::{Scanner, Token, TokenType},
    value::{create_string, InternedString, Value},
    vm::{CompileError, VM},
};

type CompilerResult = Result<Chunk, CompileError>;

pub struct Compiler<'src, 'vm> {
    pub vm: &'vm mut VM,
    scanner: Scanner<'src>,
    pub(crate) previous: Option<Token<'src>>,
    current: Option<Token<'src>>,
    first_error: Option<CompileError>,
    panic_mode: bool,
    chunk: Chunk,
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

    pub fn consume(&mut self, token_type: TokenType, message: &str) {
        if let Some(token) = &self.current {
            if token.token_type == token_type {
                self.advance();
                return;
            }
        }

        self.error_at_current(message, CompileError::ParseError);
    }

    fn declaration(&mut self) {
        if self.match_token(TokenType::Var) {
            self.var_declaration();
        } else {
            self.statement();
        }

        if self.panic_mode {
            self.synchronize();
        }
    }

    fn statement(&mut self) {
        if self.match_token(TokenType::Print) {
            self.print_statement();
        } else {
            self.expression_statement();
        }
    }

    fn print_statement(&mut self) {
        self.expression();
        self.consume(TokenType::Semicolon, "Expect ';' after value.");
        self.emit_byte(OpCode::Print.into());
    }

    fn expression_statement(&mut self) {
        self.expression();
        self.consume(TokenType::Semicolon, "Expect ';' after expression.");
        self.emit_byte(OpCode::Pop.into());
    }

    fn var_declaration(&mut self) {
        match self.parse_variable("Expect variable name.") {
            Err(e) => self.error(&format!("{}", e), e),
            Ok(global) => {
                if self.match_token(TokenType::Equal) {
                    self.expression();
                } else {
                    self.emit_byte(OpCode::Nil.into());
                }

                self.consume(
                    TokenType::Semicolon,
                    "Expect ';' after variable declaration",
                );

                self.define_variable(global);
            }
        }
    }

    pub fn previous_identifier(&mut self) -> Value {
        let name = &self.previous.as_ref().unwrap().lexeme.unwrap();
        let vm = &mut self.vm;
        create_string(vm, name).into()
    }

    pub fn identifier_constant(&mut self, name: Value) -> Result<usize, CompileError> {
        let interned: InternedString = name.clone().try_into().unwrap();
        match self.vm.globals.get(&interned) {
            Some(Value::Number(constant)) => Ok(*constant as usize),
            None => {
                let index = self.vm.global_values.len();
                self.vm.global_values.push(Value::Undefined);
                self.vm.global_names.push(name);
                self.vm.globals.insert(interned, (index as f64).into());
                Ok(index)
            }
            _ => unimplemented!(),
        }
    }

    fn parse_variable(&mut self, error_message: &str) -> Result<usize, CompileError> {
        self.consume(TokenType::Identifier, error_message);
        let identifier = self.previous_identifier();
        self.identifier_constant(identifier)
    }

    fn define_variable(&mut self, global: usize) {
        // TODO: This only allows for globals "constants" less than 255, doesn't account for 24 bit long constant addressable range.
        self.emit_bytes(OpCode::DefineGlobal.into(), global as u8);
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

    fn check_token(&self, token_type: TokenType) -> bool {
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

    fn end(mut self) -> CompilerResult {
        self.emit_return();
        #[cfg(feature = "dump")]
        {
            if self.first_error.is_none() {
                debug::disassemble_chunk(self.vm, &self.chunk, "code");
            }
        }
        match self.first_error {
            Some(ce) => Err(ce),
            None => Ok(self.chunk),
        }
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
