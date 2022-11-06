use crate::{
    chunk::{Chunk, OpCode},
    debug,
    parser::{get_rule, Precedence},
    scanner::{Scanner, Token, TokenType},
    value::Value,
    vm::CompileError,
};

type CompilerResult = Result<Chunk, CompileError>;

pub struct Compiler<'src> {
    scanner: Scanner<'src>,
    pub(crate) previous: Option<Token<'src>>,
    current: Option<Token<'src>>,
    first_error: Option<CompileError>,
    panic_mode: bool,
    chunk: Chunk,
}

impl<'src> Compiler<'src> {
    pub fn new(scanner: Scanner<'src>) -> Self {
        Compiler {
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

        // Look up a prefix parser for the current token
        match get_rule(self.previous.as_ref().unwrap().token_type).prefix {
            Some(rule) => rule(self),
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
                .unwrap()(self);
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
                debug::disassemble_chunk(&self.chunk, "code");
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
        match self.chunk.write_constant(value, line) {
            Ok(_) => (),
            Err(CompileError::TooManyConstants) => {
                let message: &str = &format!("{}", CompileError::TooManyConstants);
                self.error(message, CompileError::TooManyConstants);
            }
            _ => unimplemented!(),
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

pub(crate) fn compile(source: &str) -> CompilerResult {
    let scanner = Scanner::new(source);
    let mut compiler = Compiler::new(scanner);
    compiler.advance();
    compiler.expression();
    compiler.consume(TokenType::Eof, "Expected end of expression.");
    compiler.end()
}
