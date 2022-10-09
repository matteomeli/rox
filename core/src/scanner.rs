use unicode_segmentation::UnicodeSegmentation;

use std::error::Error;
use std::fmt;

use crate::token::{Token, TokenType};
use crate::types::Literal;

pub type ScanResult<T> = std::result::Result<T, ScanError>;

pub struct Scanner {
    source: String,
    tokens: Vec<Token>,
    start: usize,
    current: usize,
    line: u32,
}

impl Scanner {
    pub fn new(source: String) -> Self {
        Scanner {
            source,
            tokens: Vec::new(),
            start: 0,
            current: 0,
            line: 1,
        }
    }

    pub fn scan_tokens(&mut self) -> ScanResult<&[Token]> {
        while !self.is_at_end() {
            self.start = self.current;
            self.scan_token()?
        }

        self.tokens
            .push(Token::new(TokenType::Eof, "".to_string(), None, self.line));

        Ok(self.tokens())
    }

    pub fn tokens(&self) -> &[Token] {
        self.tokens.as_slice()
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.source.graphemes(true).count()
    }

    fn scan_token(&mut self) -> ScanResult<()> {
        match self.advance() {
            // Single character tokens
            "(" => self.add_token(TokenType::LeftParen, None),
            ")" => self.add_token(TokenType::RightParen, None),
            "{" => self.add_token(TokenType::LeftBrace, None),
            "}" => self.add_token(TokenType::RightBrace, None),
            "," => self.add_token(TokenType::Comma, None),
            "." => self.add_token(TokenType::Dot, None),
            "-" => self.add_token(TokenType::Minus, None),
            "+" => self.add_token(TokenType::Plus, None),
            ";" => self.add_token(TokenType::Semicolon, None),
            "*" => self.add_token(TokenType::Star, None),

            // One or two character tokens
            "!" => {
                if self.matches("=") {
                    self.add_token(TokenType::NotEqual, None)
                } else {
                    self.add_token(TokenType::Bang, None)
                }
            }
            "=" => {
                if self.matches("=") {
                    self.add_token(TokenType::EqualEqual, None)
                } else {
                    self.add_token(TokenType::Equal, None)
                }
            }
            ">" => {
                if self.matches("=") {
                    self.add_token(TokenType::GreaterEqual, None)
                } else {
                    self.add_token(TokenType::Greater, None)
                }
            }
            "<" => {
                if self.matches("=") {
                    self.add_token(TokenType::LessEqual, None)
                } else {
                    self.add_token(TokenType::Less, None)
                }
            }

            // Comments
            "/" => {
                if self.matches("/") {
                    while let Some(str) = self.peek() {
                        // A comment goes until the end of the line
                        if str == "\n" || str == "\r\n" || self.is_at_end() {
                            break;
                        }

                        self.advance();
                    }
                    Ok(())
                } else if self.matches("*") {
                    let mut opened = 1u32;

                    while !self.is_at_end() {
                        match (self.peek(), self.peek_next()) {
                            // A block comment goes until a "*/" pair is found
                            (Some("*"), Some("/")) => {
                                self.advance();
                                self.advance();
                                opened -= 1;
                                if opened == 0 {
                                    break;
                                }
                            }
                            // Handle nested block comments
                            (Some("/"), Some("*")) => {
                                self.advance();
                                self.advance();
                                opened += 1;
                            }
                            (Some("\n"), _) | (Some("\r\n"), _) => {
                                self.line += 1;
                                self.advance();
                            }
                            _ => {
                                self.advance();
                            }
                        }
                    }

                    // If no opened block comments left we are ok
                    if opened == 0 {
                        Ok(())
                    } else {
                        Err(ScanError::new(
                            self.line,
                            ScanErrorKind::UnterminatedBlockComment,
                        ))
                    }
                } else {
                    self.add_token(TokenType::Slash, None)
                }
            }

            // Ignore whitespace
            " " | "\r" | "\t" => Ok(()),

            // Newline
            "\n" | "\r\n" => {
                self.line += 1;
                Ok(())
            }

            r#"""# => self.string(),

            str => {
                if Scanner::is_digit(str) {
                    self.number()
                } else if Scanner::is_alpha(str) {
                    self.identifier()
                } else {
                    Err(ScanError::new(
                        self.line,
                        ScanErrorKind::UnexpcetedCharacter,
                    ))
                }
            }
        }
    }

    fn number(&mut self) -> ScanResult<()> {
        while let Some(str) = self.peek() {
            if !Scanner::is_digit(str) {
                break;
            }
            self.advance();
        }

        // Look for a fractional part
        if let Some(".") = self.peek() {
            if let Some(str) = self.peek_next() {
                if Scanner::is_digit(str) {
                    // Consume .
                    self.advance();

                    while let Some(str) = self.peek() {
                        if !Scanner::is_digit(str) {
                            break;
                        }
                        self.advance();
                    }
                }
            }
        }

        let literal_length = self.current - self.start;
        let number_literal = Literal::Number(
            self.source
                .graphemes(true)
                .skip(self.start)
                .take(literal_length)
                .collect::<String>()
                .parse::<f64>()
                .unwrap(),
        );
        self.add_token(TokenType::Number, Some(number_literal))
    }

    fn identifier(&mut self) -> ScanResult<()> {
        while let Some(str) = self.peek() {
            if !Scanner::is_alphanumeric(str) {
                break;
            }
            self.advance();
        }

        let identifier_length = self.current - self.start;
        let identifier = self
            .source
            .graphemes(true)
            .skip(self.start)
            .take(identifier_length)
            .collect::<String>();

        let token_type = match identifier.as_str() {
            "and" => TokenType::And,
            "class" => TokenType::Class,
            "else" => TokenType::Else,
            "false" => TokenType::False,
            "for" => TokenType::For,
            "fun" => TokenType::Fun,
            "if" => TokenType::If,
            "nil" => TokenType::Nil,
            "or" => TokenType::Or,
            "print" => TokenType::Print,
            "return" => TokenType::Return,
            "super" => TokenType::Super,
            "this" => TokenType::This,
            "true" => TokenType::True,
            "var" => TokenType::Var,
            "while" => TokenType::While,
            "break" => TokenType::Break,
            "continue" => TokenType::Continue,
            _ => TokenType::Identifier,
        };

        self.add_token(token_type, None)
    }

    fn is_digit(str: &str) -> bool {
        str.bytes().all(|c| c.is_ascii_digit())
    }

    fn is_alpha(str: &str) -> bool {
        str.bytes().all(|c| c.is_ascii_alphabetic()) || str == "_"
    }

    fn is_alphanumeric(str: &str) -> bool {
        str.bytes().all(|c| c.is_ascii_alphanumeric()) || str == "_"
    }

    fn string(&mut self) -> ScanResult<()> {
        while let Some(str) = self.peek() {
            if str == r#"""# || self.is_at_end() {
                break;
            }

            if let Some("\n") = self.peek() {
                self.line += 1;
            }

            self.advance();
        }

        if self.is_at_end() {
            return Err(ScanError::new(self.line, ScanErrorKind::UnterminatedString));
        }

        // Consume the closing "
        self.advance();

        // Trim surrounding quotes
        let literal_length = (self.current - 1) - (self.start + 1);
        let string_literal = Literal::String(
            self.source
                .graphemes(true)
                .skip(self.start + 1)
                .take(literal_length)
                .collect(),
        );
        self.add_token(TokenType::String, Some(string_literal))
    }

    fn peek(&self) -> Option<&str> {
        self.source.graphemes(true).nth(self.current)
    }

    fn peek_next(&self) -> Option<&str> {
        self.source.graphemes(true).nth(self.current + 1)
    }

    fn matches(&mut self, expected: &str) -> bool {
        if self.is_at_end() {
            return false;
        }

        if let Some(str) = self.source.graphemes(true).nth(self.current) {
            if str != expected {
                return false;
            }
        }

        self.current += 1;

        true
    }

    fn advance(&mut self) -> &str {
        // SAFETY: By construction advance() is only called after checking if is_at_end()
        let grapheme = unsafe {
            self.source
                .graphemes(true)
                .nth(self.current)
                .unwrap_unchecked()
        };
        self.current += 1;
        grapheme
    }

    fn add_token(&mut self, token_type: TokenType, literal: Option<Literal>) -> ScanResult<()> {
        let lexeme_length = self.current - self.start;
        let lexeme = self
            .source
            .graphemes(true)
            .skip(self.start)
            .take(lexeme_length)
            .collect();
        self.tokens
            .push(Token::new(token_type, lexeme, literal, self.line));
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct ScanError {
    line: u32,
    kind: ScanErrorKind,
}

impl ScanError {
    pub fn new(line: u32, kind: ScanErrorKind) -> Self {
        ScanError { line, kind }
    }
}

impl Error for ScanError {}

impl fmt::Display for ScanError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[line {}] Error: {}", self.line, self.kind)
    }
}

#[derive(Debug, Clone)]
pub enum ScanErrorKind {
    UnexpcetedCharacter,
    UnterminatedString,
    UnterminatedBlockComment,
}

impl fmt::Display for ScanErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self {
            Self::UnexpcetedCharacter => write!(f, "Unexpected character."),
            Self::UnterminatedString => write!(f, "Unterminated string."),
            Self::UnterminatedBlockComment => write!(f, "Unterminated block comment."),
        }
    }
}

// TODO: Add unit tests
