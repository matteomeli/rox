use std::{iter::Peekable, str::CharIndices};

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum TokenType {
    // Single character tokens
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,

    // One or two character tokens
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    // Literals
    Identifier,
    String,
    Number,

    // Keywords
    And,
    Class,
    Else,
    False,
    For,
    Fun,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,
    Break,
    Continue,

    // Special synthetic toekns to signal compile error
    UnexpectedCharacterError,
    UnterminatedStringError,

    // End of file marker
    Eof,
}

impl TokenType {
    pub fn error_message(token_type: TokenType) -> Option<&'static str> {
        match token_type {
            TokenType::UnexpectedCharacterError => Some("Unexpected character."),
            TokenType::UnterminatedStringError => Some("Unterminated string."),
            _ => None,
        }
    }
}

pub struct Token<'a> {
    pub(crate) token_type: TokenType,
    pub(crate) lexeme: Option<&'a str>,
    pub(crate) line: u32,
}

impl<'a> Token<'a> {
    pub fn new(token_type: TokenType, lexeme: Option<&'a str>, line: u32) -> Self {
        Token {
            token_type,
            lexeme,
            line,
        }
    }
}

fn is_digit(oc: Option<char>) -> bool {
    if let Some(c) = oc {
        return c.is_numeric();
    }
    false
}

fn is_alpha(oc: Option<char>) -> bool {
    if let Some(c) = oc {
        return c.is_alphabetic() || c == '_';
    }
    false
}

fn check_keyword(word: &str, keyword: &str, offset: usize, token_type: TokenType) -> TokenType {
    if word[offset..] == keyword[offset..] {
        token_type
    } else {
        TokenType::Identifier
    }
}

pub struct Scanner<'a> {
    source: &'a str,
    token_start: usize,
    chars: Peekable<CharIndices<'a>>,
    line: u32,
}

impl<'a> Scanner<'a> {
    pub fn new(source: &'a str) -> Self {
        Scanner {
            source,
            token_start: 0,
            chars: source.char_indices().peekable(),
            line: 1,
        }
    }

    pub fn scan_token(&mut self) -> Token<'a> {
        self.skip_whitespace();

        self.token_start = self.current();
        let oc = self.advance();

        if is_alpha(oc) {
            return self.identifier();
        }

        if is_digit(oc) {
            return self.number_literal();
        }

        match oc {
            None => self.make_token(TokenType::Eof),
            Some(c) => match c {
                '(' => self.make_token(TokenType::LeftParen),
                ')' => self.make_token(TokenType::RightParen),
                '{' => self.make_token(TokenType::LeftBrace),
                '}' => self.make_token(TokenType::RightBrace),
                ';' => self.make_token(TokenType::Semicolon),
                ',' => self.make_token(TokenType::Comma),
                '.' => self.make_token(TokenType::Dot),
                '-' => self.make_token(TokenType::Minus),
                '+' => self.make_token(TokenType::Plus),
                '/' => self.make_token(TokenType::Slash),
                '*' => self.make_token(TokenType::Star),
                '!' => {
                    if self.maybe_match('=') {
                        self.make_token(TokenType::BangEqual)
                    } else {
                        self.make_token(TokenType::Bang)
                    }
                }
                '=' => {
                    if self.maybe_match('=') {
                        self.make_token(TokenType::EqualEqual)
                    } else {
                        self.make_token(TokenType::Equal)
                    }
                }
                '<' => {
                    if self.maybe_match('=') {
                        self.make_token(TokenType::LessEqual)
                    } else {
                        self.make_token(TokenType::Less)
                    }
                }
                '>' => {
                    if self.maybe_match('=') {
                        self.make_token(TokenType::GreaterEqual)
                    } else {
                        self.make_token(TokenType::Greater)
                    }
                }
                '"' => self.string_literal(),
                _ => self.make_token(TokenType::UnexpectedCharacterError),
            },
        }
    }

    fn advance(&mut self) -> Option<char> {
        self.chars.next().map(|(_index, ch)| ch)
    }

    fn current(&mut self) -> usize {
        self.chars
            .peek()
            .map(|(index, _ch)| *index)
            .unwrap_or(self.source.len())
    }

    fn content(&mut self) -> &'a str {
        let current = self.current();
        &self.source[self.token_start..current]
    }

    fn make_token(&mut self, token_type: TokenType) -> Token<'a> {
        Token::new(token_type, Some(self.content()), self.line)
    }

    fn maybe_match(&mut self, expected: char) -> bool {
        match self.chars.peek().map(|(_index, ch)| *ch) {
            None => false,
            Some(ch) => {
                if ch != expected {
                    return false;
                }
                let _ = self.advance();
                true
            }
        }
    }

    fn maybe_match_str(&mut self, expected: &str) -> bool {
        let strlen = expected.len();
        let start = match self.chars.peek() {
            None => return false,
            Some((index, _ch)) => *index,
        };

        let end = start + strlen;
        if end > self.source.len() {
            return false;
        }

        if expected == &self.source[start..end] {
            for _ in 0..expected.chars().count() {
                self.chars.next();
            }
            return true;
        }

        false
    }

    fn skip_whitespace(&mut self) {
        loop {
            match self.chars.peek().map(|(_index, ch)| *ch) {
                Some(' ') | Some('\r') | Some('\t') => {
                    self.advance();
                }
                Some('\n') => {
                    self.line += 1;
                    self.advance();
                    break;
                }
                Some('/') => {
                    if self.maybe_match_str("//") {
                        while let Some((_, c)) = self.chars.peek() {
                            if *c == '\n' {
                                break;
                            }
                            self.advance();
                        }
                    } else {
                        return;
                    }
                }
                _ => return,
            };
        }
    }

    fn string_literal(&mut self) -> Token<'a> {
        loop {
            match self.chars.peek().map(|(_index, ch)| *ch) {
                Some('"') => {
                    self.advance();
                    return self.make_token(TokenType::String);
                }
                Some(c) => {
                    if c == '\n' {
                        self.line += 1;
                    }
                    self.advance();
                }
                None => return self.make_token(TokenType::UnterminatedStringError),
            }
        }
    }

    fn consume_digits(&mut self) {
        while match self.chars.peek().map(|(_index, ch)| *ch) {
            Some(c) => is_digit(Some(c)),
            None => false,
        } {
            self.advance();
        }
    }

    fn number_literal(&mut self) -> Token<'a> {
        self.consume_digits();

        let mut chars = self.chars.clone();
        if let Some('.') = chars.next().map(|(_index, ch)| ch) {
            if let Some(c) = chars.next().map(|(_index, ch)| ch) {
                if is_digit(Some(c)) {
                    self.advance();
                    self.consume_digits();
                }
            }
        }
        self.make_token(TokenType::Number)
    }

    fn identifier(&mut self) -> Token<'a> {
        while match self.chars.peek().map(|(_index, ch)| *ch) {
            Some(c) => is_digit(Some(c)) || is_alpha(Some(c)),
            None => false,
        } {
            self.advance();
        }

        let token_tyoe = self.identifier_type();
        self.make_token(token_tyoe)
    }

    fn identifier_type(&mut self) -> TokenType {
        let word = self.content();
        if word.is_empty() {
            return TokenType::Identifier;
        }
        match &word[..1] {
            "a" => check_keyword(word, "and", 1, TokenType::And),
            "c" => check_keyword(word, "class", 1, TokenType::Class),
            "e" => check_keyword(word, "else", 1, TokenType::Else),
            "f" => {
                if word.len() < 2 {
                    return TokenType::Identifier;
                }

                match &word[1..2] {
                    "a" => check_keyword(word, "false", 2, TokenType::False),
                    "o" => check_keyword(word, "for", 2, TokenType::For),
                    "u" => check_keyword(word, "fun", 2, TokenType::Fun),
                    _ => TokenType::Identifier,
                }
            }
            "i" => check_keyword(word, "if", 1, TokenType::If),
            "n" => check_keyword(word, "nil", 1, TokenType::Nil),
            "o" => check_keyword(word, "or", 1, TokenType::Or),
            "p" => check_keyword(word, "print", 1, TokenType::Print),
            "r" => check_keyword(word, "return", 1, TokenType::Return),
            "s" => check_keyword(word, "super", 1, TokenType::Super),
            "t" => {
                if word.len() < 2 {
                    return TokenType::Identifier;
                }

                match &word[1..2] {
                    "h" => check_keyword(word, "this", 2, TokenType::This),
                    "r" => check_keyword(word, "true", 2, TokenType::True),
                    _ => TokenType::Identifier,
                }
            }
            "v" => check_keyword(word, "var", 1, TokenType::Var),
            "w" => check_keyword(word, "while", 1, TokenType::While),
            _ => TokenType::Identifier,
        }
    }
}
