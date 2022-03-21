use core::fmt;
use std::fmt::write;

pub mod scanner;
pub mod token;

pub type Result<T> = std::result::Result<T, ScannerError>;

#[derive(Debug, Clone)]
pub struct ScannerError {
    line: u32,
    kind: ScannerErrorKind,
}

impl ScannerError {
    pub fn new(line: u32, kind: ScannerErrorKind) -> Self {
        ScannerError { line, kind }
    }
}

impl fmt::Display for ScannerError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[line {}] Error: {}", self.line, self.kind)
    }
}

#[derive(Debug, Clone)]
pub enum ScannerErrorKind {
    UnexpcetedCharacter,
    UnterminatedString,
    UnterminatedBlockComment,
}

impl fmt::Display for ScannerErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self {
            Self::UnexpcetedCharacter => write!(f, "Unexpected character."),
            Self::UnterminatedString => write!(f, "Unterminated string."),
            Self::UnterminatedBlockComment => write!(f, "Unterminated block comment."),
        }
    }
}
