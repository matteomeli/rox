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

#[derive(Debug, Clone)]
pub enum ScannerErrorKind {
    UnexpcetedCharacter,
    UnterminatedString,
}
