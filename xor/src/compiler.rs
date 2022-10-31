use crate::scanner::{Scanner, TokenType};

pub(crate) fn compile(source: &str) {
    let mut scanner = Scanner::new(source);

    let mut line: u32 = 0;
    loop {
        let token = scanner.scan_token();
        if token.line != line {
            print!("{:04} ", token.line);
            line = token.line;
        } else {
            print!("   | ");
        }
        println!("{:?} {}", token.ttype, token.lexeme.unwrap());

        if token.ttype == TokenType::Eof {
            break;
        }
    }
}
