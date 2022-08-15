use std::{
    cmp::Ordering,
    fs::File,
    io::{self, BufRead, Read, Write},
    process::exit,
};

use rlox::{
    parser::{ParseResult, Parser},
    printers::AstPrinter,
    scanner::Scanner,
};

fn main() {
    let mut args = std::env::args();
    match args.len().cmp(&2) {
        Ordering::Greater => {
            println!("Usage: rlox [script]");
            exit(64);
        }
        Ordering::Equal => {
            if let Err(err) = run_file(args.nth(1).unwrap()) {
                eprintln!("{}", err);
                exit(65);
            }
        }
        _ => {
            let _ = run_prompt();
        }
    }
}

fn run_file(path: String) -> ParseResult<()> {
    let mut file = File::open(path).unwrap();
    let mut source = String::new();
    file.read_to_string(&mut source).unwrap();
    run(source)
}

fn run_prompt() -> ParseResult<()> {
    print!("> ");
    io::stdout().flush().unwrap();
    let stdin = io::stdin();
    for line in stdin.lock().lines() {
        match line {
            Ok(line) => {
                if let Err(err) = run(line) {
                    eprintln!("{}", err);
                }
            }
            Err(_) => break,
        };
        print!("> ");
        io::stdout().flush().unwrap();
    }
    Ok(())
}

fn run(source: String) -> ParseResult<()> {
    let mut scanner = Scanner::new(source);
    if let Ok(tokens) = scanner.scan_tokens() {
        // Print tokens
        for token in tokens {
            println!("{:?}", token);
        }

        println!();

        // Parse tokens
        let mut parser = Parser::new(tokens.to_vec());
        let expr = parser.parse()?;
        println!("{}", AstPrinter.print(&expr));
    }

    Ok(())
}
