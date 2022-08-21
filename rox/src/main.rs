use std::{
    cmp::Ordering,
    error::Error,
    fs::File,
    io::{self, BufRead, Read, Write},
    process::exit,
};

use rox::{
    ast::Statement,
    interpreter::{InterpretError, Interpreter},
    parser::Parser,
    scanner::Scanner,
};

type RoxResult = Result<(), Box<dyn Error>>;

#[derive(Default)]
struct Rox {
    interpreter: Interpreter,
}

impl Rox {
    fn run_file(&mut self, path: String) -> RoxResult {
        let mut file = File::open(path).unwrap();
        let mut source = String::new();

        file.read_to_string(&mut source).unwrap();

        self.run(source, false)
    }

    fn run_prompt(&mut self) -> RoxResult {
        print!("> ");
        io::stdout().flush().unwrap();

        let stdin = io::stdin();
        for line in stdin.lock().lines() {
            match line {
                Ok(line) => {
                    if let Err(err) = self.run(line, true) {
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

    fn run(&mut self, source: String, is_repl: bool) -> RoxResult {
        // Scan source into tokens
        let mut scanner = Scanner::new(source);
        let tokens = scanner.scan_tokens()?;

        // Parse tokens into AST
        let mut parser = Parser::new(tokens.to_vec());
        let statements = if is_repl {
            parser.parse_repl()?
        } else {
            parser.parse()?
        };

        // Interpret AST
        self.interpreter.interpret(&statements)?;

        Ok(())
    }
}

fn main() {
    let mut args = std::env::args();
    let mut rox = Rox::default();
    match args.len().cmp(&2) {
        Ordering::Greater => {
            println!("Usage: rox [script]");
            exit(64);
        }
        Ordering::Equal => {
            if let Err(err) = rox.run_file(args.nth(1).unwrap()) {
                eprintln!("{}", err);
                if err.is::<InterpretError>() {
                    exit(70);
                } else {
                    exit(65);
                }
            }
        }
        _ => {
            let _ = rox.run_prompt();
        }
    }
}
