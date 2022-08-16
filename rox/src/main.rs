use std::{
    cmp::Ordering,
    error::Error,
    fs::File,
    io::{self, BufRead, Read, Write},
    process::exit,
};

use rox::{
    interpreter::{InterpretError, Interpreter},
    parser::Parser,
    scanner::Scanner,
    types::Type,
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

        self.run(source)
    }

    fn run_prompt(&mut self) -> RoxResult {
        print!("> ");
        io::stdout().flush().unwrap();

        let stdin = io::stdin();
        for line in stdin.lock().lines() {
            match line {
                Ok(line) => {
                    if let Err(err) = self.run(line) {
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

    fn run(&mut self, source: String) -> RoxResult {
        // Scan source into tokens
        let mut scanner = Scanner::new(source);
        let tokens = scanner.scan_tokens()?;

        // Print tokens
        // for token in tokens {
        //     println!("{:?}", token);
        // }
        //println!();

        // Parse tokens into AST
        let mut parser = Parser::new(tokens.to_vec());
        let expr = parser.parse()?;

        // Print AST
        //println!("{}", AstPrinter.print(&expr));

        // Interpret AST
        let result = self.interpreter.interpret(&expr);
        match result {
            Ok(result) => match result {
                Type::Nil => println!("nil"),
                Type::Boolean(b) => println!("{}", b),
                Type::String(s) => println!("{}", s),
                Type::Number(n) => println!("{}", n),
            },
            Err(error) => eprintln!("{}", error),
        }

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
