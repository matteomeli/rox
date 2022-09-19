use std::{
    error::Error,
    fs::File,
    io::{self, BufRead, Read, Write},
};

use crate::{interpreter::Interpreter, parser::Parser, resolver::Resolver, scanner::Scanner};

pub type RoxResult = Result<(), Box<dyn Error>>;

#[derive(Default)]
pub struct Rox {
    interpreter: Interpreter,
}

impl Rox {
    pub fn run_file(&mut self, path: &str) -> RoxResult {
        let mut file = File::open(path)?;
        let mut source = String::new();

        file.read_to_string(&mut source)?;

        self.run(source, false)
    }

    pub fn run_prompt(&mut self) -> RoxResult {
        print!("> ");
        io::stdout().flush()?;

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
            io::stdout().flush()?;
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

        // Resolve variables and bindings
        let mut resolver = Resolver::new(&mut self.interpreter);
        resolver.resolve(&statements)?;

        // Interpret AST
        if is_repl {
            self.interpreter.interpret_repl(&statements)?;
        } else {
            self.interpreter.interpret(&statements)?;
        }

        Ok(())
    }
}
