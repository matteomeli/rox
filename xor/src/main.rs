use std::io::{BufRead, Write};

use xor::vm::{VMError, VM};

#[allow(unused_variables, unreachable_code, unused_mut)]
fn main() {
    let mut vm = VM::default();

    let args: Vec<String> = std::env::args().collect();
    let argc = args.len();
    if argc == 1 {
        repl(&mut vm);
    } else if argc == 2 {
        run_file(&mut vm, &args[1]);
    } else {
        eprintln!("usage: xor [path]");
        std::process::exit(64);
    }
}

fn repl(vm: &mut VM) {
    print!("> ");
    std::io::stdout().flush().expect("Error writing to stdout.");
    for line in std::io::stdin().lock().lines() {
        vm.interpret(&line.unwrap()).unwrap_or(());
        print!("> ");
        std::io::stdout().flush().expect("Error writing to stdout.");
    }
}

fn run_file(vm: &mut VM, path: &str) {
    let source = std::fs::read_to_string(path).unwrap_or_else(|_| {
        eprintln!("Could not read input file: {}", path);
        std::process::exit(74);
    });
    let exit_code = match vm.interpret(&source) {
        Ok(()) => 0,
        Err(VMError::CompileError) => 65,
        Err(VMError::RuntimeError(_)) => 70,
    };
    std::process::exit(exit_code);
}
