use std::{cmp::Ordering, process::exit};

use rox::{interpreter::RuntimeError, Rox};

fn main() {
    let mut args = std::env::args();
    let mut rox = Rox::default();
    match args.len().cmp(&2) {
        Ordering::Greater => {
            println!("Usage: rox [script]");
            exit(64);
        }
        Ordering::Equal => {
            if let Err(err) = rox.run_file(unsafe { &args.nth(1).unwrap_unchecked() }) {
                eprintln!("{}", err);
                if err.is::<RuntimeError>() {
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
