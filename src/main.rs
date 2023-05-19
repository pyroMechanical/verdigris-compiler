use std::io::{self, BufRead, Write};
use std::env;
use std::path::PathBuf;
use std::fs::read_to_string;

mod ast;
mod compile;
mod parser;
mod reparse;
mod semant;
mod typechecker;

fn repl() {
    println!("To execute your code, type '-eval' on a new line after the end of your block.");
    print!("> ");
    let _ = io::stdout().flush();
    loop {
        let mut source = String::new();
        let stdin = io::stdin();
        for line in stdin.lock().lines() {
            match line {
                Ok(source_line) => {
                    if source_line.as_str().eq("-eval") {
                        let result = compile::compile_source(source.as_str());
                        if result.is_ok() {
                            println!("Compile Success!");
                        }
                        source.clear();
                    } else {
                        if !source.is_empty() {
                            source.push('\n');
                        }
                        source.push_str(source_line.as_str());
                    }
                }
                Err(e) => {
                    eprintln!("{}", e);
                }
            }
            print!("> ");
            let _ = io::stdout().flush();
        }
    }
}

fn compile_file(path: PathBuf) {
    let source = read_to_string(path);
    match source {
        Ok(source) => match compile::compile_source(source.as_str()) {
            Ok(_) => println!("Compile Success!"),
            Err(e) => ()
        }
        Err(e) => eprintln!("{}", e)
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    match args.get(1) {
        None => repl(),
        Some(arg) => {
            println!("argument provided: {}", arg);
            let path: Result<PathBuf, _> = arg.try_into();
            match path {
                Ok(path) => compile_file(path),
                Err(e) => eprintln!("{}", e)
            }
        }
    }
}
