use std::io::{self, BufRead, Write};
use nom_locate::LocatedSpan;
use nom_supreme::error::{ErrorTree, BaseErrorKind, GenericErrorTree, StackContext};
use thiserror::Error;
use miette::GraphicalReportHandler;

extern crate nom;

mod lexer;
mod parser;
mod typechecker;
mod semant;
mod compile;

type Span<'a> = LocatedSpan<&'a str>;

#[derive(thiserror::Error, Debug, miette::Diagnostic)]
#[error("bad input")]
struct BadInput<'a> {
    #[source_code]
    src: &'a str,

    #[label("{kind}")]
    bad_bit: miette::SourceSpan,

    kind: BaseErrorKind<&'static str, Box<dyn std::error::Error + Send + Sync>>,
}

fn print_errors<'a>(e:ErrorTree<Span<'a>>, source: &'a str, contexts:Vec<(LocatedSpan<&str>, StackContext<&str>)>) {
    match e {
        GenericErrorTree::Base{location, kind} => {
            let offset = location.location_offset().into();
            let err = BadInput {
                src: source,
                bad_bit: miette::SourceSpan::new(offset, 0.into()),
                kind
            };
            let mut s = String::new();
            GraphicalReportHandler::new()
                .with_cause_chain()
                .render_report(&mut s, &err)
                .unwrap();
            println!("{s}");
        },
        GenericErrorTree::Stack{base, contexts} => {
            print_errors(*base, source, contexts);
            //todo!()
        },
        GenericErrorTree::Alt(errors) => {
            for e in errors {
                print_errors(e, source, vec![]);
            }
        },
    }
}

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
                        //println!("\"{}\"", source);
                        let result = compile::compile_source(source.as_str());
                        match result
                        {
                            Ok(_) => {println!("Compile Success!");}
                            Err(e) => {print_errors(e, source.as_ref(), vec![]);}
                        }
                        source.clear();
                    }
                    else {
                        if !source.is_empty() {source.push('\n');}
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

fn main()
{
    repl()
}