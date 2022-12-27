use std::io::{self, BufRead, Write};
use nom_locate::LocatedSpan;

extern crate nom;

mod lexer;
mod parser;
mod typechecker;
mod semant;
mod compile;

type Span<'a> = LocatedSpan<&'a str>;

#[derive(Clone, Copy, Debug)]
pub struct State<'a>(&'a std::cell::RefCell<Vec<Error<'a>>>);

impl<'a> State<'a> {
    pub(crate) fn new(errs: &'a std::cell::RefCell<Vec<Error<'a>>>) -> Self {
        State(errs)
    }
    pub(crate) fn error_count(&self) -> usize {
        self.0.borrow().len()
    }
    pub(crate) fn report_error(&self, err: Error<'a>) {
        self.0.borrow_mut().push(err);
    }
    pub(crate) fn truncate_errors(&self, length: usize) {
        self.0.borrow_mut().truncate(length);
    }
}

#[derive(Clone, Copy, Debug)]
struct Error<'a> {
    src: Span<'a>,
    msg: &'static str,
}
type SpanWith<'a> = LocatedSpan<&'a str, State<'a>>;

pub(crate) fn to_span(span_with: SpanWith) -> Span {
    Span::new(span_with.fragment())
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
                        let errs = std::cell::RefCell::new(vec![]);
                        let state = State::new(&errs);
                        let source_span = SpanWith::new_extra(source.as_str(), state);
                        let result = compile::compile_source(source_span);
                        match result
                        {
                            Ok(_) => {println!("Compile Success!");}
                            Err(e) => {println!("{:?}", e);}
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