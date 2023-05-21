pub mod data_types;
mod decl;
mod expr;
mod patterns;
mod types;
pub use crate::parser::data_types::{ParsedTree, TokenKind};
use crate::parser::data_types::{Parser, TreeBuilder};
use logos::Logos;

fn single_token(parser: &mut Parser) {
    parser.advance();
}

fn parameter(parser: &mut Parser) -> bool {
    parser.start_node(TokenKind::Parameter);
    let mut last_succeeded = patterns::pattern(parser);
    if parser.matched(TokenKind::Colon) {
        last_succeeded &= types::type_(parser);
    }
    parser.finish_node();
    last_succeeded
}

pub fn parse(source: &str) -> ParsedTree {
    let lexer = TokenKind::lexer(source);
    let tokens: Vec<_> = lexer
        .spanned()
        .map(|(token_kind, range)| (token_kind, &source[range]))
        .collect();
    let mut parser = Parser::from_tokens(tokens.as_slice());
    parser.start_node(TokenKind::SourceFile);
    let mut last_succeeded = true;
    while last_succeeded && parser.peek().is_some() {
        last_succeeded = decl::declaration(&mut parser);
    }
    if let Some(x) = parser.peek() {
        parser.report_error(format!("unexpected {}", x));
    }
    parser.finish_node();
    let builder = TreeBuilder::new(tokens.as_slice(), parser.events());
    let cst = builder.finish();
    let result = ParsedTree {
        tree: cst,
        errors: parser.into_errors(),
    };
    result.print_tree();
    for error in &result.errors {
        println!("Error: {}", error);
    }
    result
}
