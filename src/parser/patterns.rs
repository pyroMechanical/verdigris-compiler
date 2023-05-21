use super::data_types::{Parser, TokenKind};
fn parenthesized_pattern(parser: &mut Parser) -> bool {
    assert!(parser.peek() == Some(TokenKind::Paren));
    let checkpoint = parser.checkpoint();
    parser.advance();
    let mut patterns = 0;
    let mut last_succeeded = true;
    while last_succeeded && parser.peek().is_some() && parser.peek() != Some(TokenKind::CloseParen)
    {
        last_succeeded &= pattern(parser);
        patterns += 1;
        parser.matched(TokenKind::Comma);
    }
    parser.expect(TokenKind::CloseParen);
    if patterns == 0 {
        parser.start_node_at(checkpoint, TokenKind::UnitPattern);
    } else if patterns == 1 {
        parser.start_node_at(checkpoint, TokenKind::GroupingPattern);
    } else {
        parser.start_node_at(checkpoint, TokenKind::TuplePattern);
    }
    parser.finish_node();
    last_succeeded
}

fn array_pattern(parser: &mut Parser) -> bool {
    assert!(parser.peek() == Some(TokenKind::Bracket));
    parser.start_node(TokenKind::ArrayPattern);
    parser.advance();
    let mut last_succeeded = true;
    while last_succeeded && parser.peek() != Some(TokenKind::CloseBrace) {
        last_succeeded &= pattern(parser);
        if !parser.matched(TokenKind::Comma) {
            break;
        }
    }
    parser.expect(TokenKind::CloseBrace);
    parser.finish_node();
    last_succeeded
}

pub(crate) fn pattern(parser: &mut Parser) -> bool {
    let checkpoint = parser.checkpoint();
    match parser.peek() {
        None => false,
        Some(x) => match x {
            TokenKind::Identifier => identifier_pattern(parser),
            TokenKind::Placeholder => {
                parser.advance();
                parser.start_node_at(checkpoint, TokenKind::PlaceholderPattern);
                parser.finish_node();
                true
            }
            TokenKind::True
            | TokenKind::False
            | TokenKind::String
            | TokenKind::Char
            | TokenKind::Float
            | TokenKind::Double
            | TokenKind::BinInt
            | TokenKind::OctInt
            | TokenKind::Int
            | TokenKind::HexInt => {
                parser.advance();
                parser.start_node_at(checkpoint, TokenKind::LiteralPattern);
                parser.finish_node();
                true
            }
            TokenKind::Paren => parenthesized_pattern(parser),
            TokenKind::Bracket => array_pattern(parser),
            x => {
                parser.report_error(format!("expected Pattern, found {}", x));
                false
            }
        },
    }
}

fn identifier_pattern(parser: &mut Parser) -> bool {
    let checkpoint = parser.checkpoint();
    parser.advance();
    if parser.matched(TokenKind::Brace) {
        let mut last_succeeded = true;
        while last_succeeded
            && parser.peek().is_some()
            && parser.peek() != Some(TokenKind::CloseBrace)
        {
            last_succeeded &= pattern(parser);
            if parser.matched(TokenKind::Colon) {
                last_succeeded &= pattern(parser);
            }
            if !parser.matched(TokenKind::Comma) {
                break;
            }
        }
        parser.expect(TokenKind::CloseBrace);
        parser.start_node_at(checkpoint, TokenKind::StructPattern);
        parser.finish_node();
        last_succeeded
    } else if parser.matched(TokenKind::Paren) {
        let mut last_succeeded = true;
        while last_succeeded
            && parser.peek().is_some()
            && parser.peek() != Some(TokenKind::CloseParen)
        {
            last_succeeded &= pattern(parser);
            if !parser.matched(TokenKind::Comma) {
                break;
            }
        }
        parser.expect(TokenKind::CloseParen);
        parser.start_node_at(checkpoint, TokenKind::TupleStructPattern);
        parser.finish_node();
        last_succeeded
    } else {
        parser.start_node_at(checkpoint, TokenKind::IdentifierPattern);
        parser.finish_node();
        true
    }
}