use super::data_types::{TokenKind, Parser};
pub(crate) fn type_(parser: &mut Parser) -> bool{
    function_type(parser)
}

pub(crate) fn function_type(parser: &mut Parser) -> bool {
    let checkpoint = parser.checkpoint();
    let mut succeeded = true;
    succeeded &= applied_type(parser);
    if parser.matched(TokenKind::Arrow)
        || parser.matched(TokenKind::MutArrow)
        || parser.matched(TokenKind::OnceArrow)
    {
        parser.start_node_at(checkpoint, TokenKind::FunctionType);
        succeeded &= function_type(parser);
        parser.finish_node();
    };
    succeeded
}

pub fn is_type_token(token: Option<TokenKind>) -> bool {
    match token {
        None => false,
        Some(t) => matches!(
            t,
            TokenKind::Identifier
                | TokenKind::SingleQuote
                | TokenKind::BasicArrow
                | TokenKind::BasicMutArrow
                | TokenKind::BasicOnceArrow
                | TokenKind::Bracket
                | TokenKind::Paren
                | TokenKind::HashSign
                | TokenKind::Reference
                | TokenKind::Dereference
        ),
    }
}

pub(crate) fn applied_type(parser: &mut Parser) -> bool{
    let checkpoint = parser.checkpoint();
    let mut succeeded = array_or_pointer_type(parser);
    if is_type_token(parser.peek()) {
        parser.start_node_at(checkpoint, TokenKind::AppliedType);
        while succeeded && is_type_token(parser.peek()) {
            succeeded &= array_or_pointer_type(parser);
        }
        parser.finish_node();
    };
    succeeded
}

pub(crate) fn array_or_pointer_type(parser: &mut Parser) -> bool{
    let checkpoint = parser.checkpoint();
    let mut succeeded = true;
    if parser.matched(TokenKind::Reference) {
        parser.matched(TokenKind::Mutable);
        let lifetime_checkpoint = parser.checkpoint();
        if parser.matched(TokenKind::HashSign) {
            parser.start_node_at(lifetime_checkpoint, TokenKind::LifetimeType);
            parser.expect(TokenKind::Identifier);
            parser.finish_node();
        };
        succeeded &= array_or_pointer_type(parser);
        parser.start_node_at(checkpoint, TokenKind::ReferenceType);
        parser.finish_node();
    } else if parser.matched(TokenKind::Dereference) {
        succeeded &= array_or_pointer_type(parser);
        parser.start_node_at(checkpoint, TokenKind::PointerType);
        parser.finish_node();
    } else if parser.matched(TokenKind::Bracket) {
        if parser.matched(TokenKind::CloseBracket) {
            parser.start_node_at(checkpoint, TokenKind::BasicArrayType);
            parser.finish_node();
        } else if parser.matched(TokenKind::Int) {
            parser.expect(TokenKind::CloseBracket);
            parser.start_node_at(checkpoint, TokenKind::BasicArrayType);
            parser.finish_node();
        } else {
            type_(parser);
            if parser.matched(TokenKind::Semicolon) {
                parser.expect(TokenKind::Int);
            }
            parser.expect(TokenKind::CloseBracket);
            parser.start_node_at(checkpoint, TokenKind::ArrayType);
            parser.finish_node();
        }
    } else {
        succeeded &= paren_type(parser);
    };
    succeeded
}

pub(crate) fn paren_type(parser: &mut Parser) -> bool{
    let checkpoint = parser.checkpoint();
    let mut succeeded = true;
    if parser.matched(TokenKind::Paren) {
        if parser.matched(TokenKind::CloseParen) {
            parser.start_node_at(checkpoint, TokenKind::UnitType);
            parser.finish_node();
        } else if parser.matched(TokenKind::Comma) {
            while succeeded && parser.peek() != Some(TokenKind::CloseParen) {
                succeeded &= parser.expect(TokenKind::Comma);
            }
            succeeded &= parser.expect(TokenKind::CloseParen);
            parser.start_node_at(checkpoint, TokenKind::BasicTupleType);
            parser.finish_node();
        } else {
            let mut types = 0;
            while parser.peek() != Some(TokenKind::CloseParen) {
                types += 1;
                succeeded &= type_(parser);
                if !parser.matched(TokenKind::Comma) {
                    break;
                }
            }
            parser.expect(TokenKind::CloseParen);
            match types {
                0 => (),
                1 => {
                    parser.start_node_at(checkpoint, TokenKind::GroupingType);
                    parser.finish_node();
                }
                _ => {
                    parser.start_node_at(checkpoint, TokenKind::TupleType);
                    parser.finish_node();
                }
            }
        }
    } else {
        succeeded &= basic_type(parser);
    };
    succeeded
}

pub(crate) fn basic_type(parser: &mut Parser) -> bool{
    let checkpoint = parser.checkpoint();
    let mut succeeded = true;
    if parser.matched(TokenKind::SingleQuote) {
        parser.start_node_at(checkpoint, TokenKind::VarType);
        succeeded &= parser.expect(TokenKind::Identifier);
        parser.finish_node();
    } else if parser.matched(TokenKind::BasicArrow) {
        parser.start_node_at(checkpoint, TokenKind::BasicArrowType);
        parser.finish_node();
    } else if parser.matched(TokenKind::HashSign) {
        parser.start_node_at(checkpoint, TokenKind::LifetimeType);
        succeeded &= parser.expect(TokenKind::Identifier);
        parser.finish_node();
    } else {
        parser.start_node(TokenKind::BasicType);
        succeeded &= parser.expect(TokenKind::Identifier);
        parser.finish_node();
    };
    succeeded
}

pub(crate) fn generic_type(parser: &mut Parser) {
    let checkpoint = parser.checkpoint();
    let mut type_ = None;
    basic_type(parser);
    while matches!(
        parser.peek(),
        Some(TokenKind::Identifier)
            | Some(TokenKind::HashSign)
            | Some(TokenKind::BasicArrow)
            | Some(TokenKind::SingleQuote)
    ) {
        type_ = Some(TokenKind::AppliedType);
        basic_type(parser);
    }
    if let Some(type_) = type_ {
        parser.start_node_at(checkpoint, type_);
        parser.finish_node();
    }
}