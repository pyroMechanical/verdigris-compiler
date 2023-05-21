use super::data_types::{Parser, TokenKind};
use super::expr;
use super::patterns;
use super::types;
pub(crate) fn let_decl(parser: &mut Parser) {
    assert!(parser.peek() == Some(TokenKind::Let));
    parser.start_node(TokenKind::VariableDecl);
    parser.advance();
    parser.matched(TokenKind::Mutable);
    patterns::pattern(parser);
    if parser.matched(TokenKind::Colon) {
        types::type_(parser);
    }
    if parser.matched(TokenKind::Equal) {
        expr::expr(parser, false, true);
    }
    parser.expect(TokenKind::Semicolon);
    parser.finish_node();
}
fn function_decl(parser: &mut Parser) {
    assert!(parser.peek() == Some(TokenKind::Fn));
    parser.start_node(TokenKind::FunctionDecl);
    parser.advance();
    if parser.matched(TokenKind::Identifier) {
        parser.expect(TokenKind::Paren);
        let mut last_succeeded = true;
        while last_succeeded && parser.peek() != Some(TokenKind::CloseParen) {
            last_succeeded = patterns::pattern(parser);
            if !parser.matched(TokenKind::Comma) {
                break;
            }
        }
        parser.expect(TokenKind::CloseParen);
    } else if parser.matched(TokenKind::Paren) {
        if parser.matched(TokenKind::Operator) {
            patterns::pattern(parser);
            parser.expect(TokenKind::CloseParen);
        } else {
            patterns::pattern(parser);
            parser.expect(TokenKind::Operator);
            patterns::pattern(parser);
            parser.expect(TokenKind::CloseParen);
        }
    }
    if parser.matched(TokenKind::Colon) {
        types::type_(parser);
    }
    if parser.peek() == Some(TokenKind::Brace) {
        parser.start_node(TokenKind::BlockExpr);
        expr::block_expr(parser);
        parser.finish_node();
    } else {
        parser.expect(TokenKind::Semicolon);
    }
    parser.finish_node();
}
fn class_constraint(parser: &mut Parser) -> bool {
    let checkpoint = parser.checkpoint();
    let mut succeeded = true;
    succeeded &= parser.expect(TokenKind::Identifier);
    succeeded &= types::type_(parser);
    if succeeded {
        parser.start_node_at(checkpoint, TokenKind::ClassConstraint);
    } else {
        parser.start_node_at(checkpoint, TokenKind::ParseError);
    }
    parser.finish_node();
    succeeded
}
fn class_decl(parser: &mut Parser) {
    assert!(parser.peek() == Some(TokenKind::Class));
    parser.start_node(TokenKind::ClassDecl);
    parser.advance();
    let constraint = parser.checkpoint();
    if parser.matched(TokenKind::Paren) {
        parser.start_node_at(constraint, TokenKind::ClassConstraintList);
        let mut succeeded = true;
        while succeeded && parser.peek() != Some(TokenKind::CloseParen) {
            succeeded &= class_constraint(parser);
            if !parser.matched(TokenKind::Comma) {
                break;
            }
        }
        parser.expect(TokenKind::CloseParen);
    } else {
        class_constraint(parser);
        if parser.matched(TokenKind::WideArrow) {
            parser.start_node_at(constraint, TokenKind::TypeConstraint);
            parser.finish_node();
            class_constraint(parser);
        }
    }
    if parser.matched(TokenKind::Brace) {
        let mut last_succeeded = true;
        while last_succeeded
            && parser.peek().is_some()
            && parser.peek() != Some(TokenKind::CloseBrace)
        {
            last_succeeded = declaration(parser);
        }
        parser.expect(TokenKind::CloseBrace);
    }
    parser.finish_node();
}
fn implementation_decl(parser: &mut Parser) {
    assert!(parser.peek() == Some(TokenKind::Implement));
    parser.start_node(TokenKind::ImplementationDecl);
    parser.advance();
    let constraint = parser.checkpoint();
    if parser.matched(TokenKind::Paren) {
        parser.start_node_at(constraint, TokenKind::ClassConstraintList);
        let mut succeeded = true;
        while succeeded && parser.peek() != Some(TokenKind::CloseParen) {
            succeeded &= class_constraint(parser);
            if !parser.matched(TokenKind::Comma) {
                break;
            }
        }
        parser.expect(TokenKind::CloseParen);
    } else {
        class_constraint(parser);
        if parser.matched(TokenKind::WideArrow) {
            parser.start_node_at(constraint, TokenKind::TypeConstraint);
            parser.finish_node();
            class_constraint(parser);
        }
    }
    if parser.matched(TokenKind::Brace) {
        let mut last_succeeded = true;
        while last_succeeded
            && parser.peek().is_some()
            && parser.peek() != Some(TokenKind::CloseBrace)
        {
            last_succeeded = declaration(parser);
        }
        parser.expect(TokenKind::CloseBrace);
    }
    parser.finish_node();
}
fn struct_decl(parser: &mut Parser) {
    assert!(parser.peek() == Some(TokenKind::Struct));
    parser.start_node(TokenKind::StructDecl);
    parser.advance();
    types::generic_type(parser);
    if parser.peek() == Some(TokenKind::Paren) {
        types::paren_type(parser);
        parser.expect(TokenKind::Semicolon);
    } else if parser.matched(TokenKind::Brace) {
        let mut last_succeeded = true;
        while last_succeeded
            && parser.peek().is_some()
            && parser.peek() != Some(TokenKind::CloseBrace)
        {
            last_succeeded = super::parameter(parser);
            if !parser.matched(TokenKind::Comma) {
                break;
            }
        }
        parser.expect(TokenKind::CloseBrace);
    } else {
        parser.report_error("expected struct or tuple struct definition!".into());
    }
    parser.finish_node();
}
fn union_decl(parser: &mut Parser) {
    assert!(parser.peek() == Some(TokenKind::Union));
    parser.start_node(TokenKind::UnionDecl);
    parser.advance();
    types::generic_type(parser);
    parser.expect(TokenKind::Brace);
    let mut last_succeeded = true;
    while last_succeeded
        && parser.peek().is_some()
        && parser.peek() != Some(TokenKind::CloseBrace)
    {
        last_succeeded = union_variant(parser);
        if !parser.matched(TokenKind::Comma) {
            break;
        }
    }
    parser.expect(TokenKind::CloseBrace);
    parser.finish_node();
}
fn union_variant(parser: &mut Parser) -> bool {
    let checkpoint = parser.checkpoint();
    parser.expect(TokenKind::Identifier);
    let mut last_succeeded = true;
    if parser.matched(TokenKind::Brace) {
        while last_succeeded && parser.peek() != Some(TokenKind::CloseBrace) {
            last_succeeded = super::parameter(parser);
            if !parser.matched(TokenKind::Comma) {
                break;
            }
        }
        parser.expect(TokenKind::CloseBrace);
        parser.start_node_at(checkpoint, TokenKind::StructVariant);
        parser.finish_node();
    } else if parser.peek() == Some(TokenKind::Paren) {
        last_succeeded &= types::paren_type(parser);
        parser.start_node_at(checkpoint, TokenKind::TupleVariant);
        parser.finish_node();
    } else {
        parser.start_node_at(checkpoint, TokenKind::PlainVariant);
        parser.finish_node();
    }
    last_succeeded
}
fn using_decl(parser: &mut Parser) {
    assert!(parser.peek() == Some(TokenKind::Using));
    parser.start_node(TokenKind::UsingDecl);
    parser.advance();
    parser.expect(TokenKind::Identifier);
    let mut last_succeeded = true;
    while last_succeeded && parser.peek() == Some(TokenKind::Path) {
        last_succeeded = expr::path(parser, 0);
    }
    parser.expect(TokenKind::Semicolon);
    parser.finish_node();
}
fn operator_decl(parser: &mut Parser) {
    assert!(
        parser.peek() == Some(TokenKind::Prefix)
            || parser.peek() == Some(TokenKind::Infixl)
            || parser.peek() == Some(TokenKind::Infixr)
    );
    parser.start_node(TokenKind::OperatorDecl);
    parser.advance();
    parser.expect(TokenKind::Operator);
    parser.matched(TokenKind::Int);
    parser.expect(TokenKind::Semicolon);
    parser.finish_node();
}
fn module_decl(parser: &mut Parser) {
    assert!(parser.peek() == Some(TokenKind::Module));
    parser.start_node(TokenKind::ModuleDecl);
    parser.advance();
    parser.expect(TokenKind::Identifier);
    if parser.peek() == Some(TokenKind::Brace) {
        expr::block_expr(parser);
    } else {
        parser.expect(TokenKind::Semicolon);
    }
    parser.finish_node();
}
fn type_decl(parser: &mut Parser) {
    assert!(parser.peek() == Some(TokenKind::Type));
    parser.start_node(TokenKind::NewTypeDecl);
    parser.advance();
    types::type_(parser);
    parser.expect(TokenKind::Equal);
    types::type_(parser);
    parser.expect(TokenKind::Semicolon);
    parser.finish_node();
}
pub(crate) fn declaration(parser: &mut Parser) -> bool {
    let checkpoint = parser.checkpoint();
    let mut result = true;
    match parser.peek() {
        None => (),
        Some(kind) => match kind {
            TokenKind::Semicolon => parser.advance(),
            TokenKind::Let => let_decl(parser),
            TokenKind::Fn => function_decl(parser),
            TokenKind::Class => class_decl(parser),
            TokenKind::Implement => implementation_decl(parser),
            TokenKind::Struct => struct_decl(parser),
            TokenKind::Union => union_decl(parser),
            TokenKind::Using => using_decl(parser),
            TokenKind::Module => module_decl(parser),
            TokenKind::Type => type_decl(parser),
            TokenKind::Prefix | TokenKind::Infixl | TokenKind::Infixr => operator_decl(parser),
            _ => {
                result = expr::expr(parser, false, true);
                if result {
                    parser.expect(TokenKind::Semicolon);
                    parser.start_node_at(checkpoint, TokenKind::ExprDecl);
                    parser.finish_node();
                }
            }
        },
    };
    result
}