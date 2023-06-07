use super::data_types::{Parser, TokenKind};
use super::patterns;
fn operator_expr(parser: &mut Parser, precedence: usize) -> bool {
    assert!(parser.peek() == Some(TokenKind::Operator));
    parser.advance();
    prec_expr(parser, precedence, false, true)
}
fn field_call(parser: &mut Parser, precedence: usize) -> bool {
    assert!(parser.peek() == Some(TokenKind::Dot));
    parser.advance();
    prec_expr(parser, precedence, false, true)
}
fn prefix_expr(parser: &mut Parser) {
    assert!(parser.peek() == Some(TokenKind::Operator));
    parser.advance();
    prec_expr(parser, 2, false, true);
}
fn function_call(parser: &mut Parser, precedence: usize) -> bool {
    assert!(parser.peek() == Some(TokenKind::Paren));
    parser.advance();
    let mut last_succeeded = true;
    while last_succeeded
        && parser.peek().is_some()
        && parser.peek() != Some(TokenKind::CloseParen)
    {
        last_succeeded = prec_expr(parser, precedence, false, true);
        if !parser.matched(TokenKind::Comma) {
            break;
        }
    }
    parser.expect(TokenKind::CloseParen);
    last_succeeded
}
fn parenthesized_expr(parser: &mut Parser) {
    assert!(parser.peek() == Some(TokenKind::Paren));
    let checkpoint = parser.checkpoint();
    parser.advance();
    let mut exprs = 0;
    let mut last_succeeded = true;
    while last_succeeded
        && parser.peek().is_some()
        && parser.peek() != Some(TokenKind::CloseParen)
    {
        last_succeeded = expr(parser, false, true);
        exprs += 1;
        parser.matched(TokenKind::Comma);
    }
    parser.expect(TokenKind::CloseParen);
    if exprs == 0 {
        parser.start_node_at(checkpoint, TokenKind::UnitExpr);
    } else if exprs == 1 {
        parser.start_node_at(checkpoint, TokenKind::GroupingExpr);
    } else {
        parser.start_node_at(checkpoint, TokenKind::TupleExpr);
    }
    parser.finish_node();
}
pub(crate) fn block_expr(parser: &mut Parser) {
    assert!(parser.peek() == Some(TokenKind::Brace));
    parser.advance();
    while parser.peek().is_some() && parser.peek() != Some(TokenKind::CloseBrace) {
        if let Some(kind) = parser.peek() {
            if expr_type_from_token(kind).is_some() {
                let checkpoint = parser.checkpoint();
                if !expr(parser, false, true) {
                    break;
                } else if parser.matched(TokenKind::Semicolon) {
                    parser.start_node_at(checkpoint, TokenKind::ExprDecl);
                    parser.finish_node();
                } else {
                    break;
                }
            } else if !super::decl::declaration(parser) {
                break;
            }
        } else if !super::decl::declaration(parser) {
            break;
        }
    }
    parser.expect(TokenKind::CloseBrace);
}
fn if_expr(parser: &mut Parser) {
    assert!(parser.peek() == Some(TokenKind::If));
    parser.advance();
    expr(parser, false, false);
    if parser.peek() == Some(TokenKind::Brace) {
        parser.start_node(TokenKind::BlockExpr);
        block_expr(parser);
        parser.finish_node();
    } else {
        parser.report_error("expected block after expression in if expression!".to_string());
    }
    if parser.matched(TokenKind::Else) {
        if parser.peek() == Some(TokenKind::Brace) {
            parser.start_node(TokenKind::BlockExpr);
            block_expr(parser);
            parser.finish_node();
        } else {
            parser
                .report_error("expected block after expression in if expression!".to_string());
        }
    }
}
fn while_expr(parser: &mut Parser) {
    assert!(parser.peek() == Some(TokenKind::While));
    parser.advance();
    expr(parser, false, false);
    if parser.peek() == Some(TokenKind::Brace) {
        parser.start_node(TokenKind::BlockExpr);
        block_expr(parser);
        parser.finish_node();
    } else {
        parser.report_error("expected block after expression in if expression!".to_string());
    }
}
fn loop_expr(parser: &mut Parser) {
    assert!(parser.peek() == Some(TokenKind::Loop));
    parser.advance();
    if parser.peek() == Some(TokenKind::Brace) {
        parser.start_node(TokenKind::BlockExpr);
        block_expr(parser);
        parser.finish_node();
    } else {
        parser.report_error("expected block after expression in if expression!".to_string());
    }
}
fn array_expr(parser: &mut Parser) {
    assert!(parser.peek() == Some(TokenKind::Bracket));
    parser.advance();
    let mut last_succeeded = true;
    while last_succeeded
        && parser.peek().is_some()
        && parser.peek() != Some(TokenKind::CloseBracket)
    {
        last_succeeded = expr(parser, false, true);
        if !parser.matched(TokenKind::Comma) {
            break;
        }
    }
    parser.expect(TokenKind::CloseBracket);
}
fn unsafe_expr(parser: &mut Parser) {
    assert!(parser.peek() == Some(TokenKind::Unsafe));
    parser.advance();
    expr(parser, false, true);
}
fn return_expr(parser: &mut Parser) {
    assert!(parser.peek() == Some(TokenKind::Return));
    parser.advance();
    expr(parser, false, true);
}
fn lambda_expr(parser: &mut Parser) {
    assert!(parser.peek() == Some(TokenKind::Lambda));
    parser.advance();
    parser.expect(TokenKind::Paren);
    let mut last_succeeded = true;
    while last_succeeded && parser.peek() != Some(TokenKind::CloseParen) {
        last_succeeded = patterns::pattern(parser);
        if !parser.matched(TokenKind::Comma) {
            break;
        }
    }
    parser.expect(TokenKind::CloseParen);
    expr(parser, false, true);
}
struct PrefixRule {
    f: fn(&mut Parser) -> (),
    kind: Option<TokenKind>,
}
fn expr_type_from_token(token: TokenKind) -> Option<PrefixRule> {
    match token {
        TokenKind::Identifier => Some(PrefixRule {
            f: super::single_token,
            kind: Some(TokenKind::IdentifierExpr),
        }),
        TokenKind::Placeholder => Some(PrefixRule {
            f: super::single_token,
            kind: Some(TokenKind::PlaceholderExpr),
        }),
        TokenKind::True
        | TokenKind::False
        | TokenKind::String
        | TokenKind::Char
        | TokenKind::Float
        | TokenKind::Double
        | TokenKind::BinInt
        | TokenKind::OctInt
        | TokenKind::Int
        | TokenKind::HexInt => Some(PrefixRule {
            f: super::single_token,
            kind: Some(TokenKind::LiteralExpr),
        }),
        TokenKind::Operator => Some(PrefixRule {
            f: prefix_expr,
            kind: Some(TokenKind::PrefixExpr),
        }),
        TokenKind::Paren => Some(PrefixRule {
            f: parenthesized_expr,
            kind: None,
        }),
        TokenKind::Brace => Some(PrefixRule {
            f: block_expr,
            kind: Some(TokenKind::BlockExpr),
        }),
        TokenKind::If => Some(PrefixRule {
            f: if_expr,
            kind: Some(TokenKind::IfExpr),
        }),
        TokenKind::While => Some(PrefixRule {
            f: while_expr,
            kind: Some(TokenKind::WhileExpr),
        }),
        TokenKind::Loop => Some(PrefixRule {
            f: loop_expr,
            kind: Some(TokenKind::LoopExpr),
        }),
        TokenKind::Bracket => Some(PrefixRule {
            f: array_expr,
            kind: Some(TokenKind::ArrayConstructorExpr),
        }),
        TokenKind::Unsafe => Some(PrefixRule {
            f: unsafe_expr,
            kind: Some(TokenKind::UnsafeExpr),
        }),
        TokenKind::Return => Some(PrefixRule {
            f: return_expr,
            kind: Some(TokenKind::ReturnExpr),
        }),
        TokenKind::Lambda => Some(PrefixRule {
            f: lambda_expr,
            kind: Some(TokenKind::LambdaExpr),
        }),
        _ => None,
    }
}
fn dereference(parser: &mut Parser, _: usize) -> bool {
    parser.advance();
    true
}
fn reference(parser: &mut Parser, _: usize) -> bool {
    parser.advance();
    true
}
fn struct_init(parser: &mut Parser, _: usize) -> bool {
    assert!(parser.peek() == Some(TokenKind::Brace));
    parser.advance();
    let mut last_succeeded = true;
    while last_succeeded
        && parser.peek().is_some()
        && parser.peek() != Some(TokenKind::CloseBrace)
    {
        parser.start_node(TokenKind::StructField);
        let checkpoint = parser.checkpoint();
        last_succeeded &= parser.expect(TokenKind::Identifier);
        if parser.matched(TokenKind::Colon) {
            last_succeeded &= expr(parser, false, true);
        }
        else {
            parser.start_node_at(checkpoint, TokenKind::IdentifierExpr);
            parser.finish_node();
        }
        parser.finish_node();
        if !parser.matched(TokenKind::Comma) {
            break;
        }
    }
    parser.expect(TokenKind::CloseBrace);
    last_succeeded
}
pub(crate) fn path(parser: &mut Parser, _: usize) -> bool {
    assert!(parser.peek() == Some(TokenKind::Path));
    parser.advance();
    
    if parser.matched(TokenKind::Brace) {
        todo!("this whole section needs redone");
        //let mut last_succeeded = true;
        //while last_succeeded
        //    && parser.peek().is_some()
        //    && parser.peek() != Some(TokenKind::CloseBrace)
        //{
        //    let checkpoint = parser.checkpoint();
        //    parser.start_node(TokenKind::IdentifierExpr);
        //    parser.expect(TokenKind::Identifier);
        //    parser.finish_node();
        //    if parser.peek() == Some(TokenKind::Path) {
        //        last_succeeded = path(parser, 0);
        //        parser.start_node_at(checkpoint, TokenKind::PathExpr);
        //    }
        //    if !parser.matched(TokenKind::Comma) {
        //        break;
        //    }
        //}
        //parser.expect(TokenKind::CloseBrace);
        //last_succeeded
    } else {
        parser.start_node(TokenKind::IdentifierExpr);
        parser.expect(TokenKind::Identifier);
        parser.finish_node();
        true
    }
}
fn array_index(parser: &mut Parser, _: usize) -> bool {
    assert!(parser.peek() == Some(TokenKind::Bracket));
    parser.advance();
    let result = expr(parser, false, true);
    parser.expect(TokenKind::CloseBracket);
    result
}
fn assignment(parser: &mut Parser, _: usize) -> bool {
    assert!(parser.peek() == Some(TokenKind::Equal));
    parser.advance();
    expr(parser, false, true)
}
struct InfixRule {
    f: fn(&mut Parser, usize) -> bool,
    kind: TokenKind,
    prec: usize,
}
fn rhs_type_from_token(token: TokenKind) -> Option<InfixRule> {
    match token {
        TokenKind::Dot => Some(InfixRule {
            f: field_call,
            kind: TokenKind::FieldCallExpr,
            prec: 3,
        }),
        TokenKind::Paren => Some(InfixRule {
            f: function_call,
            kind: TokenKind::FunctionCallExpr,
            prec: 3,
        }),
        TokenKind::Dereference => Some(InfixRule {
            f: dereference,
            kind: TokenKind::DereferenceExpr,
            prec: 2,
        }),
        TokenKind::Reference => Some(InfixRule {
            f: reference,
            kind: TokenKind::ReferenceExpr,
            prec: 2,
        }),
        TokenKind::Operator => Some(InfixRule {
            f: operator_expr,
            kind: TokenKind::BinaryExpr,
            prec: 1,
        }), //binary expr
        TokenKind::Brace => Some(InfixRule {
            f: struct_init,
            kind: TokenKind::StructInitExpr,
            prec: 1,
        }),
        TokenKind::Path => Some(InfixRule {
            f: path,
            kind: TokenKind::PathExpr,
            prec: 0,
        }),
        TokenKind::Bracket => Some(InfixRule {
            f: array_index,
            kind: TokenKind::ArrayIndexExpr,
            prec: 3,
        }),
        TokenKind::Equal => Some(InfixRule {
            f: assignment,
            kind: TokenKind::AssignmentExpr,
            prec: 0,
        }),
        _ => None,
    }
}
fn prec_expr(
    parser: &mut Parser,
    precedence: usize,
    allow_let: bool,
    allow_struct: bool,
) -> bool {
    let checkpoint = parser.checkpoint();
    let mut result = false;
    match parser.peek() {
        Some(TokenKind::Let) => {
            if allow_let {
                super::decl::let_decl(parser);
            } else {
                parser.report_error("expected expression or semicolon!".into());
                parser.recover()
            }
        }
        Some(kind) => match expr_type_from_token(kind) {
            Some(PrefixRule { f, kind }) => {
                let checkpoint = parser.checkpoint();
                f(parser);
                if let Some(kind) = kind {
                    parser.start_node_at(checkpoint, kind);
                    parser.finish_node();
                }
                result = true;
            }
            None => {
                parser.report_error("expected expression or semicolon!".into());
                parser.recover()
            }
        },
        _ => {}
    }
    'rhs: loop {
        match parser.peek() {
            Some(TokenKind::Brace) => {
                if allow_struct {
                    match rhs_type_from_token(TokenKind::Brace) {
                        Some(InfixRule { f, kind, prec }) => {
                            if prec >= precedence {
                                f(parser, prec);
                                parser.start_node_at(checkpoint, kind);
                                parser.finish_node();
                            } else {
                                break 'rhs;
                            }
                        }
                        None => break 'rhs,
                    }
                } else {
                    break 'rhs;
                }
            }
            Some(kind) => match rhs_type_from_token(kind) {
                Some(InfixRule { f, kind, prec }) => {
                    if prec >= precedence {
                        f(parser, prec);
                        parser.start_node_at(checkpoint, kind);
                        parser.finish_node();
                    } else {
                        break 'rhs;
                    }
                }
                None => break 'rhs,
            },
            None => break 'rhs,
        }
    }
    result
}
pub(crate) fn expr(parser: &mut Parser, allow_let: bool, allow_struct: bool) -> bool {
    prec_expr(parser, 0, allow_let, allow_struct)
}