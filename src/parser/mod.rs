pub mod types;

use logos::Logos;
use crate::parser::types::{TokenKind, ParsedTree, Parser, TreeBuilder};

fn single_token(parser: &mut Parser) {
    parser.advance();
}

fn operator_expr(parser: &mut Parser, precedence: usize) -> bool{
    assert!(parser.peek() == Some(TokenKind::Operator));
    parser.advance();
    prec_expr(parser, precedence, false, true)
}

fn field_call(parser: &mut Parser, precedence: usize) -> bool{
    assert!(parser.peek() == Some(TokenKind::Dot));
    parser.advance();
    prec_expr(parser, precedence, false, true)
}

fn prefix_expr(parser: &mut Parser) {
    assert!(parser.peek() == Some(TokenKind::Operator));
    parser.advance();
    prec_expr(parser, 2, false, true);
}

fn function_call(parser: &mut Parser, precedence: usize) -> bool{
    assert!(parser.peek() == Some(TokenKind::Paren));
    parser.advance();
    let mut last_succeeded = true;
    while last_succeeded && parser.peek().is_some() && parser.peek() != Some(TokenKind::CloseParen) {
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
    while last_succeeded &&parser.peek().is_some() && parser.peek() != Some(TokenKind::CloseParen) {
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

fn block_expr(parser: &mut Parser) {
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
            } else if !declaration(parser) {
                break;
            }
        } else if !declaration(parser) {
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
            parser.report_error("expected block after expression in if expression!".to_string());
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
    while last_succeeded && parser.peek().is_some() && parser.peek() != Some(TokenKind::CloseBracket) {
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
        last_succeeded = pattern(parser);
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
            f: single_token,
            kind: Some(TokenKind::IdentifierExpr),
        }),
        TokenKind::Placeholder => Some(PrefixRule {
            f: single_token,
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
            f: single_token,
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

fn dereference(parser: &mut Parser, _: usize) -> bool{
    parser.advance();
    true
}

fn reference(parser: &mut Parser, _: usize) -> bool{
    parser.advance();
    true
}

fn struct_init(parser: &mut Parser, _: usize) -> bool{
    assert!(parser.peek() == Some(TokenKind::Brace));
    parser.advance();
    let mut last_succeeded = true;
    while last_succeeded && parser.peek().is_some() && parser.peek() != Some(TokenKind::CloseBrace) {
        last_succeeded &= parser.expect(TokenKind::Identifier);
        if parser.matched(TokenKind::Colon) {
            last_succeeded &= expr(parser, false, true);
        }
        if !parser.matched(TokenKind::Comma) {
            break;
        }
    }
    parser.expect(TokenKind::CloseBrace);
    last_succeeded
}

fn path(parser: &mut Parser, _: usize) -> bool{
    assert!(parser.peek() == Some(TokenKind::Path));
    parser.advance();
    if parser.matched(TokenKind::Brace) {
        let mut last_succeeded = true;
        while last_succeeded && parser.peek().is_some() && parser.peek() != Some(TokenKind::CloseBrace) {
            let checkpoint = parser.checkpoint();
            parser.expect(TokenKind::Identifier);
            if parser.peek() == Some(TokenKind::Path) {
                last_succeeded = path(parser, 0);
                parser.start_node_at(checkpoint, TokenKind::PathExpr);
            }
            if !parser.matched(TokenKind::Comma) {
                break;
            }
        }
        parser.expect(TokenKind::CloseBrace);
        last_succeeded
    } else {
        parser.expect(TokenKind::Identifier);
        true
    }
}

fn array_index(parser: &mut Parser, _: usize) -> bool{
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

fn parameter(parser: &mut Parser) -> bool{
    parser.start_node(TokenKind::Parameter);
    let mut last_succeeded = pattern(parser);
    if parser.matched(TokenKind::Colon) {
        last_succeeded &= type_(parser);
    }
    parser.finish_node();
    last_succeeded
}

fn let_decl(parser: &mut Parser) {
    assert!(parser.peek() == Some(TokenKind::Let));
    parser.start_node(TokenKind::VariableDecl);
    parser.advance();
    parser.matched(TokenKind::Mutable);
    pattern(parser);
    if parser.matched(TokenKind::Colon) {
        type_(parser);
    }
    if parser.matched(TokenKind::Equal) {
        expr(parser, false, true);
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
            last_succeeded = pattern(parser);
            if !parser.matched(TokenKind::Comma) {
                break;
            }
        }
        parser.expect(TokenKind::CloseParen);
    }
    else if parser.matched(TokenKind::Paren) {
        if parser.matched(TokenKind::Operator) {
            pattern(parser);
            parser.expect(TokenKind::CloseParen);
        }
        else {
            pattern(parser);
            parser.expect(TokenKind::Operator);
            pattern(parser);
            parser.expect(TokenKind::CloseParen);
        }
    }
    if parser.matched(TokenKind::Colon) {
        type_(parser);
    }
    if parser.peek() == Some(TokenKind::Brace) {
        parser.start_node(TokenKind::BlockExpr);
        block_expr(parser);
        parser.finish_node();
    } else {
        parser.expect(TokenKind::Semicolon);
    }
    parser.finish_node();
}

fn class_constraint(parser: &mut Parser) -> bool{
    let checkpoint = parser.checkpoint();
    let mut succeeded = true;
    succeeded &= parser.expect(TokenKind::Identifier);
    succeeded &= type_(parser);
    if succeeded {
        parser.start_node_at(checkpoint, TokenKind::ClassConstraint);
    }
    else {
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
    }
    else {
        class_constraint(parser);
        if parser.matched(TokenKind::WideArrow) {
            parser.start_node_at(constraint, TokenKind::TypeConstraint);
            parser.finish_node();
            class_constraint(parser);
        }
    }
    if parser.matched(TokenKind::Brace) {
        let mut last_succeeded = true;
        while last_succeeded && parser.peek().is_some() && parser.peek() != Some(TokenKind::CloseBrace) {
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
    }
    else {
        class_constraint(parser);
        if parser.matched(TokenKind::WideArrow) {
            parser.start_node_at(constraint, TokenKind::TypeConstraint);
            parser.finish_node();
            class_constraint(parser);
        }
    }
    if parser.matched(TokenKind::Brace) {
        let mut last_succeeded = true;
        while last_succeeded && parser.peek().is_some() && parser.peek() != Some(TokenKind::CloseBrace) {
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
    generic_type(parser);
    if parser.peek() == Some(TokenKind::Paren) {
        paren_type(parser);
        parser.expect(TokenKind::Semicolon);
    } else if parser.matched(TokenKind::Brace) {
        let mut last_succeeded = true;
        while last_succeeded && parser.peek().is_some() && parser.peek() != Some(TokenKind::CloseBrace) {
            last_succeeded = parameter(parser);
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
    generic_type(parser);
    parser.expect(TokenKind::Brace);
    let mut last_succeeded = true;
    while last_succeeded && parser.peek().is_some() && parser.peek() != Some(TokenKind::CloseBrace) {
        last_succeeded = union_variant(parser);
        if !parser.matched(TokenKind::Comma) {
            break;
        }
    }
    parser.expect(TokenKind::CloseBrace);
    parser.finish_node();
}

fn union_variant(parser: &mut Parser) -> bool{
    let checkpoint = parser.checkpoint();
    parser.expect(TokenKind::Identifier);
    let mut last_succeeded = true;
    if parser.matched(TokenKind::Brace) {
        while last_succeeded && parser.peek() != Some(TokenKind::CloseBrace) {
            last_succeeded = parameter(parser);
            if !parser.matched(TokenKind::Comma) {
                break;
            }
        }
        parser.expect(TokenKind::CloseBrace);
        parser.start_node_at(checkpoint, TokenKind::StructVariant);
        parser.finish_node();
    } else if parser.peek() == Some(TokenKind::Paren) {
        last_succeeded &= paren_type(parser);
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
        last_succeeded = path(parser, 0);
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
        block_expr(parser);
    } else {
        parser.expect(TokenKind::Semicolon);
    }
    parser.finish_node();
}

fn type_decl(parser: &mut Parser){
    assert!(parser.peek() == Some(TokenKind::Type));
    
    parser.start_node(TokenKind::NewTypeDecl);
    parser.advance();
    type_(parser);
    parser.expect(TokenKind::Equal);
    type_(parser);
    parser.expect(TokenKind::Semicolon);
    parser.finish_node();
}

fn declaration(parser: &mut Parser) -> bool {
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
                result = expr(parser, false, true);
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

fn type_(parser: &mut Parser) -> bool{
    function_type(parser)
}

fn function_type(parser: &mut Parser) -> bool {
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

fn is_type_token(token: Option<TokenKind>) -> bool {
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

fn applied_type(parser: &mut Parser) -> bool{
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

fn array_or_pointer_type(parser: &mut Parser) -> bool{
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

fn paren_type(parser: &mut Parser) -> bool{
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

fn basic_type(parser: &mut Parser) -> bool{
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

fn generic_type(parser: &mut Parser) {
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

fn parenthesized_pattern(parser: &mut Parser) -> bool {
    assert!(parser.peek() == Some(TokenKind::Paren));
    let checkpoint = parser.checkpoint();
    parser.advance();
    let mut patterns = 0;
    let mut last_succeeded = true;
    while last_succeeded && parser.peek().is_some() && parser.peek() != Some(TokenKind::CloseParen) {
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

fn pattern(parser: &mut Parser) -> bool {
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
            },
        },
    }
}

fn identifier_pattern(parser: &mut Parser) -> bool {
    let checkpoint = parser.checkpoint();
    parser.advance();
    if parser.matched(TokenKind::Brace) {
        let mut last_succeeded = true;
        while last_succeeded && parser.peek().is_some() && parser.peek() != Some(TokenKind::CloseBrace) {
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
        while last_succeeded &&  parser.peek().is_some() && parser.peek() != Some(TokenKind::CloseParen) {
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

fn expr(parser: &mut Parser, allow_let: bool, allow_struct: bool) -> bool {
    prec_expr(parser, 0, allow_let, allow_struct)
}

fn prec_expr(parser: &mut Parser, precedence: usize, allow_let: bool, allow_struct: bool) -> bool {
    let checkpoint = parser.checkpoint();
    let mut result = false;
    match parser.peek() {
        Some(TokenKind::Let) => {
            if allow_let {
                let_decl(parser);
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

pub fn parse(source: &str) -> ParsedTree {
    let mut lexer = TokenKind::lexer(source);
    let mut tokens = vec![];
    'tokenize: loop {
        match lexer.next() {
            Some(t) => {
                let slice = lexer.slice();
                tokens.push((t, slice));
            }
            None => break 'tokenize,
        }
    }
    let mut parser = Parser::from_tokens(tokens.as_slice());
    parser.start_node(TokenKind::SourceFile);
    let mut last_succeeded = true;
    while last_succeeded && parser.peek().is_some() {
        last_succeeded = declaration(&mut parser);
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