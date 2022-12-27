use nom::{
    IResult,
    combinator::fail,
    multi::{many0, many1, separated_list0, separated_list1},
    sequence::{preceded, delimited, terminated, pair, tuple},
};
use nom_supreme::{
    final_parser::final_parser
};
use crate::lexer::{
    Token
};

use crate::{Span, SpanWith};

#[derive(Clone, Debug)]
pub struct CaseExpr<'a>(Box<Pattern<'a>>, Box<Expr<'a>>);

#[derive(Clone, Debug)]
pub enum Expr<'a> {
    If{cond: Box<Expr<'a>>, true_branch: Box<Expr<'a>>, else_branch: Option<Box<Expr<'a>>>},
    //For{init: Option<Box<Expression<'a>>>, cond: Option<Box<Expression<'a>>>, incr: Option<Box<Expression<'a>>>, _loop: Box<Expression<'a>>},
    While{cond: Box<Expr<'a>>, loop_: Box<Expr<'a>>},
    Loop{loop_: Box<Expr<'a>>},
    Switch{expr: Box<Expr<'a>>, arms: Vec<Box<CaseExpr<'a>>>},
    Assignment{assigned: Box<Expr<'a>>, value: Box<Expr<'a>>},
    Literal(Token<'a>),
    _Lambda,
    Identifier(Token<'a>),
    Placeholder,
    Tuple(Vec<Box<Expr<'a>>>),
    Prefix{op: Token<'a>, expr: Box<Expr<'a>>},
    Block{declarations: Vec<Box<Decl<'a>>>, ret: Option<Box<Expr<'a>>>},
    ArrayConstructor(Vec<Box<Expr<'a>>>),
    Unsafe(Box<Expr<'a>>),
    Unit,
    Return(Box<Expr<'a>>),
    //left-recursive expressions
    Dereference(Box<Expr<'a>>),
    Reference{ref_: Box<Expr<'a>>, mutable: bool},
    Postfix{op: Token<'a>, expr: Box<Expr<'a>>},
    Binary{expr1: Box<Expr<'a>>, op: Token<'a>, expr2: Box<Expr<'a>>},
    Path{identifier: Box<Expr<'a>>, expr: Box<Expr<'a>>},
    FieldCall(Box<Expr<'a>>, Box<Expr<'a>>),
    FunctionCall{function:  Box<Expr<'a>>, args: Vec<Box<Expr<'a>>>},
    ArrayIndex{array:Box<Expr<'a>>, arg: Box<Expr<'a>>},
    StructInit{id: Box<Expr<'a>>, args: Vec<Box<Expr<'a>>>},
    Error
}

pub enum LRExpr<'a> {
    Dereference,
    Reference(bool),
    Postfix(Token<'a>),
    Assignment(Box<Expr<'a>>),
    Binary(Token<'a>, Box<Expr<'a>>),
    FieldCall(Box<Expr<'a>>),
    FunctionCall(Vec<Box<Expr<'a>>>),
    ArrayIndex(Box<Expr<'a>>),
    StructInit(Vec<Box<Expr<'a>>>),
    Error
}

#[derive(Clone, Debug, PartialEq) ]
pub enum Ty<'a> {
    Basic(Token<'a>),
    ArrayBasic(Option<Token<'a>>), //todo: make this a number
    Unit,
    Reference,
    Pointer,
    Var(Token<'a>),
    GeneratedVar(u64),
    Applied(Box<Ty<'a>>, Vec<Box<Ty<'a>>>),
    Array(Box<Ty<'a>>, Option<Token<'a>>), //todo: make size a number
    Tuple(Vec<Box<Ty<'a>>>),
    Error
}

#[derive(Clone, Debug)]
pub enum Pattern<'a> {
    Identifier(Span<'a>),
    Literal(Token<'a>),
    Tuple(Vec<Box<Pattern<'a>>>),
    Placeholder,
    Array(Vec<Box<Pattern<'a>>>),
    VariantUnwrap(Box<Pattern<'a>>, Vec<Box<Pattern<'a>>>),
    Error
}
#[derive(Clone, Debug)]
pub struct Param<'a> {
    name: Box<Pattern<'a>>,
    type_: Option<Box<Ty<'a>>>
}
#[derive(Clone, Debug)]
pub enum Fixity {
    Prefix,
    Infix,
    Postfix
}

#[derive(Clone, Debug)]
pub enum Decl<'a> {
    Variable{mutable: bool, pattern: Box<Pattern<'a>>, type_: Option<Box<Ty<'a>>>, value: Option<Box<Expr<'a>>>},
    Function{ name: Span<'a>, args: Vec<Param<'a>>, return_type: Option<Box<Ty<'a>>>, body: Box<Expr<'a>>},
    Operator{name: Token<'a>, fixity: Fixity, precedence: Token<'a>},
    Class{class: Token<'a>, types: Vec<Box<Ty<'a>>>, constraints: Option<Vec<Box<Ty<'a>>>>, body: Box<Expr<'a>>},
    Implementation{class: Token<'a>, types: Vec<Box<Ty<'a>>>, constraints: Option<Vec<Box<Ty<'a>>>>, body: Box<Expr<'a>>}, //finish this
    Module{name: Token<'a>, body: Box<Expr<'a>>},
    Union{name: Box<Ty<'a>>, variants: Vec<Param<'a>>},
    Struct{name: Box<Ty<'a>>, members: Vec<Param<'a>>},
    NewType{defined: Box<Ty<'a>>, replacing: Box<Ty<'a>>},
    Expr(Box<Expr<'a>>),
    Error
}

fn optional<'a, F, O, E>(mut parser: F) -> impl FnMut(SpanWith<'a>) -> IResult<SpanWith<'a>, Option<O>, E> 
where F: nom::Parser<SpanWith<'a>, O, E>,
E: nom::error::ParseError<SpanWith<'a>>
{
    move |input| {
        let i = input.clone();
        let current_size = i.extra.error_count();
        let result = parser.parse(input);
        match result {
            Ok((i, o)) => Ok((i, Some(o))),
            Err(_) => {
                i.extra.truncate_errors(current_size);
                Ok((i, None))
            }
        }
    }
}

fn expect<'a, F, O>(mut parser: F, msg: &'static str) -> impl FnMut(SpanWith<'a>) -> IResult<SpanWith<'a>, Option<O>> 
where F: FnMut(SpanWith<'a>) -> IResult<SpanWith<'a>, O>,
{
    move |input| match parser(input) {
        Ok((rest, out)) => Ok((rest, Some(out))),
        Err(nom::Err::Error(nom::error::Error{input: rest, code: _}))|
        Err(nom::Err::Failure(nom::error::Error{input: rest, code: _})) => {
            let error = crate::Error{src: crate::to_span(rest), msg};
            input.extra.report_error(error);
            Ok((rest, None))
        },
        Err(err) => Err(err)
    }
}

pub fn parse(source: SpanWith) -> Result<Vec<Box<Decl>>, nom::error::Error<SpanWith>> {
    let result: Result<Vec<Box<Decl>>, nom::error::Error<SpanWith>> = final_parser(many1(declaration))(source);
    match result {
        Ok(expr) => {/*println!("{:?}", expr);*/ Ok(expr)},
        e => e
    }
}

fn token<'a>(t: Token<'a>) -> impl Fn(SpanWith<'a>) -> IResult<SpanWith<'a>, Token<'a>> {
    let discriminant = std::mem::discriminant(&t);
    move |source| {
        let s = source.clone();
        let (rest, token) = crate::lexer::scan_token(source)?;
        if discriminant == std::mem::discriminant(&token) {return Ok((rest, token));}
        else { return fail(s);}
    }
}

fn expression(scrutinee: bool) -> impl Fn(SpanWith) -> IResult<SpanWith, Box<Expr>>{
    move |source| {
        let s = source.clone();
        let (rest, token) = crate::lexer::scan_token(s)?;
        let (string, expr) = match token {
            Token::If => if_(source)?,
            Token::While => while_(source)?,
            Token::Unsafe => unsafe_(source)?,
            Token::Loop => loop_(source)?,
            Token::Switch => switch_(source)?,
            Token::Brace => block(source)?,
            Token::Bracket => array_constructor(source)?,
            Token::Prefix => prefix_operator(source)?,
            Token::Paren => paren_or_tuple(source)?,
            Token::Float(_) => literal(source)?,
            Token::Int(_) => literal(source)?,
            Token::BinInt(_) => literal(source)?,
            Token::OctInt(_) => literal(source)?,
            Token::HexInt(_) => literal(source)?,
            Token::Char(_) => literal(source)?,
            Token::String(_) => literal(source)?,
            Token::Identifier(_) => identifier_or_path(source)?,
            Token::Return => return_expr(source)?,
            Token::Placeholder => (rest, Box::new(Expr::Placeholder)),
            _ => fail(source)?
        };
        right_expression(expr, string, scrutinee)
    }
}

fn right_expression<'a>(expr: Box<Expr<'a>>, source: SpanWith<'a>, scrutinee: bool) -> IResult <SpanWith<'a>, Box<Expr<'a>>> {
    let (_, token) = crate::lexer::scan_token(source)?;

    let (rest, lr) = match token {
        Token::Reference => {
            let (rest, ref_) = reference(source)?;
            (rest, Some(ref_))
        }
        Token::Dereference => {
            let (rest, deref) = dereference(source)?;
            (rest, Some(deref))
        }
        Token::Operator(_) => {
            let (rest, binary_or_postfix) = binary_or_postfix(scrutinee)(source)?;
            (rest, Some(binary_or_postfix))
        }
        Token::Dot => {
            let (rest, field_call) = field_call(scrutinee)(source)?;
            (rest, Some(field_call))
        }
        Token::Paren => {
            let (rest, function_call) = function_call(source)?;
            (rest, Some(function_call))
        }
        Token::Bracket => {
            let (rest, array_index) = array_index(source)?;
            (rest, Some(array_index))
        }
        Token::Equal => {
            let (rest, assignment) = assignment(scrutinee)(source)?;
            (rest, Some(assignment))
        }
        Token::Brace => if scrutinee {
            (source, None)
        } else {
            let (rest, struct_init) = struct_init(source)?;
            (rest, Some(struct_init))
        },
        _ => (source, None)
    };

    match lr {
        None => Ok((source, expr)),
        Some(lr_expr) => match *lr_expr {
            LRExpr::Dereference => right_expression(Box::new(Expr::Dereference(expr)), rest, scrutinee),
            LRExpr::Reference(mutable) => right_expression(Box::new(Expr::Reference{ref_: expr, mutable: mutable}), rest, scrutinee),
            LRExpr::Binary(op, expr2) => right_expression(Box::new(Expr::Binary{expr1: expr, op: op, expr2: expr2}), rest, scrutinee),
            LRExpr::Postfix(op) => right_expression(Box::new(Expr::Postfix{op: op, expr: expr}), rest, scrutinee),
            LRExpr::FieldCall(expr2) => right_expression(Box::new(Expr::FieldCall(expr, expr2)), rest, scrutinee),
            LRExpr::FunctionCall(args) => right_expression(Box::new(Expr::FunctionCall{function: expr, args: args}), rest, scrutinee),
            LRExpr::ArrayIndex(arg) => right_expression(Box::new(Expr::ArrayIndex{array: expr, arg: arg}), rest, scrutinee),
            LRExpr::Assignment(value) => right_expression(Box::new(Expr::Assignment{assigned: expr, value: value}), rest, scrutinee),
            LRExpr::StructInit(args) => right_expression(Box::new(Expr::StructInit{id: expr, args: args}), rest, scrutinee),
            LRExpr::Error => Ok((rest, Box::new(Expr::Error)))
        }
    }
}

fn reference(source:SpanWith) -> IResult<SpanWith, Box<LRExpr>>{
    let (rest, ref_) = expect(token(Token::Reference), "expected '&'")(source)?;
    if ref_.is_some() {
        let (rest, mut_) = optional(token(Token::Mutable))(rest)?;
        let is_mut = mut_.is_some();
        return Ok((rest, Box::new(LRExpr::Reference(is_mut))));
    }
    Ok((rest, Box::new(LRExpr::Error)))
}

fn dereference(source: SpanWith) -> IResult<SpanWith, Box<LRExpr>> {
    let (rest, _) = token(Token::Dereference)(source)?;
    Ok((rest, Box::new(LRExpr::Dereference)))
}

fn binary_or_postfix(scrutinee: bool) -> impl Fn(SpanWith) -> IResult<SpanWith, Box<LRExpr>> {
    move |source| {
        let (rest, (op, expr)) = pair(operator, optional(expression(scrutinee)))(source)?;
        match expr {
            None => Ok((rest, Box::new(LRExpr::Postfix(op)))),
            Some(expr) => Ok((rest, Box::new(LRExpr::Binary(op, expr))))
        }
    }
}

fn assignment(scrutinee: bool) -> impl Fn(SpanWith) -> IResult<SpanWith, Box<LRExpr>> {
    move |source| {
        let (rest, value) = preceded(token(Token::Equal), expression(scrutinee))(source)?;
        Ok((rest, Box::new(LRExpr::Assignment(value))))
    }
}

fn field_call(scrutinee: bool) -> impl Fn(SpanWith) -> IResult<SpanWith, Box<LRExpr>> {
    move |source| {
        let (rest, expr) = preceded(token(Token::Dot), expression(scrutinee))(source)?;
        Ok((rest, Box::new(LRExpr::FieldCall(expr))))
    }
}

fn function_call(source: SpanWith) -> IResult<SpanWith, Box<LRExpr>> {
    let (rest, list) = delimited(token(Token::Paren), expr_list, token(Token::CloseParen))(source)?;
    Ok((rest, Box::new(LRExpr::FunctionCall(list))))
}

fn array_index(source: SpanWith) -> IResult<SpanWith, Box<LRExpr>> {
    let (rest, expr) = delimited(token(Token::Bracket), expression(false), token(Token::CloseBracket))(source)?;
    Ok((rest, Box::new(LRExpr::ArrayIndex(expr))))
}

fn struct_init(source: SpanWith) -> IResult<SpanWith, Box<LRExpr>> {
    let (rest, list) = delimited(token(Token::Brace), expr_list, token(Token::CloseBrace))(source)?;
    Ok((rest, Box::new(LRExpr::StructInit(list))))
}
fn expr_list(source: SpanWith) -> IResult<SpanWith, Vec<Box<Expr>>> {
    terminated(separated_list0(token(Token::Comma), expression(false)), optional(token(Token::Comma)))(source)
}

fn array_constructor(source: SpanWith) -> IResult<SpanWith, Box<Expr>> {
    let (rest, values) = delimited(token(Token::Bracket), expr_list, token(Token::CloseBracket))(source)?;
    Ok((rest, Box::new(Expr::ArrayConstructor(values))))
}

fn unsafe_(source: SpanWith) -> IResult<SpanWith, Box<Expr>> {
    let (rest, expr) = preceded(token(Token::Unsafe), expression(false))(source)?;
    Ok((rest, Box::new(Expr::Unsafe(expr))))
}

fn if_(source: SpanWith) -> IResult<SpanWith, Box<Expr>> {
    let (rest, ((cond, true_branch), else_branch)) = pair(pair(preceded(token(Token::If), expression(true)), block), optional(preceded(token(Token::Else), block)))(source)?;
    Ok((rest, Box::new(Expr::If{cond: cond, true_branch: true_branch, else_branch: else_branch})))
}

fn while_(source: SpanWith) -> IResult<SpanWith, Box<Expr>> {
    let (rest, (cond, loop_)) = pair(preceded(token(Token::While), expression(true)), block)(source)?;
    Ok((rest, Box::new(Expr::While{cond: cond, loop_: loop_})))
}

fn loop_(source: SpanWith) -> IResult<SpanWith, Box<Expr>> {
    let (rest, loop_) = preceded(token(Token::Loop), block)(source)?;
    Ok((rest, Box::new(Expr::Loop{loop_: loop_})))
}

fn case(source: SpanWith) -> IResult<SpanWith, Box<CaseExpr>> {
    let (rest, (pattern, expr)) = preceded(token(Token::Case), pair(terminated(pattern, token(Token::Colon)), expression(false)))(source)?;
    Ok((rest, Box::new(CaseExpr(pattern, expr))))
}

fn case_block(source: SpanWith) -> IResult<SpanWith, Vec<Box<CaseExpr>>> {
    delimited(token(Token::Brace), terminated(separated_list0(token(Token::Comma), case), optional(token(Token::Comma))), token(Token::CloseBrace))(source)
}

fn switch_(source: SpanWith) -> IResult<SpanWith, Box<Expr>> {
    let(rest, (expr, cases)) = pair(preceded(token(Token::Switch), expression(true)), case_block)(source)?;
    Ok((rest, Box::new(Expr::Switch{expr: expr, arms: cases})))
}

fn operator(source: SpanWith) -> IResult<SpanWith, Token> {
    token(Token::Operator(Span::new("")))(source)
}

fn return_expr(source: SpanWith) -> IResult<SpanWith, Box<Expr>> {
    let (rest, expr) = preceded(token(Token::Return), expression(false))(source)?;
    Ok((rest, Box::new(Expr::Return(expr))))
}

fn block(source: SpanWith) -> IResult<SpanWith, Box<Expr>> {
    let (rest, (list, ret)) = delimited(token(Token::Brace), pair(many0(declaration), optional(expression(false))), token(Token::CloseBrace))(source)?;
    Ok((rest, Box::new(Expr::Block{declarations: list, ret})))
}

fn function_decl(source: SpanWith) -> IResult<SpanWith, Box<Decl>> {
    let (rest, (name, args, return_type, body)) = preceded(token(Token::Fn), tuple((token(Token::Identifier(Span::new(""))), delimited(token(Token::Paren), param_list, token(Token::CloseParen)), optional(preceded(token(Token::Arrow), type_)), block) ))(source)?;
    Ok((rest, Box::new(Decl::Function{name: name.as_span(), args: args, return_type: return_type, body: body})))
}

fn single_constraint(source: SpanWith) -> IResult<SpanWith, Vec<Box<Ty>>> {
    let (rest, constraint) = array_type(source)?;
    Ok((rest, vec![constraint]))
}

fn constraints(source: SpanWith) -> IResult<SpanWith, Vec<Box<Ty>>> {
    let s = source.clone();
    let (_, t) = crate::lexer::scan_token(s)?;
    match t {
        Token::Paren => delimited(token(Token::Paren), separated_list1(token(Token::Comma), array_type), token(Token::CloseParen))(source),
        _ => single_constraint(source)
    }
}

fn class_decl(source: SpanWith) -> IResult<SpanWith, Box<Decl>> {
    let (rest, (constraints, class, types, body)) = preceded(token(Token::Class), tuple((optional(terminated(constraints, token(Token::WideArrow))), token(Token::Identifier(Span::new(""))), many1(var_type), block)))(source)?;
    Ok((rest, Box::new(Decl::Class{class: class, types: types, constraints: constraints, body: body})))
}

fn impl_decl(source: SpanWith) -> IResult<SpanWith, Box<Decl>> {
    let (rest, (constraints, class, types, body)) = preceded(token(Token::Implement), tuple((optional(terminated(constraints, token(Token::WideArrow))), token(Token::Identifier(Span::new(""))), many1(type_), block)))(source)?;
    Ok((rest, Box::new(Decl::Implementation{class: class, types: types, constraints: constraints, body: body})))
}

fn declaration(source: SpanWith) -> IResult<SpanWith, Box<Decl>> {
    let (_, token) = crate::lexer::scan_token(source)?;

    match token {
        Token::Let => variable_decl(source),
        Token::Prefix => prefix_decl(source),
        Token::Infix => infix_decl(source),
        Token::Postfix => postfix_decl(source),
        Token::Fn => function_decl(source),
        Token::Class => class_decl(source),
        Token::Implement => impl_decl(source),
        Token::Struct => struct_decl(source),
        Token::Union => union_decl(source),
        Token::Module => module_decl(source),
        Token::NewType => newtype_decl(source),
        _ => expr_decl(source)
    }
}

fn prefix_decl(source: SpanWith) -> IResult<SpanWith, Box<Decl>> {
    let (rest, (op, prec)) = preceded(token(Token::Prefix), pair(token(Token::Operator(Span::new(""))), token(Token::Int(Span::new("")))))(source)?;
    Ok((rest, Box::new(Decl::Operator{name: op, fixity: Fixity::Prefix, precedence: prec})))
}

fn infix_decl(source: SpanWith) -> IResult<SpanWith, Box<Decl>> {
    let (rest, (op, prec)) = preceded(token(Token::Infix), pair(token(Token::Operator(Span::new(""))), token(Token::Int(Span::new("")))))(source)?;
    Ok((rest, Box::new(Decl::Operator{name: op, fixity: Fixity::Infix, precedence: prec})))
}

fn postfix_decl(source: SpanWith) -> IResult<SpanWith, Box<Decl>> {
    let (rest, (op, prec)) = preceded(token(Token::Postfix), pair(token(Token::Operator(Span::new(""))), token(Token::Int(Span::new("")))))(source)?;
    Ok((rest, Box::new(Decl::Operator{name: op, fixity: Fixity::Postfix, precedence: prec})))
}

fn expr_decl(source: SpanWith) ->IResult<SpanWith, Box<Decl>> {
    let (rest, expr) = terminated(expression(false), token(Token::Semicolon))(source)?;
    Ok((rest, Box::new(Decl::Expr(expr))))
}

fn variable_decl(source: SpanWith) -> IResult<SpanWith, Box<Decl>> {
    let (rest, (mutable, pattern)) = preceded(token(Token::Let), pair(optional(token(Token::Mutable)), pattern))(source)?;
    let (rest, type_) = 
    if let (rest, Some(_)) = optional(token(Token::Colon))(rest)? {
        let (rest, result) = type_(rest)?;
        (rest, Some(result))
    }
    else {(rest, None)};
    let (rest, value) = 
    if let (rest, Some(_)) = optional(token(Token::Equal))(rest)? {
        let (rest, (result, _)) = pair(expression(false), token(Token::Semicolon))(rest)?;
        (rest, Some(result))
    }
    else {(rest, None)};
    Ok((rest, Box::new(Decl::Variable{mutable: mutable.is_some(), pattern: pattern, type_: type_, value: value})))
}

fn struct_decl(source: SpanWith) -> IResult<SpanWith, Box<Decl>> {
    let (rest, (name, members)) = preceded(token(Token::Struct), pair(applied_type,delimited(token(Token::Brace), param_list, token(Token::Brace))))(source)?;
    Ok((rest, Box::new(Decl::Struct{name, members})))
}

fn variant(source: SpanWith) -> IResult<SpanWith, Param> {
    let (rest, (name, type_)) = pair(identifier_pattern, optional(paren_or_tuple_type))(source)?;
    Ok((rest, Param{name, type_}))
}

fn variant_list(source: SpanWith) -> IResult<SpanWith, Vec<Param>> {
    separated_list0(token(Token::Comma), variant)(source)
}

fn union_decl(source: SpanWith) -> IResult<SpanWith, Box<Decl>> {
    let (rest, (name, variants)) = preceded(token(Token::Union), pair(applied_type, delimited(token(Token::Brace), variant_list, token(Token::CloseBrace))))(source)?;
    Ok((rest, Box::new(Decl::Union{name, variants})))
}

fn module_decl(source: SpanWith) -> IResult<SpanWith, Box<Decl>> {
    let (rest, (name, body)) = preceded(token(Token::Module), pair(token(Token::Identifier(Span::new(""))), block))(source)?;
    Ok((rest, Box::new(Decl::Module{name, body})))
}

fn newtype_decl(source: SpanWith) -> IResult<SpanWith, Box<Decl>> {
    let (rest, (defined, replacing)) = preceded(token(Token::NewType), pair(applied_type, preceded(token(Token::Equal), type_)))(source)?;
    Ok((rest, Box::new(Decl::NewType{defined, replacing})))
}

fn basic_array_type(source: SpanWith) -> IResult<SpanWith, Box<Ty>> {
    let (rest, size) = delimited(token(Token::Bracket), optional(integer), token(Token::CloseBracket))(source)?;
    Ok((rest, Box::new(Ty::ArrayBasic(size))))
}

fn integer(source: SpanWith) -> IResult<SpanWith, Token> {
    let (rest, t) = crate::lexer::scan_token(source)?;
    match t {
        Token::Int(_) |
        Token::OctInt(_) |
        Token::HexInt(_) |
        Token::BinInt(_)  => Ok((rest, t)),
        _ => fail(source)
    }
}

fn basic_type(source: SpanWith) -> IResult<SpanWith, Box<Ty>> {
    let s = source.clone();
    let (rest, t) = crate::lexer::scan_token(s)?;
    match t {
        Token::Bracket => basic_array_type(source),
        Token::Reference => Ok((rest, Box::new(Ty::Reference))),
        Token::Dereference => Ok((rest, Box::new(Ty::Pointer))),
        Token::Identifier(_) => Ok((rest, Box::new(Ty::Basic(t)))),
        _ => todo!()
    }
}

fn var_type(source: SpanWith) -> IResult<SpanWith, Box<Ty>> {
    let (rest, ty) = token(Token::TypeVar(Span::new("")))(source)?;
    Ok((rest, Box::new(Ty::Var(ty))))
}

fn var_or_basic_type(source: SpanWith) -> IResult<SpanWith, Box<Ty>> {
    let s = source.clone();
    let (rest, t) = crate::lexer::scan_token(s)?;
    match t {
        Token::TypeVar(_) => Ok((rest, Box::new(Ty::Var(t)))),
        _ => basic_type(source)
    }
}

fn param(source: SpanWith) -> IResult<SpanWith, Param> {
    let (rest, (name, type_)) = pair(pattern, optional(preceded(token(Token::Colon), type_)))(source)?;
    Ok((rest, Param{name: name, type_: type_}))
}

fn param_list(source: SpanWith) -> IResult<SpanWith, Vec<Param>> {
    terminated(separated_list0(token(Token::Comma), param), optional(token(Token::Comma)))(source)
}

fn applied_type(source: SpanWith) -> IResult<SpanWith, Box<Ty>> {
    let (rest, (app, vars)) = pair(var_or_basic_type, many0(tuple_type))(source)?;
    if vars.is_empty() {
        Ok((rest, app))
    }
    else {
        Ok((rest, Box::new(Ty::Applied(app, vars))))
    }
}

fn array_type_helper(source: SpanWith) -> IResult<SpanWith, Box<Ty>> {
    let (rest, (ty, size)) = delimited(
        token(Token::Bracket),
        pair(type_,
        optional(preceded(token(Token::Semicolon), integer))),
        token(Token::CloseBracket))(source)?;
    Ok((rest, Box::new(Ty::Array(ty, size))))
}

fn array_type(source: SpanWith) -> IResult<SpanWith, Box<Ty>> {
    let s = source.clone();
    let (_, t) = crate::lexer::scan_token(s)?;
    match t {
        Token::Bracket => array_type_helper(source),
        _ => applied_type(source)
    }
}

fn paren_or_tuple_type(source: SpanWith) -> IResult<SpanWith, Box<Ty>> {
    
    let (rest, _) = token(Token::Paren)(source)?;
    let (rest, ty) = optional(type_)(rest)?;
    match ty {
        None => {
            let (rest, _) = token(Token::Paren)(rest)?;
            Ok((rest, Box::new(Ty::Unit)))
        },
        Some(ty) => { 
            let (rest, paren) = optional(token(Token::CloseParen))(rest)?;
            match paren {
                Some(_) => Ok((rest, ty)),
                None => {
                    let (rest, ty_list) = terminated(separated_list1(token(Token::Comma), type_), pair(optional(token(Token::Comma)), token(Token::CloseParen)))(source)?;
                    Ok((rest, Box::new(Ty::Tuple(ty_list))))
                }
            }
        }
    }
}

fn tuple_type(source: SpanWith) -> IResult<SpanWith, Box<Ty>> {
    let s = source.clone();
    let (_, t) = crate::lexer::scan_token(s)?;
    match t {
        Token::Paren => paren_or_tuple_type(source),
        _ => array_type(source)
    }
}

fn function_type(source: SpanWith) -> IResult<SpanWith, Box<Ty>> {
    let (rest, (ty1, ty2)) = pair(array_type, optional(preceded(token(Token::Arrow), type_)))(source)?;
    match ty2 {
        None => Ok((rest, ty1)),
        Some(t) => Ok((rest, Box::new(Ty::Applied(Box::new(Ty::Basic(Token::Identifier(Span::new("(->)")))), vec![ty1, t]))))
    }
}

fn type_(source: SpanWith) -> IResult<SpanWith, Box<Ty>> {
    function_type(source)
}

fn pattern(source: SpanWith) -> IResult<SpanWith, Box<Pattern>> {
    let (rest, t) = crate::lexer::scan_token(source)?;
    match t {
        Token::Placeholder => Ok((rest, Box::new(Pattern::Placeholder))),
        Token::Identifier(_) => identifier_pattern(source),
        Token::Bracket => array_pattern(source),
        Token::Paren => tuple_pattern(source),
        Token::Int(_) |
        Token::OctInt(_) |
        Token::HexInt(_) |
        Token::BinInt(_) |
        Token::Float(_) |
        Token::_Double(_) |
        Token::Char(_) |
        Token::String(_) =>  Ok((rest, Box::new(Pattern::Literal(t)))),
        _ => fail(source)
    }
}

fn identifier_pattern(source: SpanWith) -> IResult<SpanWith, Box<Pattern>> {
    let (rest, (id, args)) = pair(token(Token::Identifier(Span::new(""))), optional(delimited(token(Token::Paren), pattern_list, token(Token::CloseParen))))(source)?;
    match args {
        None => Ok((rest, Box::new(Pattern::Identifier(id.as_span())))),
        Some(args) => Ok((rest, Box::new(Pattern::VariantUnwrap(Box::new(Pattern::Identifier(id.as_span())), args))))
    }
}

pub fn identifiers_from_pattern<'a>(pattern: &Pattern<'a>) -> Vec<Span<'a>> {
    match pattern {
        Pattern::Identifier(string) => vec![*string],
        Pattern::Literal(_)
        | Pattern::Placeholder => vec![],
        Pattern::VariantUnwrap(_, args)
        | Pattern::Array(args)
        | Pattern::Tuple(args) => {
            args.iter().map(Box::as_ref).flat_map(identifiers_from_pattern).collect()
        }
        Pattern::Error => vec![]
    }
}

fn array_pattern(source: SpanWith) ->IResult<SpanWith, Box<Pattern>> {
    let (rest, list) = delimited(token(Token::Bracket), pattern_list, token(Token::CloseBracket))(source)?;
    Ok((rest, Box::new(Pattern::Array(list))))
}

fn pattern_list(source: SpanWith) -> IResult<SpanWith, Vec<Box<Pattern>>> {
    terminated(separated_list1(token(Token::Comma), pattern), optional(token(Token::Comma)))(source)
}

fn tuple_pattern(source: SpanWith) -> IResult<SpanWith, Box<Pattern>> {
    let (rest, patterns) = delimited(token(Token::Paren), pattern_list, token(Token::CloseParen))(source)?;
    Ok((rest, Box::new(Pattern::Tuple(patterns))))
}

fn paren_or_tuple(source: SpanWith) -> IResult<SpanWith, Box<Expr>> {
    let (rest, _) = token(Token::Paren)(source)?;
    let (rest, expr) = optional(expression(false))(rest)?;
    match expr {
        None => {
            let (rest, _) = token(Token::Paren)(rest)?;
            Ok((rest, Box::new(Expr::Unit)))
        },
        Some(expr) => { 
            let (rest, paren) = optional(token(Token::CloseParen))(rest)?;
            match paren {
                Some(_) => Ok((rest, expr)),
                None => {
                    let (rest, expr_list) = terminated(separated_list1(token(Token::Comma), expression(false)), pair(optional(token(Token::Comma)), token(Token::CloseParen)))(source)?;
                    Ok((rest, Box::new(Expr::Tuple(expr_list))))
                }
            }
        }
    }
}

fn identifier_or_path(source: SpanWith) -> IResult<SpanWith, Box<Expr>> {
    let (rest, (id, path)) = pair(token(Token::Identifier(Span::new(""))), optional(token(Token::Path)))(source)?;
    let identifier =Box::new(Expr::Identifier(id));
    match path {
        None => Ok((rest, identifier)),
        Some(_) => {
            let (rest, expr) = expression(false)(rest)?;
            Ok((rest, Box::new(Expr::Path{identifier, expr})))
        }
    }
}

fn literal(source: SpanWith) -> IResult<SpanWith, Box<Expr>> {
    let (rest, t) = crate::lexer::scan_token(source)?;
    match t {
        Token::Int(_) |
        Token::OctInt(_) |
        Token::HexInt(_) |
        Token::BinInt(_) |
        Token::Float(_) |
        Token::_Double(_) |
        Token::Char(_) |
        Token::String(_) =>  Ok((rest, Box::new(Expr::Literal(t)))),
        _ => fail(source)
    }
}

fn prefix_operator(source: SpanWith) -> IResult<SpanWith, Box<Expr>> {
    let (rest, (op, expr)) = pair(operator, expression(false))(source)?;
    Ok((rest, Box::new(Expr::Prefix{op: op, expr: expr})))
}