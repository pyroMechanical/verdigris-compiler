use nom::{
    IResult,
    branch::alt,
    combinator::{opt, fail},
    multi::{many0, many1, separated_list0, separated_list1},
    sequence::{preceded, delimited, terminated, pair, tuple},
};
use nom_locate::LocatedSpan;
use nom_supreme::{
    final_parser::final_parser
};
use crate::lexer::{
    Token, hex_int, bin_int, oct_int, integer,
    Error,
    SpanWith
};

type Span<'a> = LocatedSpan<&'a str>;

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
    StructInit(Vec<Box<Expr<'a>>>)
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
    Tuple(Vec<Box<Ty<'a>>>)
}

#[derive(Clone, Debug)]
pub enum Pattern<'a> {
    Identifier(Span<'a>),
    Literal(Token<'a>),
    Tuple(Vec<Box<Pattern<'a>>>),
    Placeholder,
    Array(Vec<Box<Pattern<'a>>>),
    VariantUnwrap(Box<Pattern<'a>>, Vec<Box<Pattern<'a>>>)
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
}

pub fn parse<'a, 'b>(source: &'a str) -> Result<Vec<Box<Decl<'a>>>, nom::error::Error<SpanWith<'a, 'b, Vec<Error<'a>>>>> {
    let source_span = Span::new(source);
    let result = final_parser(many1(declaration))(source_span);
    match result {
        Ok(expr) => {/*println!("{:?}", expr);*/ Ok(expr)},
        e => e
    }
}

fn token<'a, 'b>(t: Token<'static>) -> impl Fn(SpanWith<'a, 'b, Vec<Error<'a>>>) -> IResult<SpanWith<'a, 'b, Vec<Error<'a>>>, Token<'a>> where 'a: 'b {
    let discriminant = std::mem::discriminant(&t);
    move |source| {
        let (rest, token) = crate::lexer::scan_token(source)?;
        if discriminant == std::mem::discriminant(&token) {return Ok((rest, token));}
        else { return fail(source);}
    }
}

fn expression<'a, 'b>(scrutinee: bool) -> impl Fn(SpanWith<'a, 'b, Vec<Error<'a>>>) -> IResult<SpanWith<'a, 'b, Vec<Error<'a>>>, Box<Expr<'a>>> where 'a: 'b{
    move |source| { 
        let (rest, token) = crate::lexer::scan_token(source)?;
        let (string, expr) = match token {
            Token::If => if_(source)?,
            Token::While => while_(source)?,
            Token::Unsafe => unsafe_(source)?,
            Token::Loop => loop_(source)?,
            Token::Switch => switch_(source)?,
            Token::Brace => block(source)?,
            Token::Bracket => array_constructor(source)?,
            Token::Prefix => prefix_operator(source)?,
            Token::Paren => alt((tuples, parens))(source)?,
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

fn right_expression<'a, 'b>(expr: Box<Expr<'a>>, source: SpanWith<'a, 'b, Vec<Error<'a>>>, scrutinee: bool) -> IResult <SpanWith<'a, 'b, Vec<Error<'a>>>, Box<Expr<'a>>> {
    let (_, token) = crate::lexer::scan_token(source)?;

    let (rest, lr) = match token {
        Token::Reference => opt(reference)(source)?,
        Token::Dereference => opt(dereference)(source)?,
        Token::Operator(_) => opt(alt((binary(scrutinee), postfix_operator)))(source)?,
        Token::Dot => opt(field_call(scrutinee))(source)?,
        Token::Paren => opt(function_call)(source)?,
        Token::Bracket => opt(array_index)(source)?,
        Token::Equal => opt(assignment(scrutinee))(source)?,
        Token::Brace => if scrutinee {(source, None)} else {opt(struct_init)(source)?},
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
            LRExpr::StructInit(args) => right_expression(Box::new(Expr::StructInit{id: expr, args: args}), rest, scrutinee)
        }
    }
}

fn reference<'a, 'b>(source:SpanWith<'a, 'b, Vec<Error<'a>>>) -> IResult<SpanWith<'a, 'b, Vec<Error<'a>>>, Box<LRExpr<'a>>>{
    let (rest, _) = token(Token::Reference)(source)?;
    let (rest, x) = opt(token(Token::Mutable))(rest)?;
    let is_mut = x.is_some();
    Ok((rest, Box::new(LRExpr::Reference(is_mut))))
}

fn dereference<'a, 'b>(source: SpanWith<'a, 'b, Vec<Error<'a>>>) -> IResult<SpanWith<'a, 'b, Vec<Error<'a>>>, Box<LRExpr<'a>>> {
    let (rest, _) = token(Token::Dereference)(source)?;
    Ok((rest, Box::new(LRExpr::Dereference)))
}

fn binary<'a, 'b>(scrutinee: bool) -> impl Fn(SpanWith<'a, 'b, Vec<Error<'a>>>) -> IResult<SpanWith<'a, 'b, Vec<Error<'a>>>, Box<LRExpr<'a>>> where 'a: 'b {
    move |source| {
        let (rest, (op, expr)) = pair(operator, expression(scrutinee))(source)?;
        Ok((rest, Box::new(LRExpr::Binary(op, expr))))
    }
}

fn assignment<'a, 'b>(scrutinee: bool) -> impl Fn(SpanWith<'a, 'b, Vec<Error<'a>>>) -> IResult<SpanWith<'a, 'b, Vec<Error<'a>>>, Box<LRExpr<'a>>> where 'a: 'b {
    move |source| {
        let (rest, value) = preceded(token(Token::Equal), expression(scrutinee))(source)?;
        Ok((rest, Box::new(LRExpr::Assignment(value))))
    }
}

fn field_call<'a, 'b>(scrutinee: bool) -> impl Fn(SpanWith<'a, 'b, Vec<Error<'a>>>) -> IResult<SpanWith<'a, 'b, Vec<Error<'a>>>, Box<LRExpr<'a>>> where 'a: 'b {
    move |source| {
        let (rest, expr) = preceded(token(Token::Dot), expression(scrutinee))(source)?;
        Ok((rest, Box::new(LRExpr::FieldCall(expr))))
    }
}

fn postfix_operator<'a, 'b>(source: SpanWith<'a, 'b, Vec<Error<'a>>>) -> IResult<SpanWith<'a, 'b, Vec<Error<'a>>>, Box<LRExpr<'a>>> {
    let (rest, op) = operator(source)?;
    Ok((rest, Box::new(LRExpr::Postfix(op))))
}

fn function_call<'a, 'b>(source: SpanWith<'a, 'b, Vec<Error<'a>>>) -> IResult<SpanWith<'a, 'b, Vec<Error<'a>>>, Box<LRExpr<'a>>> {
    let (rest, list) = delimited(token(Token::Paren), expr_list, token(Token::CloseParen))(source)?;
    Ok((rest, Box::new(LRExpr::FunctionCall(list))))
}

fn array_index<'a, 'b>(source: SpanWith<'a, 'b, Vec<Error<'a>>>) -> IResult<SpanWith<'a, 'b, Vec<Error<'a>>>, Box<LRExpr<'a>>> {
    let (rest, expr) = delimited(token(Token::Bracket), expression(false), token(Token::CloseBracket))(source)?;
    Ok((rest, Box::new(LRExpr::ArrayIndex(expr))))
}

fn struct_init<'a, 'b>(source: SpanWith<'a, 'b, Vec<Error<'a>>>) -> IResult<SpanWith<'a, 'b, Vec<Error<'a>>>, Box<LRExpr<'a>>> {
    let (rest, list) = delimited(token(Token::Brace), expr_list, token(Token::CloseBrace))(source)?;
    Ok((rest, Box::new(LRExpr::StructInit(list))))
}
fn expr_list<'a, 'b>(source: SpanWith<'a, 'b, Vec<Error<'a>>>) -> IResult<SpanWith<'a, 'b, Vec<Error<'a>>>, Vec<Box<Expr<'a>>>> {
    terminated(separated_list0(token(Token::Comma), expression(false)), opt(token(Token::Comma)))(source)
}

fn array_constructor<'a, 'b>(source: SpanWith<'a, 'b, Vec<Error<'a>>>) -> IResult<SpanWith<'a, 'b, Vec<Error<'a>>>, Box<Expr<'a>>> {
    let (rest, values) = delimited(token(Token::Bracket), expr_list, token(Token::CloseBracket))(source)?;
    Ok((rest, Box::new(Expr::ArrayConstructor(values))))
}

fn tuples<'a, 'b>(source: SpanWith<'a, 'b, Vec<Error<'a>>>) -> IResult<SpanWith<'a, 'b, Vec<Error<'a>>>, Box<Expr<'a>>> {
    let (rest, list) = delimited(token(Token::Paren), expr_list, token(Token::CloseParen))(source)?;
    Ok((rest, Box::new(Expr::Tuple(list))))
}

fn unsafe_<'a, 'b>(source: SpanWith<'a, 'b, Vec<Error<'a>>>) -> IResult<SpanWith<'a, 'b, Vec<Error<'a>>>, Box<Expr<'a>>> {
    let (rest, expr) = preceded(token(Token::Unsafe), expression(false))(source)?;
    Ok((rest, Box::new(Expr::Unsafe(expr))))
}

fn if_<'a, 'b>(source: SpanWith<'a, 'b, Vec<Error<'a>>>) -> IResult<SpanWith<'a, 'b, Vec<Error<'a>>>, Box<Expr<'a>>> {
    let (rest, ((cond, true_branch), else_branch)) = pair(pair(preceded(token(Token::If), expression(true)), block), opt(preceded(token(Token::Else), block)))(source)?;
    Ok((rest, Box::new(Expr::If{cond: cond, true_branch: true_branch, else_branch: else_branch})))
}

fn while_<'a, 'b>(source: SpanWith<'a, 'b, Vec<Error<'a>>>) -> IResult<SpanWith<'a, 'b, Vec<Error<'a>>>, Box<Expr<'a>>> {
    let (rest, (cond, loop_)) = pair(preceded(token(Token::While), expression(true)), block)(source)?;
    Ok((rest, Box::new(Expr::While{cond: cond, loop_: loop_})))
}

fn loop_<'a, 'b>(source: SpanWith<'a, 'b, Vec<Error<'a>>>) -> IResult<SpanWith<'a, 'b, Vec<Error<'a>>>, Box<Expr<'a>>> {
    let (rest, loop_) = preceded(token(Token::Loop), block)(source)?;
    Ok((rest, Box::new(Expr::Loop{loop_: loop_})))
}

fn case<'a, 'b>(source: SpanWith<'a, 'b, Vec<Error<'a>>>) -> IResult<SpanWith<'a, 'b, Vec<Error<'a>>>, Box<CaseExpr<'a>>> {
    let (rest, (pattern, expr)) = preceded(token(Token::Case), pair(terminated(pattern, token(Token::Colon)), expression(false)))(source)?;
    Ok((rest, Box::new(CaseExpr(pattern, expr))))
}

fn case_block<'a, 'b>(source: SpanWith<'a, 'b, Vec<Error<'a>>>) -> IResult<SpanWith<'a, 'b, Vec<Error<'a>>>, Vec<Box<CaseExpr<'a>>>> {
    delimited(token(Token::Brace), terminated(separated_list0(token(Token::Comma), case), opt(token(Token::Comma))), token(Token::CloseBrace))(source)
}

fn switch_<'a, 'b>(source: SpanWith<'a, 'b, Vec<Error<'a>>>) -> IResult<SpanWith<'a, 'b, Vec<Error<'a>>>, Box<Expr<'a>>> {
    let(rest, (expr, cases)) = pair(preceded(token(Token::Switch), expression(true)), case_block)(source)?;
    Ok((rest, Box::new(Expr::Switch{expr: expr, arms: cases})))
}

fn operator<'a, 'b>(source: SpanWith<'a, 'b, Vec<Error<'a>>>) -> IResult<SpanWith<'a, 'b, Vec<Error<'a>>>, Token<'a>> {
    token(Token::Operator(Span::new("")))(source)
}

fn return_expr<'a, 'b>(source: SpanWith<'a, 'b, Vec<Error<'a>>>) -> IResult<SpanWith<'a, 'b, Vec<Error<'a>>>, Box<Expr<'a>>> {
    let (rest, expr) = preceded(token(Token::Return), expression(false))(source)?;
    Ok((rest, Box::new(Expr::Return(expr))))
}

fn block<'a, 'b>(source: SpanWith<'a, 'b, Vec<Error<'a>>>) -> IResult<SpanWith<'a, 'b, Vec<Error<'a>>>, Box<Expr<'a>>> {
    let (rest, (list, ret)) = delimited(token(Token::Brace), pair(many0(declaration), opt(expression(false))), token(Token::CloseBrace))(source)?;
    Ok((rest, Box::new(Expr::Block{declarations: list, ret})))
}

fn function_decl<'a, 'b>(source: SpanWith<'a, 'b, Vec<Error<'a>>>) -> IResult<SpanWith<'a, 'b, Vec<Error<'a>>>, Box<Decl<'a>>> {
    let (rest, (name, args, return_type, body)) = preceded(token(Token::Fn), tuple((token(Token::Identifier(Span::new(""))), delimited(token(Token::Paren), param_list, token(Token::CloseParen)), opt(preceded(token(Token::Arrow), type_)), block) ))(source)?;
    Ok((rest, Box::new(Decl::Function{name: name.as_span(), args: args, return_type: return_type, body: body})))
}

fn single_constraint<'a, 'b>(source: SpanWith<'a, 'b, Vec<Error<'a>>>) -> IResult<SpanWith<'a, 'b, Vec<Error<'a>>>, Vec<Box<Ty<'a>>>> {
    let (rest, constraint) = array_type(source)?;
    Ok((rest, vec![constraint]))
}

fn constraints<'a, 'b>(source: SpanWith<'a, 'b, Vec<Error<'a>>>) -> IResult<SpanWith<'a, 'b, Vec<Error<'a>>>, Vec<Box<Ty<'a>>>> {
    alt((single_constraint,delimited(token(Token::Paren), separated_list1(token(Token::Comma), array_type), token(Token::CloseParen))))(source)
}

fn class_decl<'a, 'b>(source: SpanWith<'a, 'b, Vec<Error<'a>>>) -> IResult<SpanWith<'a, 'b, Vec<Error<'a>>>, Box<Decl<'a>>> {
    let (rest, (constraints, class, types, body)) = preceded(token(Token::Class), tuple((opt(terminated(constraints, token(Token::WideArrow))), token(Token::Identifier(Span::new(""))), many1(var_type), block)))(source)?;
    Ok((rest, Box::new(Decl::Class{class: class, types: types, constraints: constraints, body: body})))
}

fn impl_decl<'a, 'b>(source: SpanWith<'a, 'b, Vec<Error<'a>>>) -> IResult<SpanWith<'a, 'b, Vec<Error<'a>>>, Box<Decl<'a>>> {
    let (rest, (constraints, class, types, body)) = preceded(token(Token::Implement), tuple((opt(terminated(constraints, token(Token::WideArrow))), token(Token::Identifier(Span::new(""))), many1(type_), block)))(source)?;
    Ok((rest, Box::new(Decl::Implementation{class: class, types: types, constraints: constraints, body: body})))
}

fn declaration<'a, 'b>(source: SpanWith<'a, 'b, Vec<Error<'a>>>) -> IResult<SpanWith<'a, 'b, Vec<Error<'a>>>, Box<Decl<'a>>> {
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

fn prefix_decl<'a, 'b>(source: SpanWith<'a, 'b, Vec<Error<'a>>>) -> IResult<SpanWith<'a, 'b, Vec<Error<'a>>>, Box<Decl<'a>>> {
    let (rest, (op, prec)) = preceded(token(Token::Prefix), pair(token(Token::Operator(Span::new(""))), token(Token::Int(Span::new("")))))(source)?;
    Ok((rest, Box::new(Decl::Operator{name: op, fixity: Fixity::Prefix, precedence: prec})))
}

fn infix_decl<'a, 'b>(source: SpanWith<'a, 'b, Vec<Error<'a>>>) -> IResult<SpanWith<'a, 'b, Vec<Error<'a>>>, Box<Decl<'a>>> {
    let (rest, (op, prec)) = preceded(token(Token::Infix), pair(token(Token::Operator(Span::new(""))), token(Token::Int(Span::new("")))))(source)?;
    Ok((rest, Box::new(Decl::Operator{name: op, fixity: Fixity::Infix, precedence: prec})))
}

fn postfix_decl<'a, 'b>(source: SpanWith<'a, 'b, Vec<Error<'a>>>) -> IResult<SpanWith<'a, 'b, Vec<Error<'a>>>, Box<Decl<'a>>> {
    let (rest, (op, prec)) = preceded(token(Token::Postfix), pair(token(Token::Operator(Span::new(""))), token(Token::Int(Span::new("")))))(source)?;
    Ok((rest, Box::new(Decl::Operator{name: op, fixity: Fixity::Postfix, precedence: prec})))
}

fn expr_decl<'a, 'b>(source: SpanWith<'a, 'b, Vec<Error<'a>>>) ->IResult<SpanWith<'a, 'b, Vec<Error<'a>>>, Box<Decl<'a>>> {
    let (rest, expr) = terminated(expression(false), token(Token::Semicolon))(source)?;
    Ok((rest, Box::new(Decl::Expr(expr))))
}

fn variable_decl<'a, 'b>(source: SpanWith<'a, 'b, Vec<Error<'a>>>) -> IResult<SpanWith<'a, 'b, Vec<Error<'a>>>, Box<Decl<'a>>> {
    let (rest, (mutable, pattern)) = preceded(token(Token::Let), pair(opt(token(Token::Mutable)), pattern))(source)?;
    let (rest, type_) = 
    if let (rest, Some(_)) = opt(token(Token::Colon))(rest)? {
        let (rest, result) = type_(rest)?;
        (rest, Some(result))
    }
    else {(rest, None)};
    let (rest, value) = 
    if let (rest, Some(_)) = opt(token(Token::Equal))(rest)? {
        let (rest, (result, _)) = pair(expression(false), token(Token::Semicolon))(rest)?;
        (rest, Some(result))
    }
    else {(rest, None)};
    Ok((rest, Box::new(Decl::Variable{mutable: mutable.is_some(), pattern: pattern, type_: type_, value: value})))
}

fn struct_decl<'a, 'b>(source: SpanWith<'a, 'b, Vec<Error<'a>>>) -> IResult<SpanWith<'a, 'b, Vec<Error<'a>>>, Box<Decl<'a>>> {
    let (rest, (name, members)) = preceded(token(Token::Struct), pair(applied_type,delimited(token(Token::Brace), param_list, token(Token::Brace))))(source)?;
    Ok((rest, Box::new(Decl::Struct{name, members})))
}

fn variant<'a, 'b>(source: SpanWith<'a, 'b, Vec<Error<'a>>>) -> IResult<SpanWith<'a, 'b, Vec<Error<'a>>>, Param<'a>> {
    let (rest, (name, type_)) = pair(identifier_pattern, opt(alt((tuple_type, delimited(token(Token::Paren), type_, token(Token::CloseParen))))))(source)?;
    Ok((rest, Param{name, type_}))
}

fn variant_list<'a, 'b>(source: SpanWith<'a, 'b, Vec<Error<'a>>>) -> IResult<SpanWith<'a, 'b, Vec<Error<'a>>>, Vec<Param<'a>>> {
    separated_list0(token(Token::Comma), variant)(source)
}

fn union_decl<'a, 'b>(source: SpanWith<'a, 'b, Vec<Error<'a>>>) -> IResult<SpanWith<'a, 'b, Vec<Error<'a>>>, Box<Decl<'a>>> {
    let (rest, (name, variants)) = preceded(token(Token::Union), pair(applied_type, delimited(token(Token::Brace), variant_list, token(Token::CloseBrace))))(source)?;
    Ok((rest, Box::new(Decl::Union{name, variants})))
}

fn module_decl<'a, 'b>(source: SpanWith<'a, 'b, Vec<Error<'a>>>) -> IResult<SpanWith<'a, 'b, Vec<Error<'a>>>, Box<Decl<'a>>> {
    let (rest, (name, body)) = preceded(token(Token::Module), pair(token(Token::Identifier(Span::new(""))), block))(source)?;
    Ok((rest, Box::new(Decl::Module{name, body})))
}

fn newtype_decl<'a, 'b>(source: SpanWith<'a, 'b, Vec<Error<'a>>>) -> IResult<SpanWith<'a, 'b, Vec<Error<'a>>>, Box<Decl<'a>>> {
    let (rest, (defined, replacing)) = preceded(token(Token::NewType), pair(applied_type, preceded(token(Token::Equal), type_)))(source)?;
    Ok((rest, Box::new(Decl::NewType{defined, replacing})))
}

fn basic_array_type<'a, 'b>(source: SpanWith<'a, 'b, Vec<Error<'a>>>) -> IResult<SpanWith<'a, 'b, Vec<Error<'a>>>, Box<Ty<'a>>> {
    let (rest, size) = delimited(token(Token::Bracket), opt(alt((integer, hex_int, oct_int, bin_int))), token(Token::CloseBracket))(source)?;
    Ok((rest, Box::new(Ty::ArrayBasic(size))))
}

fn basic_unit_type<'a, 'b>(source: SpanWith<'a, 'b, Vec<Error<'a>>>) -> IResult<SpanWith<'a, 'b, Vec<Error<'a>>>, Box<Ty<'a>>> {
    let (rest, _) = pair(token(Token::Paren), token(Token::CloseParen))(source)?;
    Ok((rest, Box::new(Ty::Unit)))
}

fn basic_ref_type<'a, 'b>(source: SpanWith<'a, 'b, Vec<Error<'a>>>) -> IResult<SpanWith<'a, 'b, Vec<Error<'a>>>, Box<Ty<'a>>> {
    let(rest, _) = token(Token::Reference)(source)?;
    Ok((rest, Box::new(Ty::Reference)))
}

fn basic_pointer_type<'a, 'b>(source: SpanWith<'a, 'b, Vec<Error<'a>>>) -> IResult<SpanWith<'a, 'b, Vec<Error<'a>>>, Box<Ty<'a>>> {
    let(rest, _) = token(Token::Dereference)(source)?;
    Ok((rest, Box::new(Ty::Pointer)))
}

fn basic_type_string<'a, 'b>(source: SpanWith<'a, 'b, Vec<Error<'a>>>) -> IResult<SpanWith<'a, 'b, Vec<Error<'a>>>, Box<Ty<'a>>> {
    let (rest, ty) = token(Token::Identifier(Span::new("")))(source)?;
    Ok((rest, Box::new(Ty::Basic(ty))))
}

fn basic_type<'a, 'b>(source: SpanWith<'a, 'b, Vec<Error<'a>>>) -> IResult<SpanWith<'a, 'b, Vec<Error<'a>>>, Box<Ty<'a>>> {
    alt((basic_array_type, basic_unit_type, basic_ref_type, basic_pointer_type, basic_type_string))(source)
}

fn var_type<'a, 'b>(source: SpanWith<'a, 'b, Vec<Error<'a>>>) -> IResult<SpanWith<'a, 'b, Vec<Error<'a>>>, Box<Ty<'a>>> {
    let (rest, ty) = token(Token::TypeVar(Span::new("")))(source)?;
    Ok((rest, Box::new(Ty::Var(ty))))
}

fn var_or_basic_type<'a, 'b>(source: SpanWith<'a, 'b, Vec<Error<'a>>>) -> IResult<SpanWith<'a, 'b, Vec<Error<'a>>>, Box<Ty<'a>>> {
    alt((var_type, basic_type))(source)
}

fn param<'a, 'b>(source: SpanWith<'a, 'b, Vec<Error<'a>>>) -> IResult<SpanWith<'a, 'b, Vec<Error<'a>>>, Param<'a>> {
    let (rest, (name, type_)) = pair(pattern, opt(preceded(token(Token::Colon), type_)))(source)?;
    Ok((rest, Param{name: name, type_: type_}))
}

fn param_list<'a, 'b>(source: SpanWith<'a, 'b, Vec<Error<'a>>>) -> IResult<SpanWith<'a, 'b, Vec<Error<'a>>>, Vec<Param<'a>>> {
    terminated(separated_list0(token(Token::Comma), param), opt(token(Token::Comma)))(source)
}

fn applied_type<'a, 'b>(source: SpanWith<'a, 'b, Vec<Error<'a>>>) -> IResult<SpanWith<'a, 'b, Vec<Error<'a>>>, Box<Ty<'a>>> {
    let (rest, (app, vars)) = pair(var_or_basic_type, many0(tuple_type))(source)?;
    if vars.is_empty() {
        Ok((rest, app))
    }
    else {
        Ok((rest, Box::new(Ty::Applied(app, vars))))
    }
}

fn array_type_helper<'a, 'b>(source: SpanWith<'a, 'b, Vec<Error<'a>>>) -> IResult<SpanWith<'a, 'b, Vec<Error<'a>>>, Box<Ty<'a>>> {
    let (rest, (ty, size)) = delimited(
        token(Token::Bracket),
        pair(type_,
        opt(preceded(token(Token::Semicolon), alt((hex_int, bin_int, oct_int, integer))))),
        token(Token::CloseBracket))(source)?;
    Ok((rest, Box::new(Ty::Array(ty, size))))
}

fn array_type<'a, 'b>(source: SpanWith<'a, 'b, Vec<Error<'a>>>) -> IResult<SpanWith<'a, 'b, Vec<Error<'a>>>, Box<Ty<'a>>> {
    alt((array_type_helper, applied_type))(source)
}

fn parenthesized_type<'a, 'b>(source: SpanWith<'a, 'b, Vec<Error<'a>>>) -> IResult<SpanWith<'a, 'b, Vec<Error<'a>>>, Box<Ty<'a>>> {
    alt((delimited(token(Token::Paren), type_, token(Token::Paren)), array_type))(source)
}

fn type_list<'a, 'b>(source: SpanWith<'a, 'b, Vec<Error<'a>>>) -> IResult<SpanWith<'a, 'b, Vec<Error<'a>>>, Vec<Box<Ty<'a>>>> {
    terminated(separated_list1(token(Token::Comma), type_), opt(token(Token::Comma)))(source)
}

fn tuple_type_helper<'a, 'b>(source: SpanWith<'a, 'b, Vec<Error<'a>>>) -> IResult<SpanWith<'a, 'b, Vec<Error<'a>>>, Box<Ty<'a>>> {
    let (rest, types) = delimited(token(Token::Paren), type_list, token(Token::CloseParen))(source)?;
    Ok((rest, Box::new(Ty::Tuple(types))))
}

fn tuple_type<'a, 'b>(source: SpanWith<'a, 'b, Vec<Error<'a>>>) -> IResult<SpanWith<'a, 'b, Vec<Error<'a>>>, Box<Ty<'a>>> {
    alt((tuple_type_helper, parenthesized_type))(source)
}

fn function_type<'a, 'b>(source: SpanWith<'a, 'b, Vec<Error<'a>>>) -> IResult<SpanWith<'a, 'b, Vec<Error<'a>>>, Box<Ty<'a>>> {
    let (rest, (ty1, ty2)) = pair(array_type, opt(preceded(token(Token::Arrow), type_)))(source)?;
    match ty2 {
        None => Ok((rest, ty1)),
        Some(t) => Ok((rest, Box::new(Ty::Applied(Box::new(Ty::Basic(Token::Identifier(Span::new("(->)")))), vec![ty1, t]))))
    }
}

fn type_<'a, 'b>(source: SpanWith<'a, 'b, Vec<Error<'a>>>) -> IResult<SpanWith<'a, 'b, Vec<Error<'a>>>, Box<Ty<'a>>> {
    function_type(source)
}

fn pattern<'a, 'b>(source: SpanWith<'a, 'b, Vec<Error<'a>>>) -> IResult<SpanWith<'a, 'b, Vec<Error<'a>>>, Box<Pattern<'a>>> {
    alt((placeholder_pattern, array_pattern, tuple_pattern, literal_pattern, identifier_pattern))(source)
}

fn literal_pattern<'a, 'b>(source: SpanWith<'a, 'b, Vec<Error<'a>>>) -> IResult<SpanWith<'a, 'b, Vec<Error<'a>>>, Box<Pattern<'a>>> {
    let (rest, token) = alt((token(Token::Float(Span::new(""))), token(Token::Int(Span::new(""))), token(Token::HexInt(Span::new(""))), token(Token::BinInt(Span::new(""))), token(Token::OctInt(Span::new(""))), token(Token::Char(Span::new(""))), token(Token::String(Span::new("")))))(source)?;
    Ok((rest, Box::new(Pattern::Literal(token))))
}

fn identifier_pattern<'a, 'b>(source: SpanWith<'a, 'b, Vec<Error<'a>>>) -> IResult<SpanWith<'a, 'b, Vec<Error<'a>>>, Box<Pattern<'a>>> {
    let (rest, (id, args)) = pair(token(Token::Identifier(Span::new(""))), opt(delimited(token(Token::Paren), pattern_list, token(Token::CloseParen))))(source)?;
    match args {
        None => Ok((rest, Box::new(Pattern::Identifier(id.as_span())))),
        Some(args) => Ok((rest, Box::new(Pattern::VariantUnwrap(Box::new(Pattern::Identifier(id.as_span())), args))))
    }
}

pub fn identifiers_from_pattern<'a, 'b>(pattern: &Pattern<'a>) -> Vec<Span<'a>> {
    match pattern {
        Pattern::Identifier(string) => vec![*string],
        Pattern::Literal(_)
        | Pattern::Placeholder => vec![],
        Pattern::VariantUnwrap(_, args)
        | Pattern::Array(args)
        | Pattern::Tuple(args) => {
            args.iter().map(Box::as_ref).flat_map(identifiers_from_pattern).collect()
        }
    }
}

fn array_pattern<'a, 'b>(source: SpanWith<'a, 'b, Vec<Error<'a>>>) ->IResult<SpanWith<'a, 'b, Vec<Error<'a>>>, Box<Pattern<'a>>> {
    let (rest, list) = delimited(token(Token::Bracket), pattern_list, token(Token::CloseBracket))(source)?;
    Ok((rest, Box::new(Pattern::Array(list))))
}

fn placeholder_pattern<'a, 'b>(source: SpanWith<'a, 'b, Vec<Error<'a>>>) -> IResult<SpanWith<'a, 'b, Vec<Error<'a>>>, Box<Pattern<'a>>> {
    let (rest, _) = token(Token::Placeholder)(source)?;
    Ok((rest, Box::new(Pattern::Placeholder)))
}

fn pattern_list<'a, 'b>(source: SpanWith<'a, 'b, Vec<Error<'a>>>) -> IResult<SpanWith<'a, 'b, Vec<Error<'a>>>, Vec<Box<Pattern<'a>>>> {
    terminated(separated_list1(token(Token::Comma), pattern), opt(token(Token::Comma)))(source)
}

fn tuple_pattern<'a, 'b>(source: SpanWith<'a, 'b, Vec<Error<'a>>>) -> IResult<SpanWith<'a, 'b, Vec<Error<'a>>>, Box<Pattern<'a>>> {
    let (rest, patterns) = delimited(token(Token::Paren), pattern_list, token(Token::CloseParen))(source)?;
    Ok((rest, Box::new(Pattern::Tuple(patterns))))
}

fn parens<'a, 'b>(source: SpanWith<'a, 'b, Vec<Error<'a>>>) -> IResult<SpanWith<'a, 'b, Vec<Error<'a>>>, Box<Expr<'a>>>{
    Ok(delimited(token(Token::Paren), expression(false), token(Token::CloseParen))(source)?)
}

fn identifier<'a, 'b>(source: SpanWith<'a, 'b, Vec<Error<'a>>>) -> IResult<SpanWith<'a, 'b, Vec<Error<'a>>>, Box<Expr<'a>>> {
    let (rest, ident) = token(Token::Identifier(Span::new("")))(source)?;
    Ok((rest, Box::new(Expr::Identifier(ident))))
}

fn path<'a, 'b>(source: SpanWith<'a, 'b, Vec<Error<'a>>>) -> IResult<SpanWith<'a, 'b, Vec<Error<'a>>>, Box<Expr<'a>>> {
    let (rest, (id, path)) = pair(identifier, preceded(token(Token::Path), expression(false)))(source)?;
    Ok((rest, Box::new(Expr::Path{identifier: id, expr: path})))
}

fn identifier_or_path<'a, 'b>(source: SpanWith<'a, 'b, Vec<Error<'a>>>) -> IResult<SpanWith<'a, 'b, Vec<Error<'a>>>, Box<Expr<'a>>> {
    alt((path, identifier))(source)
}

fn literal<'a, 'b>(source: SpanWith<'a, 'b, Vec<Error<'a>>>) -> IResult<SpanWith<'a, 'b, Vec<Error<'a>>>, Box<Expr<'a>>> {
    let (rest, token) = alt((token(Token::Float(Span::new(""))), token(Token::Int(Span::new(""))), token(Token::HexInt(Span::new(""))), token(Token::BinInt(Span::new(""))), token(Token::OctInt(Span::new(""))), token(Token::Char(Span::new(""))), token(Token::String(Span::new("")))))(source)?;
    Ok((rest, Box::new(Expr::Literal(token))))
}

fn prefix_operator<'a, 'b>(source: SpanWith<'a, 'b, Vec<Error<'a>>>) -> IResult<SpanWith<'a, 'b, Vec<Error<'a>>>, Box<Expr<'a>>> {
    let (rest, (op, expr)) = pair(operator, expression(false))(source)?;
    Ok((rest, Box::new(Expr::Prefix{op: op, expr: expr})))
}