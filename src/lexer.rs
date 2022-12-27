use nom::{
    IResult,
    multi::{many0, many1},
    branch::alt,
    character::complete::{anychar, alpha1, alphanumeric1, multispace0, one_of, none_of, digit1, hex_digit1, oct_digit1, not_line_ending},
    combinator::{fail, recognize, peek, not, opt, eof, value},
    sequence::{delimited, preceded, terminated, tuple, pair}
};
use nom_supreme::tag::complete::tag;
use crate::{Span, SpanWith, to_span};

#[derive(Clone, Debug, PartialEq)]
pub enum Token<'a> {
    Paren, 
    CloseParen,
    Brace, 
    CloseBrace,
    Bracket, 
    CloseBracket,
    Comma, 
    Dot,
    Arrow, 
    WideArrow,
    Colon,
    Equal, 
    Semicolon,
    Reference,
    Dereference,
    TypeVar(Span<'a>),
    _TypeOwned(String),
    Let,
    Lifetime(Span<'a>),
    Fn,
    Unsafe,
    Identifier(Span<'a>),
    Operator(Span<'a>),
    Placeholder,
    NewType,
    Switch,
    Path,
    Case,
    Class,
    Implement,
    Using,
    Lambda,
    Mutable,
    Module,
    True,
    False,
    Float(Span<'a>),
    _Double(Span<'a>),
    Char(Span<'a>),
    Int(Span<'a>),
    HexInt(Span<'a>),
    OctInt(Span<'a>),
    BinInt(Span<'a>),
    String(Span<'a>),
    _Unit, // "()"
    If, 
    While, 
    For, 
    Loop,
    Infix, 
    Prefix, 
    Postfix,
    Return, 
    Else,
    Break, 
    Continue,
    Struct, 
    Union,
    Public, 
    EndOfFile
}

impl<'b> Token<'b> {
    pub fn as_span(&self) -> Span<'b> {
        match self {
            Token::Paren => Span::new("("), 
            Token::CloseParen => Span::new(")"),
            Token::Brace => Span::new("{"), 
            Token::CloseBrace => Span::new("}"),
            Token::Bracket => Span::new("["), 
            Token::CloseBracket => Span::new("]"),
            Token::Comma => Span::new(","), 
            Token::Dot => Span::new("."),
            Token::Arrow => Span::new("->"), 
            Token::WideArrow => Span::new("=>"),
            Token::Colon => Span::new(":"),
            Token::Equal => Span::new("="), 
            Token::Semicolon => Span::new(";"),
            Token::Reference => Span::new("&"),
            Token::Dereference => Span::new("@"),
            Token::_TypeOwned(_) => unimplemented!(),
            Token::Let => Span::new("let"),
            Token::Fn => Span::new("fn"),
            Token::Unsafe => Span::new("unsafe"),
            Token::TypeVar(span) 
            | Token::Lifetime(span)
            | Token::Identifier(span)
            | Token::Operator(span)
            | Token::Float(span)
            | Token::_Double(span)
            | Token::Char(span)
            | Token::Int(span)
            | Token::HexInt(span)
            | Token::OctInt(span)
            | Token::BinInt(span)
            | Token::String(span) => *span,
            Token::Placeholder => Span::new("_"),
            Token::NewType => Span::new("newtype"),
            Token::Switch => Span::new("switch"),
            Token::Path => Span::new("::"),
            Token::Case => Span::new("case"),
            Token::Class => Span::new("class"),
            Token::Implement => Span::new("implement"),
            Token::Using => Span::new("using"),
            Token::Lambda => Span::new("lambda"),
            Token::Mutable => Span::new("mutable"),
            Token::Module => Span::new("module"),
            Token::True => Span::new("true"),
            Token::False => Span::new("false"),
            Token::_Unit => Span::new("()"),
            Token::If => Span::new("if"), 
            Token::While => Span::new("while"), 
            Token::For => Span::new("for"), 
            Token::Loop => Span::new("loop"),
            Token::Infix => Span::new("infix"), 
            Token::Prefix => Span::new("prefix"), 
            Token::Postfix => Span::new("postfix"),
            Token::Return => Span::new("return"), 
            Token::Else => Span::new("else"),
            Token::Break => Span::new("break"), 
            Token::Continue => Span::new("continue"),
            Token::Struct => Span::new("struct"), 
            Token::Union => Span::new("union"),
            Token::Public => Span::new("public"), 
            Token::EndOfFile => Span::new("")
        }
    }
}

fn match_token<'a>(source: SpanWith<'a>, matched: &'static str, token: Token<'a>) -> IResult<SpanWith<'a>, Token<'a>> {
    let (rest, _) = terminated(tag(matched), peek(not(alphanumeric1)))(source)?;
    Ok((rest, token))
}

fn operator(source: SpanWith) -> IResult<SpanWith, Token> {
    let (rest, op) = operator_string(source)?;
    if op.fragment().eq(&"=") { return Ok((rest, Token::Equal))}
    if op.fragment().eq(&"->") { return Ok((rest, Token::Arrow))}
    if op.fragment().eq(&"=>") { return Ok((rest, Token::WideArrow))}
    
    else {Ok((rest, Token::Operator(to_span(op))))}
}

fn operator_string(source: SpanWith) -> IResult<SpanWith, SpanWith> {
    recognize(many1(one_of("~!$^%*-+=/?><")))(source)
}

pub fn operator_name(source: SpanWith) -> IResult<SpanWith, SpanWith> {
    recognize(delimited(tag("("), operator_string, tag(")")))(source)
}

fn char_or_type(source: SpanWith) -> IResult<SpanWith, Token> {
    let x = recognize(alt((none_of("\\\'"), preceded(tag("\\"), one_of("0'rtn\\")), preceded(tag("x"), preceded(one_of("01234567"), one_of("0123456789abcdefABCDEF"))))))(source);
    match x {
        Ok((rest, y)) => Ok((rest, Token::Char(to_span(y)))),
        Err(e) => {
            let y = recognize(preceded(tag("'"), identifier))(source);

            match y {
                Ok((rest, ty)) => Ok((rest, Token::TypeVar(to_span(ty)))),
                Err(_) => Err(e)
            }
        }
    }
}

fn string(source: SpanWith) -> IResult<SpanWith, Token> {
    let (rest, str) = recognize(delimited(tag("\""), many0(alt((none_of("\\\""), preceded(tag("\\"), one_of("\"\\ntr'0"))))), tag("\"")))(source)?;
    Ok((rest, Token::String(to_span(str))))
}

fn line_comment(source: SpanWith) -> IResult<SpanWith, SpanWith> {
    recognize(preceded(tag("//"), not_line_ending))(source)
}

fn multiline_comment(source: SpanWith) -> IResult<SpanWith, SpanWith> {
    recognize(delimited(tag("/*"), opt(many0(alt((recognize(multiline_comment), recognize(preceded(not(peek(tag("*/"))), anychar)))))), tag("*/")))(source)
}

fn keyword(source: SpanWith) -> IResult<SpanWith, Token> {
    let (rest, c) = anychar(source)?;
    match c
    {
        'b' =>  match_token(rest, "reak", Token::Break),
        'c' =>  {
                    let (rest, c) = anychar(rest)?;
                    match c {
                        'a' => match_token(rest, "se", Token::Case),
                        'l' => match_token(rest, "ass", Token::Class),
                        'o' => match_token(rest, "ntinue", Token::Continue),
                        _ => fail(source)
                    }
                },
        'e' => match_token(rest, "lse", Token::Else),
        'f' => {
                    let (rest, c) = anychar(rest)?;
                    match c {
                        'a' => match_token(rest, "lse", Token::False),
                        'o' => match_token(rest, "r", Token::For),
                        'n' => match_token(rest, "", Token::Fn),
                        _ => fail(source)
                    }
                },
        'i' => {
                    let (rest, c) = anychar(rest)?;
                    match c {
                        'f' => match_token(rest, "", Token::If),
                        'm' => match_token(rest, "plement", Token::Implement),
                        'n' => match_token(rest, "fix", Token::Infix),
                        _ => fail(source)
                    }
                },
        'l' => {
                    let (rest, c) = anychar(rest)?;
                    match c {
                        'o' => match_token(rest, "op", Token::Loop),
                        'e' => match_token(rest, "t", Token::Let),
                        'a' => match_token(rest, "mbda", Token::Lambda),
                        _ => fail(source)
                    }
                },
        'm' => {
                    let (rest, c) = anychar(rest)?;
                    match c {
                        'o' => match_token(rest, "dule", Token::Module),
                        'u' => match_token(rest, "table", Token::Mutable),
                        _ => fail(source)
                    }
                },
        'p' => {
                    let (rest, c) = anychar(rest)?;
                    match c {
                        'r' => match_token(rest, "efix", Token::Prefix),
                        'o' => match_token(rest, "stfix", Token::Postfix),
                        'u' => match_token(rest, "blic", Token::Public),
                        _ => fail(source)
                    }
                },
        'r' => match_token(rest, "eturn", Token::Return),
        's' => {
                    let (rest, c) = anychar(rest)?;
                    match c {
                        't' => match_token(rest, "ruct", Token::Struct),
                        'w' => match_token(rest, "itch", Token::Switch),
                        _ => fail(source)
                    }
                },
        't' => match_token(rest, "ue", Token::True),
        'u' => {
                    let (rest, c) = anychar(rest)?;
                    match c {
                        'n' => {
                            let (rest, c) = anychar(rest)?;
                            match c {
                                'i' => match_token(rest, "on", Token::Union),
                                's' => match_token(rest, "afe", Token::Unsafe),
                                _ => fail(source)
                            }
                        },
                        's' => match_token(rest, "ing", Token::Using),
                        _ => fail(source)
                    }
                },
        '_' => match_token(rest, "", Token::Placeholder),
        _ => fail(source),
    }
}

pub fn skip_whitespace(source: SpanWith) -> IResult<SpanWith, SpanWith> {
    alt((line_comment, multiline_comment, multispace0))(source)
}

fn identifier(source: SpanWith) -> IResult<SpanWith, Token> {
    let x = keyword(source);
    match x {
        Ok(y) => Ok(y),
        Err(_) =>
        {
            let (rest, id) = identifier_string(source)?;
            Ok((rest, Token::Identifier(to_span(id))))
        }
    }
}

pub fn identifier_string(source: SpanWith) -> IResult<SpanWith, SpanWith> {
    recognize(terminated(alt((alpha1, tag("_"))), many0(alt((alphanumeric1, tag("_"))))))(source)
}

fn lifetime(source: SpanWith) -> IResult<SpanWith, Token> {
    let (rest, lifetime) =  recognize(preceded(tag("#"), identifier))(source)?;
    Ok((rest, Token::Lifetime(to_span(lifetime))))
}

fn number(source: SpanWith) -> IResult<SpanWith, Token> {
   alt((float, hex_int, bin_int, oct_int, integer))(source)
}

fn float(source: SpanWith) -> IResult<SpanWith, Token> {
    let(rest, num) = recognize(preceded(opt(tag("-")),tuple((many1(alt((digit1,tag("_")))), tag("."), many1(alt((digit1,tag("_"))))))))(source)?;
    Ok((rest, Token::Float(to_span(num))))
}

pub fn hex_int(source: SpanWith) -> IResult<SpanWith, Token> {
    let (rest, num) = recognize(preceded(opt(tag("-")),pair(alt((tag("0x"), tag("0X"))), many1(alt((hex_digit1,tag("_")))))))(source)?;
    Ok((rest, Token::HexInt(to_span(num))))
}

pub fn bin_int(source: SpanWith) -> IResult<SpanWith, Token> {
    let (rest, num) = recognize(preceded(opt(tag("-")),pair(alt((tag("0b"), tag("0B"))), many1(alt((alt((tag("0"), tag("1"))),tag("_")))))))(source)?;
    Ok((rest, Token::BinInt(to_span(num))))
}

pub fn oct_int(source: SpanWith) -> IResult<SpanWith, Token> {
    let (rest, num) = recognize(preceded(opt(tag("-")),pair(tag("0"), many1(alt((oct_digit1,tag("_")))))))(source)?;
    Ok((rest, Token::OctInt(to_span(num))))
}

pub fn integer(source: SpanWith) -> IResult<SpanWith, Token> {
    let (rest, num) = recognize(preceded(opt(tag("-")),alt((digit1,tag("_")))))(source)?;
    Ok((rest, Token::Int(to_span(num))))
}

fn colon_or_path(source: SpanWith) -> IResult<SpanWith, Token> {
    alt((value(Token::Path, tag("::")), value(Token::Colon, tag(":"))))(source)
}

pub fn scan_token(source: SpanWith) -> IResult<SpanWith, Token> {
    let (rest, _) = skip_whitespace(source)?;
    let end: IResult<SpanWith, SpanWith, nom::error::Error<SpanWith>> = eof(rest);
    if let Ok((rest, _)) = end {return Ok((rest,Token::EndOfFile));};
    let id = identifier(rest);
    if id.is_ok() { return id;}
    let num = number(rest);
    if num.is_ok() { return num;}
    let op = operator(rest);
    if op.is_ok() { return op;}
    let (rem, c) = anychar(rest)?;
    match c {
        '(' => {
            let op_id = operator_name(rest);
            if let Ok((rest, op)) = op_id { Ok((rest, Token::Identifier(to_span(op))))}
            else {Ok((rem, Token::Paren))}
        },
        ')' => Ok((rem, Token::CloseParen)),
        '{' => Ok((rem, Token::Brace)),
        '}' => Ok((rem, Token::CloseBrace)),
        '[' => Ok((rem, Token::Bracket)),
        ']' => Ok((rem, Token::CloseBracket)),
        ';' => Ok((rem, Token::Semicolon)),
        ',' => Ok((rem, Token::Comma)),
        ':' => colon_or_path(rest),
        '.' => Ok((rem, Token::Dot)),
        '@' => Ok((rem, Token::Dereference)),
        '&' => Ok((rem, Token::Reference)),
        '"' => string(rest),
        '\'' => char_or_type(rest),
        '#' => lifetime(rest),
        _ => fail(rest)
    }
}