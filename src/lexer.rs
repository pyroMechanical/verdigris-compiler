use nom::{
    IResult,
    InputIter,
    InputTake,
    InputLength,
    multi::{many0, many1},
    branch::alt,
    character::complete::{anychar, alpha1, alphanumeric1, multispace0, one_of, none_of, digit1, hex_digit1, oct_digit1, not_line_ending},
    combinator::{fail, recognize, peek, not, opt, eof, value},
    sequence::{delimited, preceded, terminated, tuple, pair}
};
use nom_locate::LocatedSpan;
use nom_supreme::{error::{ErrorTree}, parser_ext::ParserExt, tag::complete::tag};

type Span<'a> = LocatedSpan<&'a str>;

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
            Token::_TypeOwned(string) => unimplemented!(),
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

#[derive(Debug, Clone)]
pub struct Error<'a>{
    source: Span<'a>,
    location: Span<'a>, 
    msg: &'static str
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct SpanWith<'a, 'b, T>(Span<'a>, &'b std::cell::RefCell<T>) where 'a: 'b;

impl<'a, 'b, T> nom::InputTake for SpanWith<'a, 'b, T> {
    fn take(&self, count: usize) -> Self {
        let SpanWith(span, errs) = self;
        SpanWith(span.take(count), errs)
    }
    fn take_split(&self, count: usize) -> (Self, Self) {
        let SpanWith(span, errs) = self;
        let (span1, span2) = span.take_split(count);
        (SpanWith(span1, errs), SpanWith(span2, errs))
    }
}

impl<'a, 'b, T> nom::InputLength for SpanWith<'a, 'b, T> {
    fn input_len(&self) -> usize {
        let SpanWith(span, errs) = self;
        span.input_len()
    }
}

impl<'a, 'b, T> nom::InputIter for SpanWith<'a, 'b, T> {
    type Item = <Span<'a> as nom::InputIter>::Item;
    type Iter = <Span<'a> as nom::InputIter>::Iter;
    type IterElem = <Span<'a> as nom::InputIter>::IterElem;
    fn slice_index(&self, count: usize) -> Result<usize, nom::Needed> {
        let SpanWith(span, errs) = self;
        span.slice_index(count)
    }
    fn position<P>(&self, predicate: P) -> Option<usize>
      where
        P: Fn(Self::Item) -> bool {
        let SpanWith(span, errs) = self;
        span.position(predicate)
    }
    fn iter_elements(&self) -> Self::IterElem {
        let SpanWith(span, errs) = self;
        span.iter_elements()
    }
    fn iter_indices(&self) -> Self::Iter {
        let SpanWith(span, errs) = self;
        span.iter_indices()
    }
}

impl<'a, 'b, T> nom::InputTakeAtPosition for SpanWith<'a, 'b, T> {

    type Item = <Span<'a> as nom::InputTakeAtPosition>::Item;

    fn split_at_position_complete<P, E: nom::error::ParseError<Self>>(
        &self,
        predicate: P,
      ) -> IResult<Self, Self, E>
      where
        P: Fn(Self::Item) -> bool {
        match self.split_at_position(predicate) {
            Err(nom::Err::Incomplete(_)) => Ok(self.take_split(self.input_len())),
            res => res,
        }
    }

    fn split_at_position<P, E: nom::error::ParseError<Self>>(&self, predicate: P) -> IResult<Self, Self, E>
      where
        P: Fn(Self::Item) -> bool,  {
        let SpanWith(span, t) = self;
        match span.position(predicate) {
            Some(n) => Ok(self.take_split(n)),
            None => Err(nom::Err::Incomplete(nom::Needed::new(1))),
        }
    }

    fn split_at_position1<P, E: nom::error::ParseError<Self>>(
        &self,
        predicate: P,
        e: nom::error::ErrorKind,
      ) -> IResult<Self, Self, E>
      where
        P: Fn(Self::Item) -> bool 
    {
        let SpanWith(span, t) = self;
        match span.position(predicate) {
            Some(0) => Err(nom::Err::Error(E::from_error_kind(*self.clone(), e))),
            Some(n) => Ok(self.take_split(n)),
            None => Err(nom::Err::Incomplete(nom::Needed::new(1))),
        }
    }

    fn split_at_position1_complete<P, E: nom::error::ParseError<Self>>(
        &self,
        predicate: P,
        e: nom::error::ErrorKind,
      ) -> IResult<Self, Self, E>
      where
        P: Fn(Self::Item) -> bool 
    {
        let SpanWith(span, t) = self;
        match span.position(predicate) {
            Some(0) => Err(nom::Err::Error(E::from_error_kind(*self.clone(), e))),
            Some(n) => Ok(self.take_split(n)),
            None => if span.input_len() == 0 {
                Err(nom::Err::Error(E::from_error_kind(*self.clone(), e)))
            } else {
                Ok(self.take_split(self.input_len()))
            }
        }
    }
}

impl<'a, 'b, T> nom::Offset for SpanWith<'a, 'b, T> {
    fn offset(&self, second: &Self) -> usize {
        let SpanWith(span, _) = self;
        let SpanWith(span2, _) = second;
        span.offset(span2)
    }
}

impl<'a, 'b, T, R> nom::Slice<R> for SpanWith<'a, 'b, T> 
where &'a str: nom::Slice<R>
{
    fn slice(&self, range: R) -> Self {
        let SpanWith(span, t) = self;
        let span_result = span.slice(range);
        SpanWith(span_result, t)
    }
}

impl<'a, 'b, T, B> nom::Compare<B> for SpanWith<'a, 'b, T> 
where Span<'a>: nom::Compare<B>{
    fn compare(&self, t: B) -> nom::CompareResult {
        let SpanWith(span, _) = self;
        span.compare(t)
    }
    fn compare_no_case(&self, t: B) -> nom::CompareResult {
        let SpanWith(span, _) = self;
        span.compare(t)
    }
}

pub(crate) fn expect<'a, 'b, F, T>(parser: F, error_msg: &'static str) -> impl Fn(SpanWith<'a, 'b, Vec<Error<'a>>>) -> IResult<SpanWith<'a, 'b, Vec<Error<'a>>>, Option<T>> 
where
'a: 'b,
F: Fn(SpanWith<'a, 'b, Vec<Error<'a>>>) -> IResult<SpanWith<'a, 'b, Vec<Error<'a>>>, T>,
{
    move |source| 
    {
        let source_span = source.0.clone();
        match parser(source) {
            Ok((rest, out)) => Ok((rest, Some(out))),
            Err(nom::Err::Error(nom::error::Error{input,code: _})) 
            | Err(nom::Err::Failure(nom::error::Error{input,code: _})) => {
                let err = Error{source: source_span, location: input.0, msg: error_msg};
                let SpanWith(_, errs) = input;
                errs.borrow_mut().push(err);
                Ok((input, None))
            },
            Err(err) => Err(err)
        }
    }
}

fn match_token<'a, 'b>(source: SpanWith<'a, 'b, Vec<Error<'a>>>, matched: &'static str, token: Token<'a>) -> IResult<SpanWith<'a, 'b, Vec<Error<'a>>>, Token<'a>> {
    let (rest, _) = terminated(tag(matched), peek(not(alphanumeric1)))(source)?;
    Ok((rest, token))
}

fn operator<'a, 'b>(source: SpanWith<'a, 'b, Vec<Error<'a>>>) -> IResult<SpanWith<'a, 'b, Vec<Error<'a>>>, Token<'a>> {
    let (rest, SpanWith(op, _)) = operator_string(source)?;
    if op.fragment().eq(&"=") { return Ok((rest, Token::Equal))}
    if op.fragment().eq(&"->") { return Ok((rest, Token::Arrow))}
    if op.fragment().eq(&"=>") { return Ok((rest, Token::WideArrow))}
    
    else {Ok((rest, Token::Operator(op)))}
}

fn operator_string<'a, 'b>(source: SpanWith<'a, 'b, Vec<Error<'a>>>) -> IResult<SpanWith<'a, 'b, Vec<Error<'a>>>, SpanWith<'a, 'b, Vec<Error<'a>>>> {
    recognize(many1(one_of("~!$^%*-+=/?><")))(source)
}

pub fn operator_name<'a, 'b>(source: SpanWith<'a, 'b, Vec<Error<'a>>>) -> IResult<SpanWith<'a, 'b, Vec<Error<'a>>>, SpanWith<'a, 'b, Vec<Error<'a>>>> {
    recognize(delimited(tag("("), operator_string, tag(")")))(source)
}

fn char_or_type<'a, 'b>(source: SpanWith<'a, 'b, Vec<Error<'a>>>) -> IResult<SpanWith<'a, 'b, Vec<Error<'a>>>, Token<'a>> {
    let x = recognize(alt((none_of("\\\'"), preceded(tag("\\"), one_of("0'rtn\\")), preceded(tag("x"), preceded(one_of("01234567"), one_of("0123456789abcdefABCDEF"))))))(source);
    match x {
        Ok((rest, SpanWith(y, _))) => Ok((rest, Token::Char(y))),
        Err(e) => {
            let y = recognize(preceded(tag("'"), identifier))(source);

            match y {
                Ok((rest, SpanWith(ty, _))) => Ok((rest, Token::TypeVar(ty))),
                Err(_) => Err(e)
            }
        }
    }
}

fn string<'a, 'b>(source: SpanWith<'a, 'b, Vec<Error<'a>>>) -> IResult<SpanWith<'a, 'b, Vec<Error<'a>>>, Token<'a>> {
    let (rest, SpanWith(str, _)) = recognize(delimited(tag("\""), many0(alt((none_of("\\\""), preceded(tag("\\"), one_of("\"\\ntr'0"))))), tag("\"")))(source)?;
    Ok((rest, Token::String(str)))
}

fn line_comment<'a, 'b>(source: SpanWith<'a, 'b, Vec<Error<'a>>>) -> IResult<SpanWith<'a, 'b, Vec<Error<'a>>>, SpanWith<'a, 'b, Vec<Error<'a>>>> {
    recognize(preceded(tag("//"), not_line_ending))(source)
}

fn multiline_comment<'a, 'b>(source: SpanWith<'a, 'b, Vec<Error<'a>>>) -> IResult<SpanWith<'a, 'b, Vec<Error<'a>>>, SpanWith<'a, 'b, Vec<Error<'a>>>> {
    recognize(delimited(tag("/*"), opt(many0(alt((recognize(multiline_comment), recognize(preceded(not(peek(tag("*/"))), anychar)))))), tag("*/")))(source)
}

fn keyword<'a, 'b>(source: SpanWith<'a, 'b, Vec<Error<'a>>>) -> IResult<SpanWith<'a, 'b, Vec<Error<'a>>>, Token<'a>> {
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

pub fn skip_whitespace<'a, 'b>(source: SpanWith<'a, 'b, Vec<Error<'a>>>) -> IResult<SpanWith<'a, 'b, Vec<Error<'a>>>, SpanWith<'a, 'b, Vec<Error<'a>>>> {
    alt((line_comment, multiline_comment, multispace0))(source)
}

fn identifier<'a, 'b>(source: SpanWith<'a, 'b, Vec<Error<'a>>>) -> IResult<SpanWith<'a, 'b, Vec<Error<'a>>>, Token<'a>> {
    let x = keyword(source);
    match x {
        Err(_) =>
        {
            let (rest, id) = identifier_string(source)?;
            let SpanWith(span, _) = id;
            Ok((rest, Token::Identifier(span)))
        },
        res => res
    }
}

pub fn identifier_string<'a, 'b>(source: SpanWith<'a, 'b, Vec<Error<'a>>>) -> IResult<SpanWith<'a, 'b, Vec<Error<'a>>>, SpanWith<'a, 'b, Vec<Error<'a>>>> {
    recognize(terminated(alt((alpha1, tag("_"))), many0(alt((alphanumeric1, tag("_"))))))(source)
}

fn lifetime<'a, 'b>(source: SpanWith<'a, 'b, Vec<Error<'a>>>) -> IResult<SpanWith<'a, 'b, Vec<Error<'a>>>, Token<'a>> {
    let (rest, lifetime) =  recognize(preceded(tag("#"), identifier))(source)?;
    let SpanWith(span, _) = lifetime;
    Ok((rest, Token::Lifetime(span)))
}

fn number<'a, 'b>(source: SpanWith<'a, 'b, Vec<Error<'a>>>) -> IResult<SpanWith<'a, 'b, Vec<Error<'a>>>, Token<'a>> {
   alt((float, hex_int, bin_int, oct_int, integer))(source)
}

fn float<'a, 'b>(source: SpanWith<'a, 'b, Vec<Error<'a>>>) -> IResult<SpanWith<'a, 'b, Vec<Error<'a>>>, Token<'a>> {
    let(rest, SpanWith(num, _)) = recognize(preceded(opt(tag("-")),tuple((many1(alt((digit1,tag("_")))), tag("."), many1(alt((digit1,tag("_"))))))))(source)?;
    Ok((rest, Token::Float(num)))
}

pub fn hex_int<'a, 'b>(source: SpanWith<'a, 'b, Vec<Error<'a>>>) -> IResult<SpanWith<'a, 'b, Vec<Error<'a>>>, Token<'a>> {
    let (rest, SpanWith(num, _)) = recognize(preceded(opt(tag("-")),pair(alt((tag("0x"), tag("0X"))), many1(alt((hex_digit1,tag("_")))))))(source)?;
    Ok((rest, Token::HexInt(num)))
}

pub fn bin_int<'a, 'b>(source: SpanWith<'a, 'b, Vec<Error<'a>>>) -> IResult<SpanWith<'a, 'b, Vec<Error<'a>>>, Token<'a>> {
    let (rest, SpanWith(num, _)) = recognize(preceded(opt(tag("-")),pair(alt((tag("0b"), tag("0B"))), many1(alt((alt((tag("0"), tag("1"))),tag("_")))))))(source)?;
    Ok((rest, Token::BinInt(num)))
}

pub fn oct_int<'a, 'b>(source: SpanWith<'a, 'b, Vec<Error<'a>>>) -> IResult<SpanWith<'a, 'b, Vec<Error<'a>>>, Token<'a>> {
    let (rest, SpanWith(num, _)) = recognize(preceded(opt(tag("-")),pair(tag("0"), many1(alt((oct_digit1,tag("_")))))))(source)?;
    Ok((rest, Token::OctInt(num)))
}

pub fn integer<'a, 'b>(source: SpanWith<'a, 'b, Vec<Error<'a>>>) -> IResult<SpanWith<'a, 'b, Vec<Error<'a>>>, Token<'a>> {
    let (rest, SpanWith(num, _)) = recognize(preceded(opt(tag("-")),alt((digit1,tag("_")))))(source)?;
    Ok((rest, Token::Int(num)))
}

fn colon_or_path<'a, 'b>(source: SpanWith<'a, 'b, Vec<Error<'a>>>) -> IResult<SpanWith<'a, 'b, Vec<Error<'a>>>, Token<'a>> {
    alt((value(Token::Path, tag("::")), value(Token::Colon, tag(":"))))(source)
}

pub fn scan_token<'a, 'b>(source: SpanWith<'a, 'b, Vec<Error<'a>>>) -> IResult<SpanWith<'a, 'b, Vec<Error<'a>>>, Token<'a>> {
    let (rest, _) = skip_whitespace(source)?;
    let end: IResult<_, _, nom::error::Error<_>> = eof(rest);
    if let Ok((x, _)) = end {return Ok((x,Token::EndOfFile));};
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
            if op_id.is_ok() {let (rest, SpanWith(op, _)) = op_id.unwrap(); Ok((rest, Token::Identifier(op)))}
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