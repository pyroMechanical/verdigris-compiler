use logos::Logos;
#[derive(Logos, Clone, Debug, PartialEq)]
pub enum Token {
    #[regex(r"([ \t\r\n]+|//[^\n]*\n)")]
    WhiteSpace,
    #[token("(")]
    Paren, 
    #[token(")")]
    CloseParen,
    #[token("{")]
    Brace, 
    #[token("}")]
    CloseBrace,
    #[token("[")]
    Bracket, 
    #[token("]")]
    CloseBracket,
    #[token(",")]
    Comma, 
    #[token(".")]
    Dot,
    #[token("->")]
    Arrow, 
    #[token("=>")]
    WideArrow,
    #[token(":")]
    Colon,
    #[token("=")]
    Equal, 
    #[token(";")]
    Semicolon,
    #[token("&")]
    Reference,
    #[token("@")]
    Dereference,
    #[regex(r"'[a-zA-Z_][a-zA-Z_0-9]*")]
    TypeVar,
    #[token("let")]
    Let,
    #[regex(r"#[a-zA-Z_][a-zA-Z_0-9]*")]
    Lifetime,
    #[token("fn")]
    Fn,
    #[token("unsafe")]
    Unsafe,
    #[regex(r"([a-zA-Z_][a-zA-Z_0-9]*|\([~!$^%*-+=/?><|]+\))")]
    Identifier,
    #[regex(r"([~!$^%*-+=/?><|]+|`[a-zA-Z_][a-zA-Z_0-9]*`)")]
    Operator,
    #[token("_")]
    Placeholder,
    #[token("newtype")]
    NewType,
    #[token("switch")]
    Switch,
    #[token("::")]
    Path,
    #[token("case")]
    Case,
    #[token("class")]
    Class,
    #[token("implement")]
    Implement,
    #[token("using")]
    Using,
    #[token("lambda")]
    Lambda,
    #[token("mutable")]
    Mutable,
    #[token("module")]
    Module,
    #[token("true")]
    True,
    #[token("false")]
    False,
    #[regex(r"[0-9]?\.[0-9]+f")]
    Float,
    #[regex(r"[0-9]?\.[0-9]+")]
    Double,
    #[regex(r"'(\\|\t|\n|\r|.)'")]
    Char,
    #[regex(r"[0-9]+")]
    Int,
    #[regex(r"0x[0-9a-fA-F]+")]
    HexInt,
    #[regex(r"0[0-7]*")]
    OctInt,
    #[regex(r"0b[0-1]+")]
    BinInt,
    #[regex(r#""(\\"|[^"])*""#)]
    String,
    #[token("if")]
    If, 
    #[token("while")]
    While, 
    #[token("for")]
    For, 
    #[token("loop")]
    Loop,
    #[token("infix")]
    Infix, 
    #[token("prefix")]
    Prefix, 
    #[token("postfix")]
    Postfix,
    #[token("return")]
    Return, 
    #[token("else")]
    Else,
    #[token("break")]
    Break, 
    #[token("continue")]
    Continue,
    #[token("struct")]
    Struct, 
    #[token("union")]
    Union,
    #[token("public")]
    Public,
    EndOfFile,
    #[error]
    Error
}

//fn while_one_of<'a>(compared: &'static str) -> impl Fn(&'a str) -> (&'a str, &'a str) {
//    |source| {
//        let mut split_index = 0;
//        let source_chars = source.char_indices();
//        let op_chars = compared.chars();
//        for (index, char) in source_chars {
//            let mut matched = false;
//            for op_char in op_chars.clone() {
//                if char == op_char {  matched = true; }
//            }
//            if !matched {
//                break;
//            }
//            split_index = index;
//        }
//        source.split_at(split_index)
//    }
//}
//
//fn tag<'a>(tag: &'static str) -> impl Fn(&'a str) -> (Option<&'a str>, &'a str) {
//    move |source| {
//        match source.starts_with(tag) {
//            true =>{ 
//                let (tag, rest) = source.split_at(tag.len());
//                (Some(tag), rest)
//            }
//
//            false => (None, source)
//        }
//    }
//}
//
//fn match_token<'a>(source: &'a str, matched: &'static str, token: Token<'a>) -> IResult<&'a str, Token<'a>> {
//    let (rest, _) = terminated(tag(matched), peek(not(alphanumeric1)))(source)?;
//    Ok((rest, token))
//}
//
//fn operator(source: &str) -> (Option<Token>, &str) {
//    let (op, rest) = operator_string(source);
//    match op {
//        "" => (None, rest),
//        "=" => (Some(Token::Equal), rest),
//        "=>" => (Some(Token::WideArrow), rest),
//        "->" => (Some(Token::Arrow), rest),
//        x => (Some(Token::Operator(x)), rest)
//    }
//}
//
//fn operator_string(source: &str) -> (&str, &str) {
//    while_one_of("~!$^%*-+=/?><")(source)
//}
//
//pub fn operator_name(source: &str) -> (&str, &str) {
//    let (x, rest) = tag("(")(source);
//    if x.is_some()
//    {
//        let (y, rest) = operator_string(rest);
//        if y.is_some(){
//            let (z, rest) = tag(")")(source);
//        }
//    }
//    
//    
//    todo!()
//}
//
//fn char_or_type(source: &str) -> IResult<&str, Token> {
//    let x = recognize(alt((none_of("\\\'"), preceded(tag("\\"), one_of("0'rtn\\")), preceded(tag("x"), preceded(one_of("01234567"), one_of("0123456789abcdefABCDEF"))))))(source);
//    match x {
//        Ok((rest, y)) => Ok((rest, Token::Char(to_span(y)))),
//        Err(e) => {
//            let y = recognize(preceded(tag("'"), identifier))(source);
//
//            match y {
//                Ok((rest, ty)) => Ok((rest, Token::TypeVar(to_span(ty)))),
//                Err(_) => Err(e)
//            }
//        }
//    }
//}
//
//fn string(source: &str) -> IResult<&str, Token> {
//    let (rest, str) = recognize(delimited(tag("\""), many0(alt((none_of("\\\""), preceded(tag("\\"), one_of("\"\\ntr'0"))))), tag("\"")))(source)?;
//    Ok((rest, Token::String(to_span(str))))
//}
//
//fn line_comment(source: &str) -> IResult<&str, &str> {
//    recognize(preceded(tag("//"), not_line_ending))(source)
//}
//
//fn multiline_comment(source: &str) -> IResult<&str, &str> {
//    recognize(delimited(tag("/*"), opt(many0(alt((recognize(multiline_comment), recognize(preceded(not(peek(tag("*/"))), anychar)))))), tag("*/")))(source)
//}
//
//fn keyword(source: &str) -> IResult<&str, Token> {
//    let (rest, c) = anychar(source)?;
//    match c
//    {
//        'b' =>  match_token(rest, "reak", Token::Break),
//        'c' =>  {
//                    let (rest, c) = anychar(rest)?;
//                    match c {
//                        'a' => match_token(rest, "se", Token::Case),
//                        'l' => match_token(rest, "ass", Token::Class),
//                        'o' => match_token(rest, "ntinue", Token::Continue),
//                        _ => fail(source)
//                    }
//                },
//        'e' => match_token(rest, "lse", Token::Else),
//        'f' => {
//                    let (rest, c) = anychar(rest)?;
//                    match c {
//                        'a' => match_token(rest, "lse", Token::False),
//                        'o' => match_token(rest, "r", Token::For),
//                        'n' => match_token(rest, "", Token::Fn),
//                        _ => fail(source)
//                    }
//                },
//        'i' => {
//                    let (rest, c) = anychar(rest)?;
//                    match c {
//                        'f' => match_token(rest, "", Token::If),
//                        'm' => match_token(rest, "plement", Token::Implement),
//                        'n' => match_token(rest, "fix", Token::Infix),
//                        _ => fail(source)
//                    }
//                },
//        'l' => {
//                    let (rest, c) = anychar(rest)?;
//                    match c {
//                        'o' => match_token(rest, "op", Token::Loop),
//                        'e' => match_token(rest, "t", Token::Let),
//                        'a' => match_token(rest, "mbda", Token::Lambda),
//                        _ => fail(source)
//                    }
//                },
//        'm' => {
//                    let (rest, c) = anychar(rest)?;
//                    match c {
//                        'o' => match_token(rest, "dule", Token::Module),
//                        'u' => match_token(rest, "table", Token::Mutable),
//                        _ => fail(source)
//                    }
//                },
//        'p' => {
//                    let (rest, c) = anychar(rest)?;
//                    match c {
//                        'r' => match_token(rest, "efix", Token::Prefix),
//                        'o' => match_token(rest, "stfix", Token::Postfix),
//                        'u' => match_token(rest, "blic", Token::Public),
//                        _ => fail(source)
//                    }
//                },
//        'r' => match_token(rest, "eturn", Token::Return),
//        's' => {
//                    let (rest, c) = anychar(rest)?;
//                    match c {
//                        't' => match_token(rest, "ruct", Token::Struct),
//                        'w' => match_token(rest, "itch", Token::Switch),
//                        _ => fail(source)
//                    }
//                },
//        't' => match_token(rest, "ue", Token::True),
//        'u' => {
//                    let (rest, c) = anychar(rest)?;
//                    match c {
//                        'n' => {
//                            let (rest, c) = anychar(rest)?;
//                            match c {
//                                'i' => match_token(rest, "on", Token::Union),
//                                's' => match_token(rest, "afe", Token::Unsafe),
//                                _ => fail(source)
//                            }
//                        },
//                        's' => match_token(rest, "ing", Token::Using),
//                        _ => fail(source)
//                    }
//                },
//        '_' => match_token(rest, "", Token::Placeholder),
//        _ => fail(source),
//    }
//}
//
//pub fn skip_whitespace(source: &str) -> IResult<&str, &str> {
//    alt((line_comment, multiline_comment, multispace0))(source)
//}
//
//fn identifier(source: &str) -> IResult<&str, Token> {
//    let x = keyword(source);
//    match x {
//        Err(_) =>
//        {
//            let (rest, id) = identifier_string(source)?;
//            Ok((rest, Token::Identifier(to_span(id))))
//        }
//        x => x
//    }
//}
//
//pub fn identifier_string(source: &str) -> IResult<&str, &str> {
//    recognize(terminated(alt((alpha1, tag("_"))), many0(alt((alphanumeric1, tag("_"))))))(source)
//}
//
//fn lifetime(source: &str) -> IResult<&str, Token> {
//    let (rest, lifetime) =  recognize(preceded(tag("#"), identifier))(source)?;
//    Ok((rest, Token::Lifetime(to_span(lifetime))))
//}
//
//fn number(source: &str) -> IResult<&str, Token> {
//   alt((float, hex_int, bin_int, oct_int, integer))(source)
//}
//
//fn float(source: &str) -> IResult<&str, Token> {
//    let(rest, num) = recognize(preceded(opt(tag("-")),tuple((many1(alt((digit1,tag("_")))), tag("."), many1(alt((digit1,tag("_"))))))))(source)?;
//    Ok((rest, Token::Float(to_span(num))))
//}
//
//pub fn hex_int(source: &str) -> IResult<&str, Token> {
//    let (rest, num) = recognize(preceded(opt(tag("-")),pair(alt((tag("0x"), tag("0X"))), many1(alt((hex_digit1,tag("_")))))))(source)?;
//    Ok((rest, Token::HexInt(to_span(num))))
//}
//
//pub fn bin_int(source: &str) -> IResult<&str, Token> {
//    let (rest, num) = recognize(preceded(opt(tag("-")),pair(alt((tag("0b"), tag("0B"))), many1(alt((alt((tag("0"), tag("1"))),tag("_")))))))(source)?;
//    Ok((rest, Token::BinInt(to_span(num))))
//}
//
//pub fn oct_int(source: &str) -> IResult<&str, Token> {
//    let (rest, num) = recognize(preceded(opt(tag("-")),pair(tag("0"), many1(alt((oct_digit1,tag("_")))))))(source)?;
//    Ok((rest, Token::OctInt(to_span(num))))
//}
//
//pub fn integer(source: &str) -> IResult<&str, Token> {
//    let (rest, num) = recognize(preceded(opt(tag("-")),alt((digit1,tag("_")))))(source)?;
//    Ok((rest, Token::Int(to_span(num))))
//}
//
//fn colon_or_path(source: &str) -> IResult<&str, Token> {
//    alt((value(Token::Path, tag("::")), value(Token::Colon, tag(":"))))(source)
//}
//
//pub fn scan_token(source: &str) -> IResult<&str, Token> {
//    let (rest, _) = skip_whitespace(source)?;
//    let end: IResult<&str, &str, nom::error::Error<&str>> = eof(rest);
//    if let Ok((rest, _)) = end {return Ok((rest,Token::EndOfFile));};
//    let id = identifier(rest);
//    if id.is_ok() { return id;}
//    let num = number(rest);
//    if num.is_ok() { return num;}
//    let op = operator(rest);
//    if op.is_ok() { return op;}
//    let (rem, c) = anychar(rest)?;
//    match c {
//        '(' => {
//            let op_id = operator_name(rest);
//            if let Ok((rest, op)) = op_id { Ok((rest, Token::Identifier(to_span(op))))}
//            else {Ok((rem, Token::Paren))}
//        },
//        ')' => Ok((rem, Token::CloseParen)),
//        '{' => Ok((rem, Token::Brace)),
//        '}' => Ok((rem, Token::CloseBrace)),
//        '[' => Ok((rem, Token::Bracket)),
//        ']' => Ok((rem, Token::CloseBracket)),
//        ';' => Ok((rem, Token::Semicolon)),
//        ',' => Ok((rem, Token::Comma)),
//        ':' => colon_or_path(rest),
//        '.' => Ok((rem, Token::Dot)),
//        '@' => Ok((rem, Token::Dereference)),
//        '&' => Ok((rem, Token::Reference)),
//        '"' => string(rest),
//        '\'' => char_or_type(rest),
//        '#' => lifetime(rest),
//        _ => fail(rest)
//    }
//}