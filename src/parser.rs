use rowan::{GreenNode, GreenNodeBuilder};
use logos::Logos;
use std::collections::HashMap;
#[derive(Logos, Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u16)]
enum TokenKind {
  #[regex(r"([ \t\r\n]+|//[^\n]*)", priority = 2)]
  WhiteSpace,
  #[token("/*")]
  OpenComment,
  #[token("*/")]
  CloseComment,
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
  #[regex(r"([~!$^%*\-+=/?><|]+|`[a-zA-Z_][a-zA-Z_0-9]*`)")]
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
  #[error]
  Error,

  //expression nodes
  IfExpr,
  WhileExpr,
  LoopExpr,
  SwitchExpr,
  GroupingExpr,
  AssignmentExpr,
  LiteralExpr,
  LambdaExpr,
  IdentifierExpr,
  PlaceholderExpr,
  TupleExpr,
  PrefixExpr,
  BlockExpr,
  ArrayConstructorExpr,
  UnsafeExpr,
  UnitExpr,
  ReturnExpr,
  DereferenceExpr,
  ReferenceExpr,
  PostfixExpr,
  BinaryExpr,
  PathExpr,
  FieldCallExpr,
  FunctionCallExpr,
  ArrayIndexExpr,
  StructInitExpr,

  //pattern nodes
  UnitPattern, // '()'
  GroupingPattern,
  IdentifierPattern,
  LiteralPattern,
  TuplePattern,
  PlaceholderPattern,
  ArrayPattern,
  TupleStructPattern,
  StructPattern,

  //type nodes
  BasicType,
  BasicArrayType, // "[<number>]"
  BasicTupleType,
  BasicArrowType,
  UnitType,
  GroupingType,
  ReferenceType,
  PointerType,
  VarType,
  GeneratedVarType,
  AppliedType,
  ArrayType,
  TupleType,
  FunctionType,

  //declaration nodes
  VariableDecl,
  FunctionDecl,
  OperatorDecl,
  ClassDecl,
  ImplementationDecl,
  ModuleDecl,
  UnionDecl,
  StructDecl,
  NewTypeDecl,
  ExprDecl,

  ParseError,

  SourceFile
}

impl std::fmt::Display for TokenKind {
  fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
    write!(f, "{:?}", self)
  }
}

impl From<TokenKind> for rowan::SyntaxKind {
  fn from(kind: TokenKind) -> Self {
    Self(kind as u16)
  }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum Verdigris {}
impl rowan::Language for Verdigris {
  type Kind = TokenKind;
  fn kind_from_raw(raw: rowan::SyntaxKind) -> Self::Kind {
    assert!(raw.0 <= Self::Kind::SourceFile as u16);
    unsafe {std::mem::transmute::<u16, TokenKind>(raw.0)}
  }
  fn kind_to_raw(kind: Self::Kind) -> rowan::SyntaxKind {
    kind.into()
  }
}

pub struct ParsedTree {
  pub tree: GreenNode,
  pub errors: Vec<String>
}

impl ParsedTree {
  fn print_tree(&self){
    let syntax_node = rowan::SyntaxNode::<Verdigris>::new_root(self.tree.clone());
    print_syntax_node(0, &syntax_node);
  }
}

fn print_syntax_node(depth: usize, node: &rowan::SyntaxNode<Verdigris>) {
  let kind = node.kind();
  let children = node.children_with_tokens();
  let spacing = "  ".repeat(depth);
  println!("{spacing}{kind}");
  for child in children {
    match child {
      rowan::NodeOrToken::Node(n) => print_syntax_node(depth + 1, &n),
      rowan::NodeOrToken::Token(t) => print_syntax_token(depth + 1, &t)
    }
  }
}

fn print_syntax_token(depth: usize, token: &rowan::SyntaxToken<Verdigris>) {
  let kind = token.kind();
  if kind == TokenKind::WhiteSpace {return;}
  let spacing = "  ".repeat(depth);
  let str = token.text();
  println!("{spacing}{kind} \"{str}\"");
}

type Token<'a> = (TokenKind, &'a str);

struct Parser<'a> {
  pub tokens: Vec<Token<'a>>,
  builder: GreenNodeBuilder<'static>,
  errors: Vec<String>
}

impl<'a> Parser<'a> {
  fn peek(&self) -> Option<TokenKind> {
    match self.tokens.last() {
      None => None,
      Some(&(kind, _)) => Some(kind)
    }
  }
  fn advance(&mut self){
    match self.tokens.last() {
      None => (),
      Some(&(kind, str)) => {
        self.builder.token(kind.into(), str);
        self.tokens.pop();
      }
    }
  }
  fn report_error(&mut self, msg: String) {
    self.errors.push(msg);
  }

  fn matched(&mut self, token_kind: TokenKind) -> bool {
    match self.tokens.last() {
      None => false,
      Some(&(kind, str)) => {
        if token_kind == kind {
          self.advance();
          true
        }
        else {
          false
        }
      }
    }
  }

  fn pos(&mut self) -> usize {
    self.tokens.len()
  }

  fn same_pos(&mut self, last_pos: usize) -> bool {
    last_pos == self.tokens.len()
  }

  fn expect(&mut self, token_kind: TokenKind) {
    match self.tokens.last() {
      None => self.report_error(format!("expected {}", token_kind)),
      Some(&(kind, str)) => {
        if token_kind == kind {
          self.advance();
        }
        else {
          self.report_error(format!("expected {}, found {}", token_kind, kind));
        }
      }
    }
  }
  fn recover(&mut self, recover_to: Vec<TokenKind>) {
    self.builder.start_node(TokenKind::ParseError.into());
    while self.peek() != None {
      let mut recovered = false;
      for token in &recover_to {
        if self.peek() == Some(*token) {recovered = true;}
      }
      if recovered {break;} else {self.advance();}
    }
    self.builder.finish_node();
  }
  fn skip_ws(&mut self) {
    while self.peek() == Some(TokenKind::WhiteSpace) {
      self.advance();
    }
  }
}

#[derive(PartialEq, Eq)]
enum ParseError {
  Eof,
  WrongToken(TokenKind),
  TerminatorToken(TokenKind)
}
type ParseResult = Result<(), ParseError>;

pub fn parse<'a>(source: &'a str) -> ParsedTree {
  let mut lexer = TokenKind::lexer(source);
  let mut tokens = vec![];
  'tokenize: loop {
    match lexer.next() {
      Some(t) => {
        let slice = lexer.slice();
        tokens.push((t, slice));
      },
      None => break 'tokenize
    }
  }
  tokens.reverse();
  let mut parser = Parser{tokens, builder: GreenNodeBuilder::new(), errors: vec![]};
  parser.builder.start_node(TokenKind::SourceFile.into());
  while parser.peek() != None {
    declaration(&mut parser);
    parser.skip_ws();
  }
  if let Some(x) = parser.peek() {
    parser.report_error(format!("unexpected {}", x));
  }
  parser.builder.finish_node();
  let cst = parser.builder.finish();
  let result = ParsedTree{tree: cst, errors: parser.errors};
  result.print_tree();
  for error in &result.errors {
    println!("Error: {}", error);
  }
  result
}

fn single_token(parser: &mut Parser) {
  parser.advance();
}

fn operator_expr(parser: &mut Parser, precedence: usize) {
  assert!(parser.peek() == Some(TokenKind::Operator));
  parser.advance();
  prec_expr(parser, precedence);
}

fn field_call(parser: &mut Parser, precedence: usize) {
  assert!(parser.peek() == Some(TokenKind::Dot));
  parser.advance();
  prec_expr(parser, precedence);
}

fn prefix_expr(parser: &mut Parser) {
  assert!(parser.peek() == Some(TokenKind::Operator));
  parser.advance();
  prec_expr(parser, 2);
}

fn function_call(parser: &mut Parser, precedence: usize) {
  assert!(parser.peek() == Some(TokenKind::Paren));
  parser.advance();
  while parser.peek() != None && parser.peek() != Some(TokenKind::CloseParen){
    parser.skip_ws();
    prec_expr(parser, precedence);
    parser.skip_ws();
    parser.matched(TokenKind::Comma);
  }
    parser.skip_ws();
    parser.expect(TokenKind::CloseParen);
}

fn parenthesized_expr(parser: &mut Parser) {
  assert!(parser.peek() == Some(TokenKind::Paren));
  let checkpoint = parser.builder.checkpoint();
  parser.advance();
  let mut exprs = 0;
  while parser.peek() != None && parser.peek() != Some(TokenKind::CloseParen){
    parser.skip_ws();
    expr(parser);
    exprs += 1;
    parser.skip_ws();
    parser.matched(TokenKind::Comma);
  }
    parser.skip_ws();
    parser.expect(TokenKind::CloseParen);
  if exprs == 0 {
    parser.builder.start_node_at(checkpoint, TokenKind::UnitExpr.into());
  }
  else if exprs == 1 {
    parser.builder.start_node_at(checkpoint, TokenKind::GroupingExpr.into());
  }
  else {
    parser.builder.start_node_at(checkpoint, TokenKind::TupleExpr.into());
  }
  parser.builder.finish_node();
}

fn block_expr(parser: &mut Parser) {
  assert!(parser.peek() == Some(TokenKind::Brace));
  parser.advance();
  while parser.peek() != None && parser.peek() != Some(TokenKind::CloseBrace) {
    parser.skip_ws();
    if let Some(kind) = parser.peek() {
      if expr_type_from_token(kind).is_some()
      {
        let checkpoint = parser.builder.checkpoint();
        if !expr(parser) {
          parser.skip_ws();
          parser.expect(TokenKind::CloseBrace);
          return;
        }
        else {
          if parser.matched(TokenKind::Semicolon) {
            parser.builder.start_node_at(checkpoint, TokenKind::ExprDecl.into());
            parser.builder.finish_node();
          }
        }
      }
      else {
        if !declaration(parser) {
          parser.expect(TokenKind::CloseBrace);
          return;
        }
      }
    }
    else {
      if !declaration(parser) {
        parser.expect(TokenKind::CloseBrace);
        return;
      }
    }
  }
  parser.expect(TokenKind::CloseBrace);
}

fn if_expr(parser: &mut Parser) {
  assert!(parser.peek() == Some(TokenKind::If));
  parser.advance();
  expr(parser); //todo: switch to non-struct-init expressions only
  parser.skip_ws();
  if parser.peek() == Some(TokenKind::Brace) {
    parser.builder.start_node(TokenKind::BlockExpr.into());
    block_expr(parser);
    parser.builder.finish_node();
  }
  else {
    parser.report_error(format!("expected block after expression in if expression!"));
  }
  parser.skip_ws();
  if parser.matched(TokenKind::Else) {
    parser.advance();
    parser.skip_ws();
    if parser.peek() == Some(TokenKind::Brace) {
      parser.builder.start_node(TokenKind::BlockExpr.into());
      block_expr(parser);
      parser.builder.finish_node();
    }
    else {
      parser.report_error(format!("expected block after expression in if expression!"));
    }
  }
}

fn expr_type_from_token(token: TokenKind) -> Option<(fn(&mut Parser) -> (), Option<TokenKind>)> {
  match token {
    TokenKind::Identifier => Some((single_token, Some(TokenKind::IdentifierExpr))),
    TokenKind::Placeholder => Some((single_token, Some(TokenKind::PlaceholderExpr))),
    TokenKind::True
    | TokenKind::False
    | TokenKind::String
    | TokenKind::Char
    | TokenKind::Float
    | TokenKind::Double
    | TokenKind::BinInt
    | TokenKind::OctInt
    | TokenKind::Int
    | TokenKind::HexInt => Some((single_token, Some(TokenKind::LiteralExpr))),
    TokenKind::Operator => Some((prefix_expr, Some(TokenKind::PrefixExpr))),
    TokenKind::Paren => Some((parenthesized_expr, None)),
    TokenKind::Brace => Some((block_expr, Some(TokenKind::BlockExpr))),
    TokenKind::If => Some((if_expr, Some(TokenKind::IfExpr))),
    _ => None
  }
}

fn dereference(parser: &mut Parser, precedence: usize) {
  parser.advance();
}

fn reference(parser: &mut Parser, precedence: usize) {
  parser.advance();
}

fn rhs_type_from_token(token: TokenKind) -> Option<(fn(&mut Parser, usize) -> (), TokenKind, usize)> {
  match token {
    TokenKind::Dot => Some((field_call, TokenKind::FieldCallExpr, 3)),
    TokenKind::Paren => Some((function_call, TokenKind::FunctionCallExpr, 3)),
    TokenKind::Dereference => Some((dereference, TokenKind::DereferenceExpr, 2)),
    TokenKind::Reference => Some((reference, TokenKind::ReferenceExpr, 2)),
    TokenKind::Operator => Some((operator_expr, TokenKind::BinaryExpr, 1)), //binary expr
    _ => None
  }
}

fn let_decl(parser: &mut Parser) {
  assert!(parser.peek() == Some(TokenKind::Let));
  parser.builder.start_node(TokenKind::VariableDecl.into());
  parser.advance();
  parser.skip_ws();
  parser.matched(TokenKind::Mutable);
  parser.skip_ws();
  pattern(parser);
  parser.skip_ws();
  if parser.matched(TokenKind::Colon) {
    parser.advance();
    type_(parser);
  }
  parser.skip_ws();
  if parser.matched(TokenKind::Equal) {
    parser.advance();
    expr(parser);
  }
  parser.expect(TokenKind::Semicolon);
  parser.builder.finish_node();
}

fn declaration(parser: &mut Parser) -> bool {
  let checkpoint = parser.builder.checkpoint();
  let mut result = true;
  parser.skip_ws();
  match parser.peek() {
    None => (),
    Some(kind) => match kind {
      TokenKind::Let => let_decl(parser),
      TokenKind::Fn => todo!("function declaration"),
      TokenKind::Class => todo!("class declaration"),
      TokenKind::Implement => todo!("implementation declaration"),
      TokenKind::Struct => todo!("struct declaration"),
      TokenKind::Union => todo!("union declaration"),
      _ => {
        result = expr(parser);
        parser.skip_ws();
        if result {
          parser.expect(TokenKind::Semicolon);
          parser.builder.start_node_at(checkpoint, TokenKind::ExprDecl.into());
          parser.builder.finish_node();
        }
      },
    }
  };
  result
}

fn type_(parser: &mut Parser) {
  function_type(parser);
}

fn function_type(parser: &mut Parser) {
  let checkpoint = parser.builder.checkpoint();
  parser.skip_ws();
  array_or_pointer_type(parser);
  parser.skip_ws();
  if parser.matched(TokenKind::Arrow) {
    parser.builder.start_node_at(checkpoint, TokenKind::FunctionType.into());
    parser.advance();
    parser.skip_ws();
    function_type(parser);
    parser.builder.finish_node();
  }
}

fn array_or_pointer_type(parser: &mut Parser) {
  let checkpoint = parser.builder.checkpoint();
  parser.skip_ws();
  if parser.matched(TokenKind::Reference) {
    parser.skip_ws();
    parser.matched(TokenKind::Mutable);
    array_or_pointer_type(parser);
    parser.builder.start_node_at(checkpoint, TokenKind::ReferenceType.into());
    parser.builder.finish_node();
  }
  else if parser.matched(TokenKind::Dereference) {
    parser.skip_ws();
    array_or_pointer_type(parser);
    parser.builder.start_node_at(checkpoint, TokenKind::PointerType.into());
    parser.builder.finish_node();
  }
  else if parser.matched(TokenKind::Bracket) {
    parser.skip_ws();
    if parser.matched(TokenKind::CloseBracket) {
      parser.builder.start_node_at(checkpoint, TokenKind::BasicArrayType.into());
      parser.builder.finish_node();
    }
    else if parser.matched(TokenKind::Int) {
      parser.skip_ws();
      parser.expect(TokenKind::CloseBracket);
      parser.builder.start_node_at(checkpoint, TokenKind::BasicArrayType.into());
      parser.builder.finish_node();
    }
    else {
      type_(parser);
      parser.skip_ws();
      if parser.matched(TokenKind::Semicolon) {
        parser.skip_ws();
        parser.expect(TokenKind::Int);
        parser.skip_ws();
      }
      parser.expect(TokenKind::CloseBracket);
      parser.builder.start_node_at(checkpoint, TokenKind::ArrayType.into());
      parser.builder.finish_node();
    }
  }
  else {
    applied_type(parser);
  }
}

fn applied_type(parser: &mut Parser) {
  parser.skip_ws();
  paren_type(parser);
  parser.skip_ws();
  //todo: add support for successive types
}

fn paren_type(parser: &mut Parser) {
  let checkpoint = parser.builder.checkpoint();
  parser.skip_ws();
  if parser.matched(TokenKind::Paren) {
    parser.skip_ws();
    if parser.matched(TokenKind::CloseParen) {
      parser.builder.start_node_at(checkpoint, TokenKind::UnitType.into());
      parser.builder.finish_node();
    }
    else if parser.matched(TokenKind::Comma) {
      parser.skip_ws();
      while parser.peek() != Some(TokenKind::CloseParen) {
        parser.expect(TokenKind::Comma);
        parser.skip_ws();
      }
      parser.expect(TokenKind::CloseParen);
      parser.builder.start_node_at(checkpoint, TokenKind::BasicTupleType.into());
      parser.builder.finish_node();
    }
    else {
      let mut types = 0;
      while parser.peek() != Some(TokenKind::CloseParen) {
        types += 1;
        type_(parser);
        parser.skip_ws();
        if !parser.matched(TokenKind::Comma) {
          break;
        }
      }
      parser.skip_ws();
      parser.expect(TokenKind::CloseParen);
      if types == 1 {
        parser.builder.start_node_at(checkpoint, TokenKind::GroupingType.into());
        parser.builder.finish_node();
      }
      else if types > 1{
        parser.builder.start_node_at(checkpoint, TokenKind::TupleType.into());
        parser.builder.finish_node();
      }
    }
  }
  else {
    basic_type(parser);
  }
}

fn basic_type(parser: &mut Parser) {
  parser.builder.start_node(TokenKind::BasicType.into());
  parser.skip_ws();
  parser.expect(TokenKind::Identifier);
  parser.builder.finish_node();
}

fn parenthesized_pattern(parser: &mut Parser) {
  assert!(parser.peek() == Some(TokenKind::Paren));
  let checkpoint = parser.builder.checkpoint();
  parser.advance();
  let mut patterns = 0;
  while parser.peek() != None && parser.peek() != Some(TokenKind::CloseParen){
    parser.skip_ws();
    pattern(parser);
    patterns += 1;
    parser.skip_ws();
    parser.matched(TokenKind::Comma);
  }
    parser.skip_ws();
    parser.expect(TokenKind::CloseParen);
  if patterns == 0 {
    parser.builder.start_node_at(checkpoint, TokenKind::UnitPattern.into());
  }
  else if patterns == 1 {
    parser.builder.start_node_at(checkpoint, TokenKind::GroupingPattern.into());
  }
  else {
    parser.builder.start_node_at(checkpoint, TokenKind::TuplePattern.into());
  }
  parser.builder.finish_node();
}

fn pattern(parser: &mut Parser) {
  let checkpoint = parser.builder.checkpoint();
  parser.skip_ws();
  match parser.peek() {
    None => (),
    Some(x) => match x {
      TokenKind::Identifier => {
        parser.advance();
        parser.builder.start_node_at(checkpoint, TokenKind::IdentifierPattern.into());
        parser.builder.finish_node();
      }
      TokenKind::Placeholder => {
          parser.advance();
          parser.builder.start_node_at(checkpoint, TokenKind::PlaceholderPattern.into());
          parser.builder.finish_node();
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
        parser.builder.start_node_at(checkpoint, TokenKind::LiteralPattern.into());
        parser.builder.finish_node();
      }
      TokenKind::Paren => parenthesized_pattern(parser),
      x => parser.report_error(format!("expected Pattern, found {}", x))
    }
  }
}

fn expr(parser: &mut Parser) -> bool {
  prec_expr(parser, 0)
}

fn prec_expr(parser: &mut Parser, precedence: usize) -> bool {
  let checkpoint = parser.builder.checkpoint();
  let mut result = false;
  parser.skip_ws();
  match parser.peek() {
    Some(kind) => match expr_type_from_token(kind){
      Some((f, kind)) => {
        let checkpoint = parser.builder.checkpoint();
        f(parser);
        if let Some(kind) = kind {
          parser.builder.start_node_at(checkpoint, kind.into());
          parser.builder.finish_node();
        }
        result = true;
      },
      None => {},
    },
    _ => {}
  }
  parser.skip_ws();
  'rhs: loop {
    match parser.peek() {
      Some(kind) => match rhs_type_from_token(kind){
        Some((f, kind, prec)) => {
          if prec >= precedence {
            f(parser, prec);
            parser.builder.start_node_at(checkpoint, kind.into());
            parser.builder.finish_node();
          }
          else {break 'rhs}
        }
        None => break 'rhs,
      }
      None => break 'rhs,
    }
  };
  result
}