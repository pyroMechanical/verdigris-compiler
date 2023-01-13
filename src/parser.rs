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

#[derive(Clone)]
enum Event<'a> {
  StartNode{kind: TokenKind},
  StartNodeAt{kind: TokenKind, checkpoint: usize},
  AddToken{kind: TokenKind, text: &'a str},
  FinishNode,
}

struct Parser<'a, 'b> {
  tokens: &'b [Token<'a>],
  cursor: usize,
  events: Vec<Event<'a>>,
  errors: Vec<String>
}

impl<'a, 'b> Parser<'a, 'b> {
  fn peek_raw(&mut self) -> Option<TokenKind> {
    self.tokens.get(self.cursor).map(|&(kind, _)| kind)
  }
  fn peek(&mut self) -> Option<TokenKind> {
    self.skip_ws();
    self.peek_raw()
  }
  fn start_node(&mut self, kind: TokenKind) {
    self.events.push(Event::StartNode{kind});
  }
  fn checkpoint(&mut self) -> usize {
    self.events.len()
  }
  fn start_node_at(&mut self, checkpoint: usize, kind: TokenKind) {
    self.events.push(Event::StartNodeAt{kind, checkpoint});
  }
  fn finish_node(&mut self) {
    self.events.push(Event::FinishNode);
  }
  fn advance(&mut self){
    self.skip_ws();
    match self.tokens.get(self.cursor) {
      None => (),
      Some(&(kind, text)) => {
        self.cursor += 1;
        self.events.push(Event::AddToken{kind, text: text.into()});
      }
    }
  }
  fn report_error(&mut self, msg: String) {
    self.errors.push(msg);
  }

  fn matched(&mut self, token_kind: TokenKind) -> bool {
    self.skip_ws();
    match self.tokens.get(self.cursor) {
      None => false,
      Some(&(kind, _)) => {
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

  fn expect(&mut self, token_kind: TokenKind) {
    self.skip_ws();
    match self.tokens.get(self.cursor) {
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
  fn skip_ws(&mut self) {
    while self.peek_raw() == Some(TokenKind::WhiteSpace) {
      self.cursor += 1;
    }
  }
}

struct TreeBuilder<'a, 'b> {
  builder: GreenNodeBuilder<'static>,
  tokens: &'b [Token<'a>],
  cursor: usize,
  events: Vec<Event<'a>>
}

impl<'a, 'b> TreeBuilder<'a, 'b> {

  pub fn new(tokens: &'b[Token<'a>], events: Vec<Event<'a>>) -> Self {
    Self {
      builder: GreenNodeBuilder::new(),
      tokens,
      cursor: 0,
      events
    }
  }
  pub fn finish(mut self) -> GreenNode {
    let mut reordered_events = self.events.clone();
    for (index, event) in self.events.iter().enumerate() {
      if let Event::StartNodeAt{kind, checkpoint} = event {
        reordered_events.remove(index);
        reordered_events.insert(*checkpoint, Event::StartNode{kind: *kind})
      }
    }
    for event in reordered_events {
      match event {
        Event::StartNodeAt{..} => unreachable!(),
        Event::StartNode{kind} => {
          self.builder.start_node(kind.into());
        }
        Event::AddToken{kind, text} => {
          self.token(kind.into(), text);
        }
        Event::FinishNode => self.builder.finish_node(),
      }

      self.eat_whitespace();
    };
    self.builder.finish()
  }

  fn eat_whitespace(&mut self) {
    while let Some(&(kind, text)) = self.tokens.get(self.cursor) {
      if kind != TokenKind::WhiteSpace {
        break;
      }
      self.token(kind, text);
    }
  }

  fn token(&mut self, kind: TokenKind, text: &'a str) {
    self.builder.token(kind.into(), text);
    self.cursor += 1;
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
  let mut parser = Parser{tokens: tokens.as_slice(), cursor:0, events: vec![], errors: vec![]};
  parser.start_node(TokenKind::SourceFile.into());
  while parser.peek() != None {
    declaration(&mut parser);
  }
  if let Some(x) = parser.peek() {
    parser.report_error(format!("unexpected {}", x));
  }
  parser.finish_node();
  let builder = TreeBuilder::new(tokens.as_slice(), parser.events);
  let cst = builder.finish();
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
    prec_expr(parser, precedence);
    parser.matched(TokenKind::Comma);
  }
  parser.expect(TokenKind::CloseParen);
}

fn parenthesized_expr(parser: &mut Parser) {
  assert!(parser.peek() == Some(TokenKind::Paren));
  let checkpoint = parser.checkpoint();
  parser.advance();
  let mut exprs = 0;
  while parser.peek() != None && parser.peek() != Some(TokenKind::CloseParen){
    expr(parser);
    exprs += 1;
    parser.matched(TokenKind::Comma);
  }
  parser.expect(TokenKind::CloseParen);
  if exprs == 0 {
    parser.start_node_at(checkpoint, TokenKind::UnitExpr.into());
  }
  else if exprs == 1 {
    parser.start_node_at(checkpoint, TokenKind::GroupingExpr.into());
  }
  else {
    parser.start_node_at(checkpoint, TokenKind::TupleExpr.into());
  }
  parser.finish_node();
}

fn block_expr(parser: &mut Parser) {
  assert!(parser.peek() == Some(TokenKind::Brace));
  parser.advance();
  while parser.peek() != None && parser.peek() != Some(TokenKind::CloseBrace) {
    if let Some(kind) = parser.peek() {
      if expr_type_from_token(kind).is_some()
      {
        let checkpoint = parser.checkpoint();
        if !expr(parser) {
          parser.expect(TokenKind::CloseBrace);
          return;
        }
        else {
          if parser.matched(TokenKind::Semicolon) {
            parser.start_node_at(checkpoint, TokenKind::ExprDecl.into());
            parser.finish_node();
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
    if parser.peek() == Some(TokenKind::Brace) {
    parser.start_node(TokenKind::BlockExpr.into());
    block_expr(parser);
    parser.finish_node();
  }
  else {
    parser.report_error(format!("expected block after expression in if expression!"));
  }
    if parser.matched(TokenKind::Else) {
      parser.advance();
      if parser.peek() == Some(TokenKind::Brace) {
      parser.start_node(TokenKind::BlockExpr.into());
      block_expr(parser);
      parser.finish_node();
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
  parser.start_node(TokenKind::VariableDecl.into());
  parser.advance();
  parser.matched(TokenKind::Mutable);
  pattern(parser);
  if parser.matched(TokenKind::Colon) {
    type_(parser);
  }
    if parser.matched(TokenKind::Equal) {
    expr(parser);
  }
  parser.expect(TokenKind::Semicolon);
  parser.finish_node();
}

fn declaration(parser: &mut Parser) -> bool {
  let checkpoint = parser.checkpoint();
  let mut result = true;
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
        if result {
          parser.expect(TokenKind::Semicolon);
          parser.start_node_at(checkpoint, TokenKind::ExprDecl.into());
          parser.finish_node();
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
  let checkpoint = parser.checkpoint();
  array_or_pointer_type(parser);
  if parser.matched(TokenKind::Arrow) {
    parser.start_node_at(checkpoint, TokenKind::FunctionType.into());
    function_type(parser);
    parser.finish_node();
  }
}

fn array_or_pointer_type(parser: &mut Parser) {
  let checkpoint = parser.checkpoint();
  if parser.matched(TokenKind::Reference) {
    parser.matched(TokenKind::Mutable);
    array_or_pointer_type(parser);
    parser.start_node_at(checkpoint, TokenKind::ReferenceType.into());
    parser.finish_node();
  }
  else if parser.matched(TokenKind::Dereference) {
    array_or_pointer_type(parser);
    parser.start_node_at(checkpoint, TokenKind::PointerType.into());
    parser.finish_node();
  }
  else if parser.matched(TokenKind::Bracket) {
      if parser.matched(TokenKind::CloseBracket) {
      parser.start_node_at(checkpoint, TokenKind::BasicArrayType.into());
      parser.finish_node();
    }
    else if parser.matched(TokenKind::Int) {
      parser.expect(TokenKind::CloseBracket);
      parser.start_node_at(checkpoint, TokenKind::BasicArrayType.into());
      parser.finish_node();
    }
    else {
      type_(parser);
      if parser.matched(TokenKind::Semicolon) {
        parser.expect(TokenKind::Int);
      }
      parser.expect(TokenKind::CloseBracket);
      parser.start_node_at(checkpoint, TokenKind::ArrayType.into());
      parser.finish_node();
    }
  }
  else {
    applied_type(parser);
  }
}

fn applied_type(parser: &mut Parser) {
  paren_type(parser);
  //todo: add support for successive types
}

fn paren_type(parser: &mut Parser) {
  let checkpoint = parser.checkpoint();
  if parser.matched(TokenKind::Paren) {
    if parser.matched(TokenKind::CloseParen) {
      parser.start_node_at(checkpoint, TokenKind::UnitType.into());
      parser.finish_node();
    }
    else if parser.matched(TokenKind::Comma) {
      while parser.peek() != Some(TokenKind::CloseParen) {
        parser.expect(TokenKind::Comma);
      }
      parser.expect(TokenKind::CloseParen);
      parser.start_node_at(checkpoint, TokenKind::BasicTupleType.into());
      parser.finish_node();
    }
    else {
      let mut types = 0;
      while parser.peek() != Some(TokenKind::CloseParen) {
        types += 1;
        type_(parser);
        if !parser.matched(TokenKind::Comma) {
          break;
        }
      }
      parser.expect(TokenKind::CloseParen);
      if types == 1 {
        parser.start_node_at(checkpoint, TokenKind::GroupingType.into());
        parser.finish_node();
      }
      else if types > 1{
        parser.start_node_at(checkpoint, TokenKind::TupleType.into());
        parser.finish_node();
      }
    }
  }
  else {
    basic_type(parser);
  }
}

fn basic_type(parser: &mut Parser) {
  parser.start_node(TokenKind::BasicType.into());
  parser.expect(TokenKind::Identifier);
  parser.finish_node();
}

fn parenthesized_pattern(parser: &mut Parser) {
  assert!(parser.peek() == Some(TokenKind::Paren));
  let checkpoint = parser.checkpoint();
  parser.advance();
  let mut patterns = 0;
  while parser.peek() != None && parser.peek() != Some(TokenKind::CloseParen){
    pattern(parser);
    patterns += 1;
    parser.matched(TokenKind::Comma);
  }
  parser.expect(TokenKind::CloseParen);
  if patterns == 0 {
    parser.start_node_at(checkpoint, TokenKind::UnitPattern.into());
  }
  else if patterns == 1 {
    parser.start_node_at(checkpoint, TokenKind::GroupingPattern.into());
  }
  else {
    parser.start_node_at(checkpoint, TokenKind::TuplePattern.into());
  }
  parser.finish_node();
}

fn pattern(parser: &mut Parser) {
  let checkpoint = parser.checkpoint();
  match parser.peek() {
    None => (),
    Some(x) => match x {
      TokenKind::Identifier => {
        parser.advance();
        parser.start_node_at(checkpoint, TokenKind::IdentifierPattern.into());
        parser.finish_node();
      }
      TokenKind::Placeholder => {
        parser.advance();
        parser.start_node_at(checkpoint, TokenKind::PlaceholderPattern.into());
        parser.finish_node();
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
        parser.start_node_at(checkpoint, TokenKind::LiteralPattern.into());
        parser.finish_node();
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
  let checkpoint = parser.checkpoint();
  let mut result = false;
  match parser.peek() {
    Some(kind) => match expr_type_from_token(kind){
      Some((f, kind)) => {
        let checkpoint = parser.checkpoint();
        f(parser);
        if let Some(kind) = kind {
          parser.start_node_at(checkpoint, kind.into());
          parser.finish_node();
        }
        result = true;
      },
      None => {},
    },
    _ => {}
  }
  'rhs: loop {
    match parser.peek() {
      Some(kind) => match rhs_type_from_token(kind){
        Some((f, kind, prec)) => {
          if prec >= precedence {
            f(parser, prec);
            parser.start_node_at(checkpoint, kind.into());
            parser.finish_node();
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