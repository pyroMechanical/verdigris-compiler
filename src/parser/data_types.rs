use logos::Logos;
use rowan::{GreenNode, GreenNodeBuilder};
#[derive(Logos, Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u16)]
pub enum TokenKind {
    #[regex(r"([ \t\r\n]+|//[^\n]*)", priority = 3)]
    WhiteSpace,
    #[token("/*")]
    OpenComment,
    #[token("*/")]
    CloseComment,
    #[token("(", priority = 6)]
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
    #[token("+>")]
    OnceArrow,
    #[token("*>")]
    MutArrow,
    #[token("(->)")]
    BasicArrow,
    #[token("(+>)")]
    BasicOnceArrow,
    #[token("(*>)")]
    BasicMutArrow,
    #[token("=>")]
    WideArrow,
    #[token(":")]
    Colon,
    #[token("=", priority = 4)]
    Equal,
    #[token(";")]
    Semicolon,
    #[token("&")]
    Reference,
    #[token("@")]
    Dereference,
    #[token("'")]
    SingleQuote,
    #[token("let")]
    Let,
    #[token("#")]
    HashSign,
    #[token("fn")]
    Fn,
    #[token("unsafe")]
    Unsafe,
    #[regex(r"([~!$^%*\-+=/?><|]+|`[a-zA-Z_][a-zA-Z_0-9]*`)")]
    Operator,
    #[regex(r"([a-zA-Z_][a-zA-Z_0-9]*|`[~!$^%*\-+=/?><|]+`)")]
    Identifier,
    #[token("_")]
    Placeholder,
    #[token("type")]
    Type,
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
    #[token("infixl")]
    Infixl,
    #[token("infixr")]
    Infixr,
    #[token("prefix")]
    Prefix,
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
    //SwitchExpr,
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
    BasicPointerType,
    BasicReferenceType,
    UnitType,
    GroupingType,
    ReferenceType,
    PointerType,
    VarType,
    LifetimeType,
    AppliedType,
    ArrayType,
    TupleType,
    FunctionType,
    TypeConstraint,

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
    UsingDecl,

    PlainVariant,
    StructVariant,
    TupleVariant,

    Parameter,

    ClassConstraint,
    ClassConstraintList,

    ParseError,

    SourceFile,
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
pub enum Verdigris {}
impl rowan::Language for Verdigris {
    type Kind = TokenKind;
    fn kind_from_raw(raw: rowan::SyntaxKind) -> Self::Kind {
        assert!(raw.0 <= Self::Kind::SourceFile as u16);
        unsafe { std::mem::transmute::<u16, TokenKind>(raw.0) }
    }
    fn kind_to_raw(kind: Self::Kind) -> rowan::SyntaxKind {
        kind.into()
    }
}

pub struct ParsedTree {
    pub tree: GreenNode,
    pub errors: Vec<String>,
}

impl ParsedTree {
    pub fn print_tree(&self) {
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
            rowan::NodeOrToken::Token(t) => print_syntax_token(depth + 1, &t),
        }
    }
}

fn print_syntax_token(depth: usize, token: &rowan::SyntaxToken<Verdigris>) {
    let kind = token.kind();
    if kind == TokenKind::WhiteSpace {
        return;
    }
    let spacing = "  ".repeat(depth);
    let str = token.text();
    println!("{spacing}{kind} \"{str}\"");
}

type Token<'a> = (TokenKind, &'a str);

#[derive(Clone)]
pub enum Event<'a> {
    StartNode { kind: TokenKind },
    StartNodeAt { kind: TokenKind, checkpoint: usize },
    AddToken { kind: TokenKind, text: &'a str },
    FinishNode,
}

pub struct Parser<'a, 'b> {
    tokens: &'b [Token<'a>],
    cursor: usize,
    pub events: Vec<Event<'a>>,
    pub errors: Vec<String>,
}

impl<'a, 'b> Parser<'a, 'b> {
    pub fn from_tokens(tokens: &'b [Token<'a>]) -> Self {
        Self { tokens: tokens, cursor: 0,  events: vec![],errors: vec![] }
    }

    pub fn events(&self) -> Vec<Event<'a>> {
        self.events.clone()
    }

    pub fn into_errors(self) -> Vec<String> {
        self.errors
    }

    pub fn peek_raw(&mut self) -> Option<TokenKind> {
        self.tokens.get(self.cursor).map(|&(kind, _)| kind)
    }
    pub fn peek(&mut self) -> Option<TokenKind> {
        self.skip_ws();
        self.peek_raw()
    }
    pub fn start_node(&mut self, kind: TokenKind) {
        self.events.push(Event::StartNode { kind });
    }
    pub fn checkpoint(&mut self) -> usize {
        self.events.len()
    }
    pub fn start_node_at(&mut self, checkpoint: usize, kind: TokenKind) {
        self.events.push(Event::StartNodeAt { kind, checkpoint });
    }
    pub fn finish_node(&mut self) {
        self.events.push(Event::FinishNode);
    }
    pub fn advance(&mut self) {
        self.skip_ws();
        match self.tokens.get(self.cursor) {
            None => (),
            Some(&(kind, text)) => {
                self.cursor += 1;
                self.events.push(Event::AddToken { kind, text });
            }
        }
    }
    pub fn report_error(&mut self, msg: String) {
        self.errors.push(msg);
    }

    pub fn matched(&mut self, token_kind: TokenKind) -> bool {
        self.skip_ws();
        match self.tokens.get(self.cursor) {
            None => false,
            Some(&(kind, _)) => {
                if token_kind == kind {
                    self.advance();
                    true
                } else {
                    false
                }
            }
        }
    }

    pub fn recover(&mut self) {
        self.start_node(TokenKind::ParseError);
        while self.peek().is_some()
            && self.peek() != Some(TokenKind::Semicolon)
            && self.peek() != Some(TokenKind::CloseBrace)
            && self.peek() != Some(TokenKind::CloseParen)
            && self.peek() != Some(TokenKind::CloseBracket)
        {
            self.advance();
        }
        self.finish_node();
    }

    pub fn expect(&mut self, token_kind: TokenKind) -> bool {
        self.skip_ws();
        match self.tokens.get(self.cursor) {
            None => {
                self.report_error(format!("expected {}", token_kind));
                false
                },
            Some(&(kind, _)) => {
                if token_kind == kind {
                    self.advance();
                    true
                } else {
                    self.report_error(format!("expected {}, found {}", token_kind, kind));
                    false
                }
            }
        }
    }
    pub fn skip_ws(&mut self) {
        while self.peek_raw() == Some(TokenKind::WhiteSpace) {
            self.cursor += 1;
        }
    }
}

pub struct TreeBuilder<'a, 'b> {
    builder: GreenNodeBuilder<'static>,
    tokens: &'b [Token<'a>],
    cursor: usize,
    events: Vec<Event<'a>>,
}

impl<'a, 'b> TreeBuilder<'a, 'b> {
    pub fn new(tokens: &'b [Token<'a>], events: Vec<Event<'a>>) -> Self {
        Self {
            builder: GreenNodeBuilder::new(),
            tokens,
            cursor: 0,
            events,
        }
    }
    pub fn finish(mut self) -> GreenNode {
        let mut reordered_events = self.events.clone();
        for (index, event) in self.events.iter().enumerate() {
            if let Event::StartNodeAt { kind, checkpoint } = event {
                reordered_events.remove(index);
                reordered_events.insert(*checkpoint, Event::StartNode { kind: *kind })
            }
        }
        for event in reordered_events {
            match event {
                Event::StartNodeAt { .. } => unreachable!(),
                Event::StartNode { kind } => {
                    self.builder.start_node(kind.into());
                }
                Event::AddToken { kind, text } => {
                    self.token(kind, text);
                }
                Event::FinishNode => self.builder.finish_node(),
            }

            self.eat_whitespace();
        }
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