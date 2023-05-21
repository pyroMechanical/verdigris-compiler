use crate::parser::data_types::{ParsedTree, TokenKind, Verdigris};

pub type SyntaxNode = rowan::SyntaxNode<Verdigris>;
pub type SyntaxToken = rowan::SyntaxToken<Verdigris>;
pub type SyntaxElement = rowan::SyntaxElement<Verdigris>;

pub enum Type {
    //todo: add basic array types, basic function type, etc.
    Function(FunctionType),
    Applied(AppliedType),
    Reference(ReferenceType),
    Pointer(PointerType),
    Array(ArrayType),
    Grouping(GroupingType),
    Tuple(TupleType),
    Lifetime(LifetimeType),
    Var(VarType),
    Basic(BasicType),
    Unit(UnitType),
}

impl Type {
    fn cast(node: SyntaxNode) -> Option<Self> {
        let result = match node.kind() {
            TokenKind::FunctionType => Type::Function(FunctionType(node)),
            TokenKind::AppliedType => Type::Applied(AppliedType(node)),
            TokenKind::ReferenceType => Type::Reference(ReferenceType(node)),
            TokenKind::PointerType => Type::Pointer(PointerType(node)),
            TokenKind::ArrayType => Type::Array(ArrayType(node)),
            TokenKind::GroupingType => Type::Grouping(GroupingType(node)),
            TokenKind::TupleType => Type::Tuple(TupleType(node)),
            TokenKind::LifetimeType => Type::Lifetime(LifetimeType(node)),
            TokenKind::VarType => Type::Var(VarType(node)),
            TokenKind::BasicType => Type::Basic(BasicType(node)),
            TokenKind::UnitType => Type::Unit(UnitType(node)),
            _ => return None,
        };
        Some(result)
    }
}

pub struct FunctionType(SyntaxNode);
impl FunctionType {
    pub fn input_type(&self) -> Option<Type> {
        self.0
            .children_with_tokens()
            .take_while(|x| {
                !matches!(
                    x.kind(),
                    TokenKind::Arrow | TokenKind::MutArrow | TokenKind::OnceArrow
                )
            })
            .filter_map(|x| x.into_node())
            .find_map(Type::cast)
    }

    pub fn arrow_token(&self) -> Option<SyntaxToken> {
        self.0
            .children_with_tokens()
            .filter_map(|x| x.into_token())
            .find_map(|x| {
                if matches!(
                    x.kind(),
                    TokenKind::Arrow | TokenKind::MutArrow | TokenKind::OnceArrow
                ) {
                    Some(x)
                } else {
                    None
                }
            })
    }

    pub fn output_type(&self) -> Option<Type> {
        self.0
            .children_with_tokens()
            .skip_while(|x| {
                !matches!(
                    x.kind(),
                    TokenKind::Arrow | TokenKind::MutArrow | TokenKind::OnceArrow
                )
            })
            .filter_map(|x| x.into_node())
            .find_map(Type::cast)
    }
}
pub struct AppliedType(SyntaxNode);
impl AppliedType {
    pub fn vars(&self) -> impl Iterator<Item = Type> {
        self.0.children().filter_map(Type::cast).skip(1)
    }
    pub fn type_(&self) -> Option<Type> {
        self.0.children().find_map(Type::cast)
    }
}
pub struct ReferenceType(SyntaxNode);
impl ReferenceType {
    pub fn mutable(&self) -> bool {
        self.0
            .children_with_tokens()
            .any(|x| x.kind() == TokenKind::Mutable)
    }
    pub fn type_(&self) -> Option<Type> {
        self.0.children().find_map(Type::cast)
    }
}
pub struct PointerType(SyntaxNode);
impl PointerType {
    pub fn mutable(&self) -> bool {
        self.0
            .children_with_tokens()
            .any(|x| x.kind() == TokenKind::Mutable)
    }
    pub fn type_(&self) -> Option<Type> {
        self.0.children().find_map(Type::cast)
    }
}
pub struct ArrayType(SyntaxNode);
impl ArrayType {
    pub fn type_(&self) -> Option<Type> {
        self.0.children().find_map(Type::cast)
    }
    pub fn size(&self) -> Option<SyntaxToken> {
        self.0
            .children_with_tokens()
            .filter_map(|x| x.into_token())
            .find_map(|x| {
                if matches!(
                    x.kind(),
                    TokenKind::Int | TokenKind::BinInt | TokenKind::OctInt | TokenKind::HexInt
                ) {
                    Some(x)
                } else {
                    None
                }
            })
    }
}
pub struct GroupingType(SyntaxNode);
impl GroupingType {
    pub fn type_(&self) -> Option<Type> {
        self.0.children().find_map(Type::cast)
    }
}
pub struct TupleType(SyntaxNode);
impl TupleType {
    pub fn types(&self) -> impl Iterator<Item = Type> {
        self.0.children().filter_map(Type::cast)
    }
}
pub struct LifetimeType(SyntaxNode);
impl LifetimeType {
    pub fn id(&self) -> Option<SyntaxToken> {
        self.0
            .children_with_tokens()
            .filter_map(|x| x.into_token())
            .find(|x| x.kind() == TokenKind::Identifier)
    }
}
pub struct VarType(SyntaxNode);
impl VarType {
    pub fn id(&self) -> Option<SyntaxToken> {
        self.0
            .children_with_tokens()
            .filter_map(|x| x.into_token())
            .find(|x| x.kind() == TokenKind::Identifier)
    }
}
pub struct BasicType(SyntaxNode);
impl BasicType {
    pub fn id(&self) -> Option<SyntaxToken> {
        self.0
            .children_with_tokens()
            .filter_map(|x| x.into_token())
            .find(|x| x.kind() == TokenKind::Identifier)
    }
}
pub struct UnitType(SyntaxNode);

pub enum Decl {
    Variable(Variable),
    Function(Function),
    Operator(Operator),
    Class(Class),
    Implementation(Implementation),
    Module(Module),
    Union(Union),
    Struct(Struct),
    //NewType(NewType),
    Expr(Expr),
    Using(Using),
}

impl Decl {
    fn cast(node: SyntaxNode) -> Option<Self> {
        let result = match node.kind() {
            TokenKind::VariableDecl => Decl::Variable(Variable(node)),
            TokenKind::FunctionDecl => Decl::Function(Function(node)),
            TokenKind::OperatorDecl => Decl::Operator(Operator(node)),
            TokenKind::ClassDecl => Decl::Class(Class(node)),
            TokenKind::ImplementationDecl => Decl::Implementation(Implementation(node)),
            TokenKind::ModuleDecl => Decl::Module(Module(node)),
            TokenKind::UnionDecl => Decl::Union(Union(node)),
            TokenKind::StructDecl => Decl::Struct(Struct(node)),
            TokenKind::ExprDecl => {
                Decl::Expr(Expr::cast(node.children().next()?)?)
            },
            TokenKind::UsingDecl => Decl::Using(Using(node)),
            _ => return None,
        };
        Some(result)
    }
}

pub struct Parameter(SyntaxNode);
impl Parameter {
    pub fn pattern(&self) -> Option<Pattern> {
        self.0.children().find_map(Pattern::cast)
    }
    pub fn type_(&self) -> Option<Type> {
        self.0.children().find_map(Type::cast)
    }
}
pub struct Variable(SyntaxNode);
impl Variable {
    pub fn mutable(&self) -> bool {
        self.0
            .children_with_tokens()
            .any(|x| x.kind() == TokenKind::Mutable)
    }
    pub fn pattern(&self) -> Option<Pattern> {
        self.0.children().find_map(Pattern::cast)
    }
    pub fn type_(&self) -> Option<Type> {
        self.0.children().find_map(Type::cast)
    }
    pub fn value(&self) -> Option<Expr> {
        self.0.children().find_map(Expr::cast)
    }
}
pub struct Function(SyntaxNode);
impl Function {
    pub fn name(&self) -> Option<SyntaxToken> {
        self.0
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .find(|node| node.kind() == TokenKind::Identifier || node.kind() == TokenKind::Operator)
    }
    pub fn arguments(&self) -> impl Iterator<Item = Pattern> {
        self.0.children().filter_map(Pattern::cast)
    }
    pub fn type_(&self) -> Option<Type> {
        self.0.children().find_map(Type::cast)
    }
    pub fn body(&self) -> Option<Expr> {
        self.0.children().find_map(|node| {
            if node.kind() == TokenKind::BlockExpr {
                Expr::cast(node)
            } else {
                None
            }
        })
    }
}
pub struct Operator(SyntaxNode);
impl Operator {
    pub fn fixity(&self) -> Option<Fixity> {
        self.0.children_with_tokens().find_map(Fixity::cast)
    }
    pub fn operator(&self) -> Option<SyntaxToken> {
        self.0
            .children_with_tokens()
            .filter_map(|x| x.into_token())
            .find(|x| x.kind() == TokenKind::Operator)
    }
    pub fn precedence(&self) -> Option<SyntaxToken> {
        self.0
            .children_with_tokens()
            .filter_map(|x| x.into_token())
            .find(|x| {
                matches!(
                    x.kind(),
                    TokenKind::Int | TokenKind::HexInt | TokenKind::OctInt | TokenKind::BinInt
                )
            })
    }
}
#[derive(PartialEq, Eq, Hash, Debug, Clone)]
pub enum Fixity {
    Prefix,
    Infixl,
    Infixr,
}
impl Fixity {
    fn cast(node: SyntaxElement) -> Option<Fixity> {
        let result = match node.kind() {
            TokenKind::Prefix => Fixity::Prefix,
            TokenKind::Infixl => Fixity::Infixl,
            TokenKind::Infixr => Fixity::Infixr,
            _ => return None,
        };
        Some(result)
    }
}

pub struct Constraint(SyntaxNode);
impl Constraint {
    pub fn class(&self) -> Option<SyntaxToken> {
        self.0
        .children_with_tokens()
        .filter_map(|x| x.into_token()).find(|x| x.kind() == TokenKind::Identifier)
    }
    pub fn type_(&self) -> Option<Type> {
        self.0
        .children()
        .find_map(Type::cast)
    }
}
pub struct Class(SyntaxNode);
impl Class {
    pub fn constraints(&self) -> Option<impl Iterator<Item = Constraint>> {
        self.0
            .children()
            .find(|x| x.kind() == TokenKind::ClassConstraintList)
            .map(|x| x.children().filter(|x| x.kind() == TokenKind::ClassConstraint).map(|x| Constraint(x)))
    }
    pub fn defined_class(&self) -> Option<SyntaxToken> {
        self.0.children_with_tokens().filter_map(|x| x.into_token()).find(|x| x.kind() == TokenKind::Identifier)
    }
    pub fn defined_type(&self) -> Option<Type> {
        self.0.children().find_map(Type::cast)
    }
    pub fn block(&self) -> Option<Expr> {
        self.0.children().find_map(Expr::cast)
    }
}
pub struct Implementation(SyntaxNode);
impl Implementation {
    pub fn constraints(&self) -> Option<impl Iterator<Item = Constraint>> {
        self.0
            .children()
            .find(|x| x.kind() == TokenKind::ClassConstraintList)
            .map(|x| x.children().filter(|x| x.kind() == TokenKind::ClassConstraint).map(|x| Constraint(x)))
    }
    pub fn defined_class(&self) -> Option<SyntaxToken> {
        self.0.children_with_tokens().filter_map(|x| x.into_token()).find(|x| x.kind() == TokenKind::Identifier)
    }
    pub fn defined_type(&self) -> Option<Type> {
        self.0.children().find_map(Type::cast)
    }
    pub fn block(&self) -> Option<Expr> {
        self.0.children().find_map(Expr::cast)
    }
}
pub struct Module(SyntaxNode);
impl Module {
    pub fn name(&self) -> Option<SyntaxToken> {
        self.0
            .children_with_tokens()
            .filter_map(|x| x.into_token())
            .find(|x| x.kind() == TokenKind::Identifier)
    }
    pub fn body(&self) -> Option<Expr> {
        self.0.children().find_map(Expr::cast)
    }
}
pub struct Union(SyntaxNode);
impl Union {
    pub fn defined_type(&self) -> Option<Type> {
        self.0.children().find_map(Type::cast)
    }
    pub fn variants(&self) -> impl Iterator<Item = Variant> {
        self.0.children().filter_map(Variant::cast)
    }
}
pub enum Variant {
    Plain(PlainVariant),
    Tuple(TupleVariant),
    Struct(StructVariant),
}
impl Variant {
    fn cast(node: SyntaxNode) -> Option<Self> {
        let result = match node.kind() {
            TokenKind::PlainVariant => Self::Plain(PlainVariant(node)),
            TokenKind::TupleVariant => Self::Tuple(TupleVariant(node)),
            TokenKind::StructVariant => Self::Struct(StructVariant(node)),
            _ => return None,
        };
        Some(result)
    }
}
pub struct PlainVariant(SyntaxNode);
impl PlainVariant {
    pub fn identifier(&self) -> Option<SyntaxToken> {
        self.0
            .children_with_tokens()
            .filter_map(|x| x.into_token())
            .find(|x| x.kind() == TokenKind::Identifier)
    }
}
pub struct TupleVariant(SyntaxNode);
impl TupleVariant {
    pub fn identifier(&self) -> Option<SyntaxToken> {
        self.0
            .children_with_tokens()
            .filter_map(|x| x.into_token())
            .find(|x| x.kind() == TokenKind::Identifier)
    }
    pub fn tuple_type(&self) -> Option<Type> {
        self.0.children().find_map(Type::cast)
    }
}
pub struct StructVariant(SyntaxNode);
impl StructVariant {
    pub fn identifier(&self) -> Option<SyntaxToken> {
        self.0
            .children_with_tokens()
            .filter_map(|x| x.into_token())
            .find(|x| x.kind() == TokenKind::Identifier)
    }
    pub fn parameters(&self) -> impl Iterator<Item = Parameter> {
        self.0
            .children()
            .filter(|x| x.kind() == TokenKind::Parameter)
            .map(Parameter)
    }
}
pub struct Struct(SyntaxNode);
impl Struct {
    pub fn defined_type(&self) -> Option<Type> {
        self.0.children().find_map(Type::cast)
    }
    pub fn struct_body(&self) -> Option<StructBody> {
        let tuple = self.tuple_type();
        let params = self.parameters();
        tuple
            .map(StructBody::Tuple)
            .or_else(|| Some(StructBody::Struct(params.collect())))
    }
    fn tuple_type(&self) -> Option<Type> {
        self.0
            .children()
            .find(|node| {
                node.kind() == TokenKind::GroupingType || node.kind() == TokenKind::TupleType
            })
            .and_then(Type::cast)
    }
    fn parameters(&self) -> impl Iterator<Item = Parameter> {
        self.0
            .children()
            .filter(|node| node.kind() == TokenKind::Parameter)
            .map(Parameter)
    }
}
pub enum StructBody {
    Tuple(Type),
    Struct(Vec<Parameter>),
}
pub struct Using(SyntaxNode);
impl Using {
    pub fn path(&self) -> Option<Expr> {
        self.0.children().find_map(Expr::cast)
    }
}
pub enum Pattern {
    Unit(UnitPattern),
    Grouping(GroupingPattern),
    Identifier(IdentifierPattern),
    Literal(LiteralPattern),
    Tuple(TuplePattern),
    Placeholder(PlaceholderPattern),
    Array(ArrayPattern),
    TupleStruct(TupleStructPattern),
    Struct(StructPattern),
}

impl Pattern {
    pub fn cast(node: SyntaxNode) -> Option<Pattern> {
        let result = match node.kind() {
            TokenKind::UnitPattern => Pattern::Unit(UnitPattern(node)),
            TokenKind::GroupingPattern => Pattern::Grouping(GroupingPattern(node)),
            TokenKind::IdentifierPattern => Pattern::Identifier(IdentifierPattern(node)),
            TokenKind::LiteralPattern => Pattern::Literal(LiteralPattern(node)),
            TokenKind::TuplePattern => Pattern::Tuple(TuplePattern(node)),
            TokenKind::PlaceholderPattern => Pattern::Placeholder(PlaceholderPattern(node)),
            TokenKind::ArrayPattern => Pattern::Array(ArrayPattern(node)),
            TokenKind::TupleStructPattern => Pattern::TupleStruct(TupleStructPattern(node)),
            TokenKind::StructPattern => Pattern::Struct(StructPattern(node)),
            _ => return None,
        };
        Some(result)
    }
}

pub struct UnitPattern(SyntaxNode);
pub struct GroupingPattern(SyntaxNode);
impl GroupingPattern {
    pub fn pattern(&self) -> Option<Pattern> {
        self.0.children().find_map(Pattern::cast)
    }
}
pub struct IdentifierPattern(SyntaxNode);
impl IdentifierPattern {
    pub fn id(&self) -> Option<SyntaxToken> {
        self.0
            .children_with_tokens()
            .filter_map(|x| x.into_token())
            .find(|x| x.kind() == TokenKind::Identifier)
    }
}
pub struct LiteralPattern(SyntaxNode);
impl LiteralPattern {
    pub fn token(&self) -> Option<SyntaxToken> {
        self.0
            .children_with_tokens()
            .filter_map(|x| x.into_token())
            .next()
    }
}
pub struct TuplePattern(SyntaxNode);
impl TuplePattern {
    pub fn patterns(&self) -> impl Iterator<Item = Pattern> {
        self.0.children().filter_map(Pattern::cast)
    }
}
pub struct PlaceholderPattern(SyntaxNode);
pub struct ArrayPattern(SyntaxNode);
impl ArrayPattern {
    pub fn patterns(&self) -> impl Iterator<Item = Pattern> {
        self.0.children().filter_map(Pattern::cast)
    }
}
pub struct TupleStructPattern(SyntaxNode);
impl TupleStructPattern {
    pub fn name(&self) -> Option<SyntaxToken> {
        self.0
            .children_with_tokens()
            .filter_map(|x| x.into_token())
            .find(|x| x.kind() == TokenKind::Identifier)
    }
    pub fn tuple(&self) -> Option<Pattern> {
        self.0.children().find_map(Pattern::cast)
    }
}
pub struct StructPattern(SyntaxNode);
impl StructPattern {
    pub fn name(&self) -> Option<SyntaxToken> {
        self.0
            .children_with_tokens()
            .filter_map(|x| x.into_token())
            .find(|x| x.kind() == TokenKind::Identifier)
    }
    pub fn parameters(&self) -> impl Iterator<Item = Pattern> {
        self.0.children().filter_map(Pattern::cast)
    }
}
pub enum Expr {
    If(IfExpr),
    While(WhileExpr),
    Loop(LoopExpr),
    //Switch(SwitchExpr),
    Grouping(GroupingExpr),
    Assignment(AssignmentExpr),
    Literal(LiteralExpr),
    Lambda(LambdaExpr),
    Identifier(IdentifierExpr),
    Placeholder(PlaceholderExpr),
    Tuple(TupleExpr),
    Prefix(PrefixExpr),
    Block(BlockExpr),
    ArrayConstructor(ArrayConstructorExpr),
    Unsafe(UnsafeExpr),
    Unit(UnitExpr),
    Return(ReturnExpr),
    Dereference(DereferenceExpr),
    Reference(ReferenceExpr),
    Binary(BinaryExpr),
    Path(PathExpr),
    FieldCall(FieldCallExpr),
    FunctionCall(FunctionCallExpr),
    ArrayIndex(ArrayIndexExpr),
    StructInit(StructInitExpr),
}

impl Expr {
    fn cast(node: SyntaxNode) -> Option<Self> {
        let result = match node.kind() {
            TokenKind::IfExpr => Expr::If(IfExpr(node)),
            TokenKind::WhileExpr => Expr::While(WhileExpr(node)),
            TokenKind::LoopExpr => Expr::Loop(LoopExpr(node)),
            //TokenKind::SwitchExpr => Expr::Switch(SwitchExpr(node)),
            TokenKind::GroupingExpr => Expr::Grouping(GroupingExpr(node)),
            TokenKind::AssignmentExpr => Expr::Assignment(AssignmentExpr(node)),
            TokenKind::LiteralExpr => Expr::Literal(LiteralExpr(node)),
            TokenKind::LambdaExpr => Expr::Lambda(LambdaExpr(node)),
            TokenKind::IdentifierExpr => Expr::Identifier(IdentifierExpr(node)),
            TokenKind::PlaceholderExpr => Expr::Placeholder(PlaceholderExpr(node)),
            TokenKind::TupleExpr => Expr::Tuple(TupleExpr(node)),
            TokenKind::PrefixExpr => Expr::Prefix(PrefixExpr(node)),
            TokenKind::BlockExpr => Expr::Block(BlockExpr(node)),
            TokenKind::ArrayConstructorExpr => Expr::ArrayConstructor(ArrayConstructorExpr(node)),
            TokenKind::UnsafeExpr => Expr::Unsafe(UnsafeExpr(node)),
            TokenKind::UnitExpr => Expr::Unit(UnitExpr(node)),
            TokenKind::ReturnExpr => Expr::Return(ReturnExpr(node)),
            TokenKind::DereferenceExpr => Expr::Dereference(DereferenceExpr(node)),
            TokenKind::ReferenceExpr => Expr::Reference(ReferenceExpr(node)),
            TokenKind::BinaryExpr => Expr::Binary(BinaryExpr(node)),
            TokenKind::PathExpr => Expr::Path(PathExpr(node)),
            TokenKind::FieldCallExpr => Expr::FieldCall(FieldCallExpr(node)),
            TokenKind::FunctionCallExpr => Expr::FunctionCall(FunctionCallExpr(node)),
            TokenKind::ArrayIndexExpr => Expr::ArrayIndex(ArrayIndexExpr(node)),
            TokenKind::StructInitExpr => Expr::StructInit(StructInitExpr(node)),
            _ => return None,
        };
        Some(result)
    }
}
pub struct IfExpr(SyntaxNode);
impl IfExpr {
    pub fn condition(&self) -> Option<Expr> {
        self.0
            .children()
            .take_while(|x| x.kind() != TokenKind::BlockExpr)
            .find_map(Expr::cast)
    }
    pub fn then_block(&self) -> Option<Expr> {
        self.0
            .children_with_tokens()
            .take_while(|x| x.kind() != TokenKind::Else)
            .filter_map(|x| x.into_node())
            .filter(|x| x.kind() == TokenKind::BlockExpr)
            .find_map(Expr::cast)
    }
    pub fn else_block(&self) -> Option<Expr> {
        self.0
            .children_with_tokens()
            .skip_while(|x| x.kind() != TokenKind::Else)
            .filter_map(|x| x.into_node())
            .filter(|x| x.kind() == TokenKind::BlockExpr)
            .find_map(Expr::cast)
    }
}
pub struct WhileExpr(SyntaxNode);
impl WhileExpr {
    pub fn condition(&self) -> Option<Expr> {
        self.0
            .children()
            .take_while(|x| x.kind() != TokenKind::BlockExpr)
            .find_map(Expr::cast)
    }
    pub fn block(&self) -> Option<Expr> {
        self.0
            .children()
            .filter(|x| x.kind() == TokenKind::BlockExpr)
            .find_map(Expr::cast)
    }
}
pub struct LoopExpr(SyntaxNode);
impl LoopExpr {
    pub fn block(&self) -> Option<Expr> {
        self.0.children().find_map(Expr::cast)
    }
}
pub struct GroupingExpr(SyntaxNode);
impl GroupingExpr {
    pub fn expr(&self) -> Option<Expr> {
        self.0.children().find_map(Expr::cast)
    }
}
pub struct AssignmentExpr(SyntaxNode);
impl AssignmentExpr {
    pub fn lhs(&self) -> Option<Expr> {
        self.0.children().find_map(Expr::cast)
    }
    pub fn rhs(&self) -> Option<Expr> {
        self.0.children().filter_map(Expr::cast).nth(1)
    }
}
pub struct LiteralExpr(SyntaxNode);
impl LiteralExpr {
    pub fn token(&self) -> Option<SyntaxToken> {
        self.0
            .children_with_tokens()
            .filter_map(|x| x.into_token())
            .next()
    }
}
pub struct LambdaExpr(SyntaxNode);
impl LambdaExpr {
    pub fn args(&self) -> impl Iterator<Item = Pattern> {
        self.0.children().filter_map(Pattern::cast)
    }
    pub fn body(&self) -> Option<Expr> {
        self.0.children().find_map(Expr::cast)
    }
}
pub struct IdentifierExpr(SyntaxNode);
impl IdentifierExpr {
    pub fn id(&self) -> Option<SyntaxToken> {
        self.0
            .children_with_tokens()
            .filter_map(|x| x.into_token())
            .find(|x| x.kind() == TokenKind::Identifier)
    }
}
pub struct PlaceholderExpr(SyntaxNode);
pub struct TupleExpr(SyntaxNode);
impl TupleExpr {
    pub fn values(&self) -> impl Iterator<Item = Expr> {
        self.0.children().filter_map(Expr::cast)
    }
}
pub struct PrefixExpr(SyntaxNode);
impl PrefixExpr {
    pub fn operator(&self) -> Option<SyntaxToken> {
        self.0
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .find(|token| token.kind() == TokenKind::Operator)
    }

    pub fn expr(&self) -> Option<Expr> {
        self.0.children().find_map(Expr::cast)
    }
}
pub struct BlockExpr(SyntaxNode);
impl BlockExpr {
    pub fn declarations(&self) -> impl Iterator<Item = Decl> {
        self.0.children().filter_map(Decl::cast)
    }
    pub fn terminator(&self) -> Option<Expr> {
        self.0.children().find_map(Expr::cast)
    }
}
pub struct ArrayConstructorExpr(SyntaxNode);
impl ArrayConstructorExpr {
    pub fn values(&self) -> impl Iterator<Item = Expr> {
        self.0.children().filter_map(Expr::cast)
    }
}
pub struct UnsafeExpr(SyntaxNode);
impl UnsafeExpr {
    pub fn expr(&self) -> Option<Expr> {
        self.0.children().find_map(Expr::cast)
    }
}
pub struct UnitExpr(SyntaxNode);
pub struct ReturnExpr(SyntaxNode);
impl ReturnExpr {
    pub fn expr(&self) -> Option<Expr> {
        self.0.children().find_map(Expr::cast)
    }
}
pub struct DereferenceExpr(SyntaxNode);
impl DereferenceExpr {
    pub fn expr(&self) -> Option<Expr> {
        self.0.children().find_map(Expr::cast)
    }
}
pub struct ReferenceExpr(SyntaxNode);
impl ReferenceExpr {
    pub fn mutable(&self) -> bool {
        self.0
            .children_with_tokens()
            .filter_map(|x| x.into_token())
            .any(|x| x.kind() == TokenKind::Mutable)
    }
    pub fn expr(&self) -> Option<Expr> {
        self.0.children().find_map(Expr::cast)
    }
}
pub struct BinaryExpr(SyntaxNode);
impl BinaryExpr {
    pub fn lhs(&self) -> Option<Expr> {
        self.0.children().find_map(Expr::cast)
    }

    pub fn rhs(&self) -> Option<Expr> {
        self.0.children().filter_map(Expr::cast).nth(1)
    }

    pub fn operator(&self) -> Option<SyntaxToken> {
        self.0
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .find(|token| token.kind() == TokenKind::Operator)
    }
}
pub struct PathExpr(SyntaxNode);
impl PathExpr {
    pub fn lhs(&self) -> Option<Expr> {
        self.0.children().find_map(Expr::cast)
    }
    pub fn rhs(&self) -> Option<Expr> {
        self.0.children().filter_map(Expr::cast).nth(1)
    }
}
pub struct FieldCallExpr(SyntaxNode);
impl FieldCallExpr {
    pub fn lhs(&self) -> Option<Expr> {
        self.0.children().find_map(Expr::cast)
    }
    pub fn field(&self) -> Option<Expr> {
        self.0.children().filter_map(Expr::cast).nth(1)
    }
}
pub struct FunctionCallExpr(SyntaxNode);
impl FunctionCallExpr {
    pub fn lhs(&self) -> Option<Expr> {
        self.0.children().find_map(Expr::cast)
    }
    pub fn args(&self) -> impl Iterator<Item = Expr> {
        self.0.children().filter_map(Expr::cast).skip(1)
    }
}
pub struct ArrayIndexExpr(SyntaxNode);
impl ArrayIndexExpr {
    pub fn lhs(&self) -> Option<Expr> {
        self.0.children().find_map(Expr::cast)
    }
    pub fn index(&self) -> Option<Expr> {
        self.0
            .children_with_tokens()
            .skip_while(|x| x.kind() != TokenKind::Bracket)
            .take_while(|x| x.kind() != TokenKind::CloseBracket)
            .filter_map(|x| x.into_node())
            .filter_map(Expr::cast)
            .nth(1)
    }
}
pub struct StructInitExpr(SyntaxNode);
impl StructInitExpr {
    pub fn lhs(&self) -> Option<Expr> {
        self.0.children().find_map(Expr::cast)
    }
    pub fn values(&self) -> impl Iterator<Item = Expr> {
        self.0
            .children_with_tokens()
            .skip_while(|x| x.kind() != TokenKind::Brace)
            .filter_map(|x| x.into_node())
            .filter_map(Expr::cast)
    }
}
#[derive(Debug)]
pub struct SourceFile(SyntaxNode);
impl SourceFile {
    pub fn decls(&self) -> impl Iterator<Item = Decl> {
        self.0.children().filter_map(Decl::cast)
    }
    pub fn cast(node: SyntaxNode) -> Self {
        match node.kind() {
            TokenKind::SourceFile => Self(node),
            _ => panic!(),
        }
    }
}

pub(crate) fn into_tree(source: &str) -> (SourceFile, Vec<String>) {
    let ParsedTree { tree, errors } = crate::parser::parse(source);
    let ast = SourceFile::cast(SyntaxNode::new_root(tree));
    println!("{:#?}", ast);
    (ast, errors)
}
