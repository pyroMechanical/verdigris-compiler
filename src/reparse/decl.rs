use super::data_types::{SyntaxElement, SyntaxNode, SyntaxToken};
use super::expr;
use super::patterns;
use super::types;
use crate::parser::data_types::TokenKind;
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
    Expr(expr::Expr),
    Using(Using),
}

impl Decl {
    pub(crate) fn cast(node: SyntaxNode) -> Option<Self> {
        let result = match node.kind() {
            TokenKind::VariableDecl => Decl::Variable(Variable(node)),
            TokenKind::FunctionDecl => Decl::Function(Function(node)),
            TokenKind::OperatorDecl => Decl::Operator(Operator(node)),
            TokenKind::ClassDecl => Decl::Class(Class(node)),
            TokenKind::ImplementationDecl => Decl::Implementation(Implementation(node)),
            TokenKind::ModuleDecl => Decl::Module(Module(node)),
            TokenKind::UnionDecl => Decl::Union(Union(node)),
            TokenKind::StructDecl => Decl::Struct(Struct(node)),
            TokenKind::ExprDecl => Decl::Expr(expr::Expr::cast(node.children().next()?)?),
            TokenKind::UsingDecl => Decl::Using(Using(node)),
            _ => return None,
        };
        Some(result)
    }
}

pub struct Parameter(SyntaxNode);
impl Parameter {
    pub fn pattern(&self) -> Option<patterns::Pattern> {
        self.0.children().find_map(patterns::Pattern::cast)
    }
    pub fn type_(&self) -> Option<types::Type> {
        self.0.children().find_map(types::Type::cast)
    }
}
pub struct Variable(SyntaxNode);
impl Variable {
    pub fn mutable(&self) -> bool {
        self.0
            .children_with_tokens()
            .any(|x| x.kind() == TokenKind::Mutable)
    }
    pub fn pattern(&self) -> Option<patterns::Pattern> {
        self.0.children().find_map(patterns::Pattern::cast)
    }
    pub fn type_(&self) -> Option<types::Type> {
        self.0.children().find_map(types::Type::cast)
    }
    pub fn value(&self) -> Option<expr::Expr> {
        self.0.children().find_map(expr::Expr::cast)
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
    pub fn arguments(&self) -> impl Iterator<Item = patterns::Pattern> {
        self.0.children().filter_map(patterns::Pattern::cast)
    }
    pub fn type_(&self) -> Option<types::Type> {
        self.0.children().find_map(types::Type::cast)
    }
    pub fn body(&self) -> Option<expr::Expr> {
        self.0.children().find_map(|node| {
            if node.kind() == TokenKind::BlockExpr {
                expr::Expr::cast(node)
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
            .filter_map(|x| x.into_token())
            .find(|x| x.kind() == TokenKind::Identifier)
    }
    pub fn type_(&self) -> Option<types::Type> {
        self.0.children().find_map(types::Type::cast)
    }
}
pub struct Class(SyntaxNode);
impl Class {
    pub fn constraints(&self) -> Option<impl Iterator<Item = Constraint>> {
        self.0
            .children()
            .find(|x| x.kind() == TokenKind::ClassConstraintList)
            .map(|x| {
                x.children()
                    .filter(|x| x.kind() == TokenKind::ClassConstraint)
                    .map(|x| Constraint(x))
            })
    }
    pub fn defined_class(&self) -> Option<SyntaxToken> {
        self.0
            .children_with_tokens()
            .filter_map(|x| x.into_token())
            .find(|x| x.kind() == TokenKind::Identifier)
    }
    pub fn defined_type(&self) -> Option<types::Type> {
        self.0.children().find_map(types::Type::cast)
    }
    pub fn block(&self) -> Option<expr::Expr> {
        self.0.children().find_map(expr::Expr::cast)
    }
}
pub struct Implementation(SyntaxNode);
impl Implementation {
    pub fn constraints(&self) -> Option<impl Iterator<Item = Constraint>> {
        self.0
            .children()
            .find(|x| x.kind() == TokenKind::ClassConstraintList)
            .map(|x| {
                x.children()
                    .filter(|x| x.kind() == TokenKind::ClassConstraint)
                    .map(|x| Constraint(x))
            })
    }
    pub fn defined_class(&self) -> Option<SyntaxToken> {
        self.0
            .children_with_tokens()
            .filter_map(|x| x.into_token())
            .find(|x| x.kind() == TokenKind::Identifier)
    }
    pub fn defined_type(&self) -> Option<types::Type> {
        self.0.children().find_map(types::Type::cast)
    }
    pub fn block(&self) -> Option<expr::Expr> {
        self.0.children().find_map(expr::Expr::cast)
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
    pub fn body(&self) -> Option<expr::Expr> {
        self.0.children().find_map(expr::Expr::cast)
    }
}
pub struct Union(SyntaxNode);
impl Union {
    pub fn defined_type(&self) -> Option<types::Type> {
        self.0.children().find_map(types::Type::cast)
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
    pub fn tuple_type(&self) -> Option<types::Type> {
        self.0.children().find_map(types::Type::cast)
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
    pub fn defined_type(&self) -> Option<types::Type> {
        self.0.children().find_map(types::Type::cast)
    }
    pub fn struct_body(&self) -> Option<StructBody> {
        let tuple = self.tuple_type();
        let params = self.parameters();
        tuple
            .map(StructBody::Tuple)
            .or_else(|| Some(StructBody::Struct(params.collect())))
    }
    fn tuple_type(&self) -> Option<types::Type> {
        self.0
            .children()
            .find(|node| {
                node.kind() == TokenKind::GroupingType || node.kind() == TokenKind::TupleType
            })
            .and_then(types::Type::cast)
    }
    fn parameters(&self) -> impl Iterator<Item = Parameter> {
        self.0
            .children()
            .filter(|node| node.kind() == TokenKind::Parameter)
            .map(Parameter)
    }
}
pub enum StructBody {
    Tuple(types::Type),
    Struct(Vec<Parameter>),
}
pub struct Using(SyntaxNode);
impl Using {
    pub fn path(&self) -> Option<expr::Expr> {
        self.0.children().find_map(expr::Expr::cast)
    }
}
