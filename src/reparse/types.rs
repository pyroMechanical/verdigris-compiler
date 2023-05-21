use crate::parser::{TokenKind};
use super::data_types::{SyntaxNode, SyntaxToken};
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
    pub(crate) fn cast(node: SyntaxNode) -> Option<Self> {
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
