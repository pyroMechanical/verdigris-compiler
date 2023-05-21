use crate::parser::data_types::{TokenKind};
use super::data_types::{SyntaxNode, SyntaxToken};

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