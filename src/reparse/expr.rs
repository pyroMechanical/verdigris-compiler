use crate::parser::data_types::{TokenKind};
use super::data_types::{SyntaxNode, SyntaxToken, SyntaxElement};
use super::patterns;

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
    pub(crate) fn cast(node: SyntaxNode) -> Option<Self> {
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
    pub fn args(&self) -> impl Iterator<Item = patterns::Pattern> {
        self.0.children().filter_map(patterns::Pattern::cast)
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
    pub fn declarations(&self) -> impl Iterator<Item = super::decl::Decl> {
        self.0.children().filter_map(super::decl::Decl::cast)
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