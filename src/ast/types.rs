use super::data_types::Token;
use crate::parser::data_types::TokenKind;
use smol_str::SmolStr;

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum ArrowType {
    Missing,
    Arrow,
    MutArrow,
    OnceArrow,
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Type {
    Missing,
    Unit,
    Function {
        arrow: ArrowType,
        input: Box<Type>,
        output: Box<Type>,
    },
    Applied {
        applied: Box<Type>,
        type_vars: Vec<Type>,
    },
    Reference {
        mutable: bool,
        type_: Box<Type>,
    },
    Pointer {
        mutable: bool,
        type_: Box<Type>,
    },
    Array {
        type_: Box<Type>,
        size: Option<Token>,
    },
    Tuple(Vec<Type>),
    Lifetime(Token),
    Basic(Token),
    Var(Token),
}
impl Type {
    pub fn lower(ty: Option<crate::reparse::types::Type>) -> Self {
        if let Some(ty) = ty {
            match ty {
                crate::reparse::types::Type::Function(function) => Self::Function {
                    arrow: match function.arrow_token() {
                        None => ArrowType::Missing,
                        Some(x) => match x.kind() {
                            TokenKind::Arrow | TokenKind::BasicArrow => ArrowType::Arrow,
                            TokenKind::MutArrow | TokenKind::BasicMutArrow => ArrowType::MutArrow,
                            TokenKind::OnceArrow | TokenKind::BasicOnceArrow => {
                                ArrowType::OnceArrow
                            }
                            _ => ArrowType::Missing,
                        },
                    },
                    input: Box::new(Self::lower(function.input_type())),
                    output: Box::new(Self::lower(function.output_type())),
                },
                crate::reparse::types::Type::Applied(applied) => Self::Applied {
                    applied: Box::new(Self::lower(applied.type_())),
                    type_vars: applied.vars().map(|x| Self::lower(Some(x))).collect(),
                },
                crate::reparse::types::Type::Reference(ref_) => Self::Reference {
                    mutable: ref_.mutable(),
                    type_: Box::new(Self::lower(ref_.type_())),
                },
                crate::reparse::types::Type::Pointer(pointer) => Self::Pointer {
                    mutable: pointer.mutable(),
                    type_: Box::new(Self::lower(pointer.type_())),
                },
                crate::reparse::types::Type::Grouping(grouping) => Self::lower(grouping.type_()),
                crate::reparse::types::Type::Array(array) => Self::Array {
                    type_: Box::new(Self::lower(array.type_())),
                    size: array.size().map(|x| Token::lower(Some(x))),
                },
                crate::reparse::types::Type::Tuple(tuple) => {
                    Self::Tuple(tuple.types().map(|x| Self::lower(Some(x))).collect())
                }
                crate::reparse::types::Type::Lifetime(lifetime) => {
                    Self::Lifetime(Token::lower(lifetime.id()))
                }
                crate::reparse::types::Type::Basic(basic) => Self::Basic(Token::lower(basic.id())),
                crate::reparse::types::Type::Var(var) => Self::Var(Token::lower(var.id())),
                crate::reparse::types::Type::Unit(_) => Self::Unit,
            }
        } else {
            Self::Missing
        }
    }

    pub fn named_type(&self) -> Option<SmolStr> {
        match self {
            Type::Basic(token) => Some(token.str.clone()),
            Type::Applied { applied, .. } => applied.named_type(),
            _ => None,
        }
    }
}