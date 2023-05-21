use super::data_types::Token;
use smol_str::SmolStr;

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Pattern {
    Missing,
    Unit,
    Placeholder,
    Identifier(Token),
    Literal(Token),
    Tuple(Vec<Pattern>),
    Array(Vec<Pattern>),
    Struct {
        type_: Token,
        parameters: Vec<Pattern>,
    },
    TupleStruct {
        type_: Token,
        tuple: Box<Pattern>,
    },
}
impl Pattern {
    pub fn lower(pattern: Option<crate::reparse::patterns::Pattern>) -> Self {
        if let Some(pattern) = pattern {
            match pattern {
                crate::reparse::patterns::Pattern::Unit(_) => Pattern::Unit,
                crate::reparse::patterns::Pattern::Placeholder(_) => Pattern::Placeholder,
                crate::reparse::patterns::Pattern::Identifier(identifier) => {
                    Self::Identifier(Token::lower(identifier.id()))
                }
                crate::reparse::patterns::Pattern::Literal(literal) => {
                    Self::Literal(Token::lower(literal.token()))
                }
                crate::reparse::patterns::Pattern::Tuple(tuple) => {
                    Self::Tuple(tuple.patterns().map(|x| Pattern::lower(Some(x))).collect())
                }
                crate::reparse::patterns::Pattern::Array(array) => {
                    Self::Array(array.patterns().map(|x| Pattern::lower(Some(x))).collect())
                }
                crate::reparse::patterns::Pattern::Struct(struct_) => Self::Struct {
                    type_: Token::lower(struct_.name()),
                    parameters: struct_
                        .parameters()
                        .map(|x| Pattern::lower(Some(x)))
                        .collect(),
                },
                crate::reparse::patterns::Pattern::TupleStruct(tuple_struct) => Self::TupleStruct {
                    type_: Token::lower(tuple_struct.name()),
                    tuple: Box::new(Self::lower(tuple_struct.tuple())),
                },
                crate::reparse::patterns::Pattern::Grouping(grouping) => {
                    Self::lower(grouping.pattern())
                }
            }
        } else {
            Self::Missing
        }
    }

    pub fn identifiers(&self) -> Vec<SmolStr> {
        match self {
            Pattern::Missing | Pattern::Unit | Pattern::Placeholder | Pattern::Literal(_) => vec![],
            Pattern::Identifier(token) => vec![token.str.clone()],
            Pattern::Struct {
                type_: _,
                parameters,
            } => parameters.iter().flat_map(|x| x.identifiers()).collect(),
            Pattern::TupleStruct { type_: _, tuple } => tuple.identifiers(),
            Pattern::Tuple(patterns) => patterns.iter().flat_map(|x| x.identifiers()).collect(),
            Pattern::Array(patterns) => patterns.iter().flat_map(|x| x.identifiers()).collect(),
        }
    }
}