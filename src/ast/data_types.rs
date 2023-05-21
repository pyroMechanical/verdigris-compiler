use crate::parser::data_types::TokenKind;
use crate::reparse::data_types::SyntaxToken;
use smol_str::SmolStr;
use std::collections::HashMap;

index_vec::define_index_type! {pub struct TypeIdx = usize;}
index_vec::define_index_type! {pub struct DeclIdx = usize;}
index_vec::define_index_type! {pub struct ExprIdx = usize;}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct SymbolTable {
    pub(crate) value_names: HashMap<SmolStr, Vec<usize>>,
    pub(crate) value_declarations: Vec<super::decl::Decl>,
    pub(crate) type_names: HashMap<SmolStr, TypeIdx>,
    pub(crate) operator_tokens: HashMap<SmolStr, usize>,
    pub(crate) namespaces: HashMap<SmolStr, SymbolTable>,
    pub(crate) class_implementations: HashMap<SmolStr, super::decl::Decl>,
}
impl SymbolTable {
    pub(crate) fn new() -> Self {
        Self {
            value_names: HashMap::new(),
            value_declarations: vec![],
            type_names: HashMap::new(),
            operator_tokens: HashMap::new(),
            namespaces: HashMap::new(),
            class_implementations: HashMap::new(),
        }
    }
    pub fn find_type(&self, t: &str) -> Option<TypeIdx> {
        self.type_names.get(t).map(|&x| x)
    }
    pub fn find_identifier<'a>(&'a self, id: &str, index: usize) -> Option<&'a super::decl::Decl> {
        match self.value_names.get(id) {
            None => return None,
            Some(ids) => match ids.get(index - 1) {
                None => return None,
                Some(id) => return self.value_declarations.get(*id),
            },
        }
    }
    pub fn find_namespace<'a>(&'a self, id: &str) -> Option<&'a SymbolTable> {
        self.namespaces.get(id)
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct Token {
    pub(crate) kind: TokenKind,
    pub(crate) str: SmolStr,
}
impl Token {
    pub fn lower(token: Option<SyntaxToken>) -> Self {
        match token {
            Some(token) => Self {
                kind: token.kind(),
                str: SmolStr::new(token.text()),
            },
            None => Self {
                kind: TokenKind::ParseError,
                str: SmolStr::new(""),
            },
        }
    }

    pub fn str(&self) -> &str {
        self.str.as_ref()
    }
    pub fn kind(&self) -> TokenKind {
        self.kind
    }
}