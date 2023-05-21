pub mod decl;
pub mod expr;
pub mod data_types;
pub mod patterns;
pub mod types;
use crate::parser::data_types::{ParsedTree, TokenKind};

#[derive(Debug)]
pub struct SourceFile(data_types::SyntaxNode);
impl SourceFile {
    pub fn decls(&self) -> impl Iterator<Item = decl::Decl> {
        self.0.children().filter_map(decl::Decl::cast)
    }
    pub fn cast(node: data_types::SyntaxNode) -> Self {
        match node.kind() {
            TokenKind::SourceFile => Self(node),
            _ => panic!(),
        }
    }
}

pub(crate) fn into_tree(source: &str) -> (SourceFile, Vec<String>) {
    let ParsedTree { tree, errors } = crate::parser::parse(source);
    let ast = SourceFile::cast(data_types::SyntaxNode::new_root(tree));
    println!("{:#?}", ast);
    (ast, errors)
}
