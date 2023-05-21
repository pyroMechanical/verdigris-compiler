use super::data_types::{SymbolTable, DeclIdx, ExprIdx, TypeIdx, Token};
use super::expr;
use super::patterns;
use crate::parser::data_types::TokenKind;
use index_vec::{IndexVec};
use smol_str::SmolStr;

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct Parameter {
    pattern: patterns::Pattern,
    type_: Option<super::Type>,
}
impl Parameter {
    pub fn lower(param: crate::reparse::decl::Parameter) -> Self {
        Parameter {
            pattern: patterns::Pattern::lower(param.pattern()),
            type_: param.type_().map(|x| super::Type::lower(Some(x))),
        }
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Decl {
    Missing,
    Variable {
        mutable: bool,
        pattern: patterns::Pattern,
        type_: Option<super::Type>,
        value: Option<ExprIdx>,
    },
    Function {
        name: Token,
        args: Vec<patterns::Pattern>,
        type_: Option<super::Type>,
        symbols: SymbolTable,
        body: Option<ExprIdx>,
    },
    Operator {
        fixity: crate::reparse::decl::Fixity,
        op: Token,
        precedence: Option<Token>,
    },
    Class {
        constraints: Vec<(Token, super::Type)>,
        defined_class: Token,
        defined_type: super::Type,
        body: ExprIdx,
    },
    Implementation {
        constraints: Vec<(Token, super::Type)>,
        defined_class: Token,
        defined_type: super::Type,
        body: ExprIdx,
    },
    Module {
        name: Token,
        body: ExprIdx,
    },
    Union {
        defined: super::Type,
        variants: Vec<Variant>,
    },
    Struct {
        defined: super::Type,
        body: StructBody,
    },
    Using {
        path: ExprIdx,
    },
    Expr(ExprIdx),
}
impl Decl {
    pub fn lower(
        decl: Option<crate::reparse::decl::Decl>,
        errors: &mut Vec<String>,
        types: &mut IndexVec<TypeIdx, Decl>,
        declarations: &mut IndexVec<DeclIdx, Decl>,
        expressions: &mut IndexVec<ExprIdx, expr::Expr>,
    ) -> DeclIdx {
        if let Some(decl) = decl {
            match decl {
                crate::reparse::decl::Decl::Variable(var_decl) => {
                    let result =
                        Self::var_lower(var_decl, errors, types, declarations, expressions);
                    declarations.push(result)
                }
                crate::reparse::decl::Decl::Function(fn_decl) => {
                    let result = Self::fn_lower(fn_decl, errors, types, declarations, expressions);
                    declarations.push(result)
                }
                crate::reparse::decl::Decl::Operator(op_decl) => {
                    declarations.push(Self::op_lower(op_decl))
                }
                crate::reparse::decl::Decl::Class(class_decl) => {
                    let result =
                        Self::class_lower(class_decl, errors, types, declarations, expressions);
                    declarations.push(result)
                }
                crate::reparse::decl::Decl::Implementation(impl_decl) => {
                    let result =
                        Self::impl_lower(impl_decl, errors, types, declarations, expressions);
                    declarations.push(result)
                }
                crate::reparse::decl::Decl::Module(mod_decl) => {
                    let result =
                        Self::module_lower(mod_decl, errors, types, declarations, expressions);
                    declarations.push(result)
                }
                crate::reparse::decl::Decl::Union(union_decl) => {
                    declarations.push(Self::union_lower(union_decl))
                }
                crate::reparse::decl::Decl::Struct(struct_decl) => {
                    declarations.push(Self::struct_lower(struct_decl))
                }
                crate::reparse::decl::Decl::Using(using_decl) => {
                    let path =
                        expr::Expr::lower(using_decl.path(), errors, types, declarations, expressions);
                    declarations.push(Self::Using { path })
                }
                crate::reparse::decl::Decl::Expr(expr) => {
                    let result = Decl::Expr(expr::Expr::lower(
                        Some(expr),
                        errors,
                        types,
                        declarations,
                        expressions,
                    ));
                    declarations.push(result)
                }
            }
        } else {
            declarations.push(Self::Missing)
        };
        (declarations.len() - 1).into()
    }

    fn var_lower(
        decl: crate::reparse::decl::Variable,
        errors: &mut Vec<String>,
        types: &mut IndexVec<TypeIdx, Decl>,
        declarations: &mut IndexVec<DeclIdx, Decl>,
        expressions: &mut IndexVec<ExprIdx, expr::Expr>,
    ) -> Self {
        let mutable = decl.mutable();
        let pattern = patterns::Pattern::lower(decl.pattern());
        let type_ = decl.type_().map(|x| super::Type::lower(Some(x)));
        let value = decl
            .value()
            .map(|x| expr::Expr::lower(Some(x), errors, types, declarations, expressions));
        Self::Variable {
            mutable,
            pattern,
            type_,
            value,
        }
    }

    fn fn_lower(
        decl: crate::reparse::decl::Function,
        errors: &mut Vec<String>,
        types: &mut IndexVec<TypeIdx, Decl>,
        declarations: &mut IndexVec<DeclIdx, Decl>,
        expressions: &mut IndexVec<ExprIdx, expr::Expr>,
    ) -> Self {
        let name = Token::lower(decl.name());
        let args: Vec<_> = decl.arguments().map(|x| patterns::Pattern::lower(Some(x))).collect();
        let type_ = decl.type_().map(|x| super::Type::lower(Some(x)));
        let mut symbols = SymbolTable::new();
        for arg in &args {
            let decl = Decl::Variable {
                mutable: false,
                pattern: arg.clone(),
                type_: Some(super::Type::Var(Token {
                    kind: TokenKind::Identifier,
                    str: "".into(),
                })),
                value: None,
            };
            let decl_idx: DeclIdx = declarations.len().into();
            declarations.push(decl);
            let names: Vec<SmolStr> = arg.identifiers();
            for name in names {
                match symbols.value_names.entry(name) {
                    std::collections::hash_map::Entry::Occupied(mut occupied) => {
                        occupied.get_mut().push(symbols.value_declarations.len());
                        symbols.value_declarations.push(
                            declarations
                                .get(decl_idx)
                                .expect("Invalid Declaration Index!")
                                .clone(),
                        );
                    }
                    std::collections::hash_map::Entry::Vacant(vacant) => {
                        vacant.insert(vec![symbols.value_declarations.len()]);
                        symbols.value_declarations.push(
                            declarations
                                .get(decl_idx)
                                .expect("Invalid Declaration Index!")
                                .clone(),
                        );
                    }
                }
            }
        }
        let body = decl
            .body()
            .map(|x| expr::Expr::lower(Some(x), errors, types, declarations, expressions));
        Self::Function {
            name,
            args,
            type_,
            symbols,
            body,
        }
    }

    fn op_lower(decl: crate::reparse::decl::Operator) -> Self {
        let fixity = decl.fixity().unwrap();
        let op = Token::lower(decl.operator());
        let precedence = decl.precedence().map(|x| Token::lower(Some(x)));
        Self::Operator {
            fixity,
            op,
            precedence,
        }
    }

    fn class_lower(
        decl: crate::reparse::decl::Class,
        errors: &mut Vec<String>,
        types: &mut IndexVec<TypeIdx, Decl>,
        declarations: &mut IndexVec<DeclIdx, Decl>,
        expressions: &mut IndexVec<ExprIdx, expr::Expr>,
    ) -> Self {
        let constraints = match decl.constraints() {
            None => vec![],
            Some(x) => x
                .map(|x| (Token::lower(x.class()), super::Type::lower(x.type_())))
                .collect(),
        };
        let defined_class = Token::lower(decl.defined_class());
        let defined_type = super::Type::lower(decl.defined_type());
        let body = expr::Expr::lower(decl.block(), errors, types, declarations, expressions);
        Self::Class {
            constraints,
            defined_class,
            defined_type,
            body,
        }
    }

    fn impl_lower(
        decl: crate::reparse::decl::Implementation,
        errors: &mut Vec<String>,
        types: &mut IndexVec<TypeIdx, Decl>,
        declarations: &mut IndexVec<DeclIdx, Decl>,
        expressions: &mut IndexVec<ExprIdx, expr::Expr>,
    ) -> Self {
        let constraints = match decl.constraints() {
            None => vec![],
            Some(x) => x
                .map(|x| (Token::lower(x.class()), super::Type::lower(x.type_())))
                .collect(),
        };
        let defined_class = Token::lower(decl.defined_class());
        let defined_type = super::Type::lower(decl.defined_type());
        let body = expr::Expr::lower(decl.block(), errors, types, declarations, expressions);
        Self::Implementation {
            constraints,
            defined_class,
            defined_type,
            body,
        }
    }

    fn module_lower(
        decl: crate::reparse::decl::Module,
        errors: &mut Vec<String>,
        types: &mut IndexVec<TypeIdx, Decl>,
        declarations: &mut IndexVec<DeclIdx, Decl>,
        expressions: &mut IndexVec<ExprIdx, expr::Expr>,
    ) -> Self {
        let name = Token::lower(decl.name());
        let body = expr::Expr::lower(decl.body(), errors, types, declarations, expressions);
        Self::Module { name, body }
    }

    fn union_lower(decl: crate::reparse::decl::Union) -> Self {
        let defined = super::Type::lower(decl.defined_type());
        let variants = decl.variants().map(|x| Variant::lower(Some(x))).collect();
        Self::Union { defined, variants }
    }

    fn struct_lower(decl: crate::reparse::decl::Struct) -> Self {
        let defined = super::Type::lower(decl.defined_type());
        let body = StructBody::lower(decl.struct_body());
        Self::Struct { defined, body }
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum StructBody {
    Missing,
    TupleStruct(super::Type),
    Struct(Vec<Parameter>),
}
impl StructBody {
    fn lower(body: Option<crate::reparse::decl::StructBody>) -> Self {
        if let Some(body) = body {
            match body {
                crate::reparse::decl::StructBody::Struct(parameters) => {
                    Self::Struct(parameters.into_iter().map(Parameter::lower).collect())
                }
                crate::reparse::decl::StructBody::Tuple(tuple) => {
                    Self::TupleStruct(super::Type::lower(Some(tuple)))
                }
            }
        } else {
            Self::Missing
        }
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Variant {
    Missing,
    SimpleVariant(Token),
    TupleVariant(Token, super::Type),
    StructVariant(Token, Vec<Parameter>),
}
impl Variant {
    pub fn lower(variant: Option<crate::reparse::decl::Variant>) -> Self {
        match variant {
            None => Self::Missing,
            Some(variant) => match variant {
                crate::reparse::decl::Variant::Plain(simple) => {
                    Self::SimpleVariant(Token::lower(simple.identifier()))
                }
                crate::reparse::decl::Variant::Tuple(tuple) => Self::TupleVariant(
                    Token::lower(tuple.identifier()),
                    super::Type::lower(tuple.tuple_type()),
                ),
                crate::reparse::decl::Variant::Struct(struct_) => Self::StructVariant(
                    Token::lower(struct_.identifier()),
                    struct_.parameters().map(Parameter::lower).collect(),
                ),
            },
        }
    }
}