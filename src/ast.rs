use crate::parser::data_types::TokenKind;
use crate::reparse::data_types::SyntaxToken;
use index_vec::{index_vec, IndexVec};
use smol_str::SmolStr;
use std::collections::HashMap;

index_vec::define_index_type! {pub struct TypeIdx = usize;}
index_vec::define_index_type! {pub struct DeclIdx = usize;}
index_vec::define_index_type! {pub struct ExprIdx = usize;}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct SymbolTable {
    value_names: HashMap<SmolStr, Vec<usize>>,
    pub(crate) value_declarations: Vec<Decl>,
    type_names: HashMap<SmolStr, TypeIdx>,
    operator_tokens: HashMap<SmolStr, usize>,
    namespaces: HashMap<SmolStr, SymbolTable>,
    class_implementations: HashMap<SmolStr, Decl>,
}
impl SymbolTable {
    fn new() -> Self {
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
    pub fn find_identifier<'a>(&'a self, id: &str, index: usize) -> Option<&'a Decl> {
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
pub struct SourceFile {
    pub(crate) declarations: IndexVec<DeclIdx, Decl>,
    pub(crate) top_level_declarations: Vec<DeclIdx>,
    pub(crate) expressions: IndexVec<ExprIdx, Expr>,
    pub(crate) symbol_names: SymbolTable,
    pub(crate) types: IndexVec<TypeIdx, Decl>,
}
impl SourceFile {
    pub fn lower(source: crate::reparse::SourceFile, errors: &mut Vec<String>) -> Self {
        //17 missing spots are for primitive types. in order:
        //0: bool
        //1: i8
        //2: i16
        //3: i32
        //4: i64
        //5: i128
        //6: isize
        //7: u8
        //8: u16
        //9: u32
        //10: u64
        //11: u128
        //12: usize
        //13: f32
        //14: f64
        //15: char
        //16: str
        let mut types = index_vec![Decl::Missing; 17];
        let mut expressions: IndexVec<ExprIdx, Expr> = index_vec![];
        let mut declarations: IndexVec<DeclIdx, Decl> = index_vec![];
        let top_level_declarations = source
            .decls()
            .map(|x| {
                Decl::lower(
                    Some(x),
                    errors,
                    &mut types,
                    &mut declarations,
                    &mut expressions,
                )
            })
            .collect();
        let symbol_names = symbols_from_declarations(
            &top_level_declarations,
            &mut declarations,
            &mut expressions,
            errors,
            &mut types,
        );
        Self {
            declarations,
            top_level_declarations,
            expressions,
            symbol_names,
            types,
        }
    }
}

fn symbols_from_declarations(
    declaration_ids: &Vec<DeclIdx>,
    declarations: &mut IndexVec<DeclIdx, Decl>,
    expressions: &mut IndexVec<ExprIdx, Expr>,
    errors: &mut Vec<String>,
    types: &mut IndexVec<TypeIdx, Decl>,
) -> SymbolTable {
    let mut symbols = SymbolTable::new();
    for declaration in declaration_ids {
        if let Some(decl) = declarations.get_mut(*declaration) {
            match decl {
                Decl::Variable { pattern, value, .. } => {
                    let names = pattern.identifiers();
                    match value {
                        None => (),
                        Some(value) => {
                            identifier_counts(*value, &symbols, declarations, expressions)
                        }
                    };
                    for name in names {
                        match symbols.value_names.entry(name) {
                            std::collections::hash_map::Entry::Occupied(mut occupied) => {
                                occupied.get_mut().push(symbols.value_declarations.len());
                                symbols.value_declarations.push(
                                    declarations
                                        .get(*declaration)
                                        .expect("Invalid Declaration Index!")
                                        .clone(),
                                );
                            }
                            std::collections::hash_map::Entry::Vacant(vacant) => {
                                vacant.insert(vec![symbols.value_declarations.len()]);
                                symbols.value_declarations.push(
                                    declarations
                                        .get(*declaration)
                                        .expect("Invalid Declaration Index!")
                                        .clone(),
                                );
                            }
                        }
                    }
                }
                Decl::Function { name, body, .. } => {
                    let name = name.str.clone();
                    match body {
                        None => (),
                        Some(body) => identifier_counts(*body, &symbols, declarations, expressions),
                    };
                    match symbols.value_names.entry(name.clone()) {
                        std::collections::hash_map::Entry::Occupied(_) => {
                            errors.push(format!("Attempted redefinition of symbol \"{}\"", &name));
                        }
                        std::collections::hash_map::Entry::Vacant(vacant) => {
                            vacant.insert(vec![symbols.value_declarations.len()]);
                            symbols.value_declarations.push(
                                declarations
                                    .get(*declaration)
                                    .expect("Invalid Declaration Index!")
                                    .clone(),
                            );
                        }
                    }
                }
                Decl::Class {
                    defined_class,
                    body,
                    ..
                } => {
                    let name = defined_class.str.clone();
                    identifier_counts(*body, &symbols, declarations, expressions);
                    match symbols.type_names.entry(name.clone()) {
                        std::collections::hash_map::Entry::Occupied(_) => {
                            errors.push(format!("Attempted redefinition of symbol \"{}\"", name));
                        }
                        std::collections::hash_map::Entry::Vacant(vacant) => {
                            vacant.insert(types.len().into());
                            types.push(
                                declarations
                                    .get(*declaration)
                                    .expect("Invalid Declaration Index!")
                                    .clone(),
                            );
                        }
                    }
                }
                Decl::Implementation {
                    defined_class,
                    defined_type,
                    body,
                    ..
                } => {
                    let name = defined_type.named_type();
                    let class = defined_class.str.clone();
                    identifier_counts(*body, &symbols, declarations, expressions);
                    if let Some(name) = name {
                        match symbols.namespaces.entry(name.clone()) {
                            std::collections::hash_map::Entry::Occupied(mut occupied) => {
                                let table = occupied.get_mut();
                                match table.class_implementations.entry(class.clone()) {
                                    std::collections::hash_map::Entry::Occupied(_) => {
                                        errors.push(format!("Attempted redefinition of implementation \"{}\" for type \"{}\"", class, name));
                                    }
                                    std::collections::hash_map::Entry::Vacant(vacant) => {
                                        vacant.insert(
                                            declarations.get(*declaration).unwrap().clone(),
                                        );
                                    }
                                }
                            }
                            std::collections::hash_map::Entry::Vacant(_) => {
                                errors.push(format!("Could not find type \"{}\"", name));
                            }
                        }
                    }
                }
                Decl::Module { name, body } => {
                    let name = name.str.clone();
                    identifier_counts(*body, &symbols, declarations, expressions);
                    match symbols.type_names.entry(name.clone()) {
                        std::collections::hash_map::Entry::Occupied(_) => {
                            errors.push(format!("Attempted redefinition of symbol \"{}\"", name));
                        }
                        std::collections::hash_map::Entry::Vacant(vacant) => {
                            vacant.insert(types.len().into());
                            types.push(
                                declarations
                                    .get(*declaration)
                                    .expect("Invalid Declaration Index!")
                                    .clone(),
                            );
                        }
                    }
                }
                Decl::Operator { op, .. } => match symbols.operator_tokens.entry(op.str.clone()) {
                    std::collections::hash_map::Entry::Occupied(_) => {
                        errors.push(format!(
                            "Attempted redefinition of operator token \"{}\"",
                            op.str
                        ));
                    }
                    std::collections::hash_map::Entry::Vacant(vacant) => {
                        vacant.insert(symbols.value_declarations.len());
                        symbols.value_declarations.push(
                            declarations
                                .get(*declaration)
                                .expect("Invalid Declaration Index!")
                                .clone(),
                        );
                    }
                },
                Decl::Using { .. } => {}
                Decl::Union { defined, .. } => {
                    if let Some(name) = defined.named_type() {
                        match symbols.type_names.entry(name.clone()) {
                            std::collections::hash_map::Entry::Occupied(_) => {
                                errors
                                    .push(format!("Attempted redefinition of symbol \"{}\"", name));
                            }
                            std::collections::hash_map::Entry::Vacant(vacant) => {
                                vacant.insert(types.len().into());
                                types.push(
                                    declarations
                                        .get(*declaration)
                                        .expect("Invalid Declaration Index!")
                                        .clone(),
                                );
                            }
                        }
                    }
                }
                Decl::Struct { defined, .. } => {
                    if let Some(name) = defined.named_type() {
                        match symbols.type_names.entry(name.clone()) {
                            std::collections::hash_map::Entry::Occupied(_) => {
                                errors
                                    .push(format!("Attempted redefinition of symbol \"{}\"", name));
                            }
                            std::collections::hash_map::Entry::Vacant(vacant) => {
                                vacant.insert(types.len().into());
                                types.push(
                                    declarations
                                        .get(*declaration)
                                        .expect("Invalid Declaration Index!")
                                        .clone(),
                                );
                            }
                        }
                    }
                }
                Decl::Expr(expr) => identifier_counts(*expr, &symbols, declarations, expressions),
                Decl::Missing => (),
            }
        }
    }
    symbols
}

fn identifier_counts(
    expr: ExprIdx,
    symbols: &SymbolTable,
    declarations: &mut IndexVec<DeclIdx, Decl>,
    expressions: &mut IndexVec<ExprIdx, Expr>,
) {
    if let Some(expression) = expressions.get(expr) {
        let expression = expression.clone();
        match expression {
            Expr::Missing => (),
            Expr::Unit => (),
            Expr::Placeholder => (),
            Expr::Block {
                declarations: block_decls,
                terminator,
                ..
            } => {
                match terminator {
                    None => (),
                    Some(expr) => identifier_counts(expr, symbols, declarations, expressions),
                };
                for declaration in block_decls {
                    identifier_counts_decl(declaration, symbols, declarations, expressions);
                }
            }
            Expr::Binary {
                lhs,
                rhs,
                op,
                op_idx,
            } => {
                match op_idx {
                    Some(_) => (),
                    None => match symbols.value_names.get(op.str()) {
                        None => (),
                        Some(vec) => {
                            if vec.len() != 0 {
                                match expressions.get_mut(expr) {
                                    None => unreachable!("Invalid Expression Index!"),
                                    Some(expr) => match expr {
                                        Expr::Binary { op_idx, .. } => *op_idx = Some(vec.len()),
                                        _ => unreachable!(),
                                    },
                                }
                            }
                        }
                    },
                };
                identifier_counts(lhs, symbols, declarations, expressions);
                identifier_counts(rhs, symbols, declarations, expressions);
            }
            Expr::Grouping(expr) => {
                identifier_counts(expr, symbols, declarations, expressions);
            }
            Expr::Prefix { expr, op, op_idx } => {
                match op_idx {
                    Some(_) => (),
                    None => match symbols.value_names.get(op.str()) {
                        None => (),
                        Some(vec) => {
                            if vec.len() != 0 {
                                match expressions.get_mut(expr) {
                                    None => unreachable!("Invalid Expression Index!"),
                                    Some(expr) => match expr {
                                        Expr::Binary { op_idx, .. } => *op_idx = Some(vec.len()),
                                        _ => unreachable!(),
                                    },
                                }
                            }
                        }
                    },
                };
                identifier_counts(expr, symbols, declarations, expressions)
            }
            Expr::Tuple(exprs) => {
                for expr in exprs {
                    identifier_counts(expr, symbols, declarations, expressions)
                }
            }
            Expr::ArrayConstructor(exprs) => {
                for expr in exprs {
                    identifier_counts(expr, symbols, declarations, expressions)
                }
            }
            Expr::ArrayIndex { indexed, index } => {
                identifier_counts(indexed, symbols, declarations, expressions);
                identifier_counts(index, symbols, declarations, expressions);
            }
            Expr::If {
                condition,
                then,
                else_,
            } => {
                identifier_counts(condition, symbols, declarations, expressions);
                identifier_counts(then, symbols, declarations, expressions);
                match else_ {
                    None => (),
                    Some(expr) => identifier_counts(expr, symbols, declarations, expressions),
                };
            }
            Expr::While { condition, body } => {
                identifier_counts(condition, symbols, declarations, expressions);
                identifier_counts(body, symbols, declarations, expressions);
            }
            Expr::Loop(body) => identifier_counts(body, symbols, declarations, expressions),
            Expr::Assignment { assigned, value } => {
                identifier_counts(assigned, symbols, declarations, expressions);
                identifier_counts(value, symbols, declarations, expressions);
            }
            Expr::Identifier { name, index } => match index {
                Some(_) => (),
                None => match symbols.value_names.get(name.str()) {
                    None => println!("{:?}", symbols.value_names), //todo!() remove this and replace with ()
                    Some(vec) => {
                        if vec.len() != 0 {
                            match expressions.get_mut(expr) {
                                None => unreachable!("Invalid Expression Index!"),
                                Some(expr) => match expr {
                                    Expr::Identifier { index, .. } => *index = Some(vec.len()),
                                    _ => unreachable!(),
                                },
                            }
                        }
                    }
                },
            },
            Expr::Literal(_) => (),
            Expr::Unsafe(expr)
            | Expr::Dereference { expr }
            | Expr::Reference { expr, .. }
            | Expr::Return(expr) => identifier_counts(expr, symbols, declarations, expressions),
            Expr::Path {
                lhs: _lhs,
                rhs: _rhs,
            } => todo!(),
            Expr::FieldCall { lhs, .. } => {
                identifier_counts(lhs, symbols, declarations, expressions)
            }
            Expr::FunctionCall { lhs, arguments } | Expr::StructInit { lhs, arguments } => {
                for arg in arguments {
                    identifier_counts(arg, symbols, declarations, expressions);
                }
                identifier_counts(lhs, symbols, declarations, expressions);
            }
            Expr::Lambda {
                body,
                symbols: lambda_symbols,
                ..
            } => {
                identifier_counts(body, &lambda_symbols, declarations, expressions);
            }
        }
    } else {
        unreachable!("Invalid Expression Index was provided!");
    }
}

fn identifier_counts_decl(
    decl: DeclIdx,
    symbols: &SymbolTable,
    declarations: &mut IndexVec<DeclIdx, Decl>,
    expressions: &mut IndexVec<ExprIdx, Expr>,
) {
    if let Some(decl) = declarations.get_mut(decl) {
        match decl {
            Decl::Missing => (),
            Decl::Variable { value, .. } => match value {
                None => (),
                Some(value) => identifier_counts(*value, symbols, declarations, expressions),
            },
            Decl::Function { body, symbols, .. } => match body {
                None => (),
                Some(body) => identifier_counts(*body, &symbols.clone(), declarations, expressions),
            },
            Decl::Operator { .. } => (),
            Decl::Class { body, .. } => {
                identifier_counts(*body, symbols, declarations, expressions)
            }
            Decl::Implementation { body, .. } => {
                identifier_counts(*body, symbols, declarations, expressions)
            }
            Decl::Module { body, .. } => {
                identifier_counts(*body, symbols, declarations, expressions)
            }
            Decl::Union { .. } => (),
            Decl::Struct { .. } => (),
            Decl::Using { .. } => (),
            Decl::Expr(expr) => identifier_counts(*expr, symbols, declarations, expressions),
        };
    } else {
        unreachable!("invalid Declaration Index was provided!");
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct Token {
    kind: TokenKind,
    str: SmolStr,
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

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct Parameter {
    pattern: Pattern,
    type_: Option<Type>,
}
impl Parameter {
    pub fn lower(param: crate::reparse::decl::Parameter) -> Self {
        Parameter {
            pattern: Pattern::lower(param.pattern()),
            type_: param.type_().map(|x| Type::lower(Some(x))),
        }
    }
}

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

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Decl {
    Missing,
    Variable {
        mutable: bool,
        pattern: Pattern,
        type_: Option<Type>,
        value: Option<ExprIdx>,
    },
    Function {
        name: Token,
        args: Vec<Pattern>,
        type_: Option<Type>,
        symbols: SymbolTable,
        body: Option<ExprIdx>,
    },
    Operator {
        fixity: crate::reparse::decl::Fixity,
        op: Token,
        precedence: Option<Token>,
    },
    Class {
        constraints: Vec<(Token, Type)>,
        defined_class: Token,
        defined_type: Type,
        body: ExprIdx,
    },
    Implementation {
        constraints: Vec<(Token, Type)>,
        defined_class: Token,
        defined_type: Type,
        body: ExprIdx,
    },
    Module {
        name: Token,
        body: ExprIdx,
    },
    Union {
        defined: Type,
        variants: Vec<Variant>,
    },
    Struct {
        defined: Type,
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
        expressions: &mut IndexVec<ExprIdx, Expr>,
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
                        Expr::lower(using_decl.path(), errors, types, declarations, expressions);
                    declarations.push(Self::Using { path })
                }
                crate::reparse::decl::Decl::Expr(expr) => {
                    let result = Decl::Expr(Expr::lower(
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
        expressions: &mut IndexVec<ExprIdx, Expr>,
    ) -> Self {
        let mutable = decl.mutable();
        let pattern = Pattern::lower(decl.pattern());
        let type_ = decl.type_().map(|x| Type::lower(Some(x)));
        let value = decl
            .value()
            .map(|x| Expr::lower(Some(x), errors, types, declarations, expressions));
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
        expressions: &mut IndexVec<ExprIdx, Expr>,
    ) -> Self {
        let name = Token::lower(decl.name());
        let args: Vec<_> = decl.arguments().map(|x| Pattern::lower(Some(x))).collect();
        let type_ = decl.type_().map(|x| Type::lower(Some(x)));
        let mut symbols = SymbolTable::new();
        for arg in &args {
            let decl = Decl::Variable {
                mutable: false,
                pattern: arg.clone(),
                type_: Some(Type::Var(Token {
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
            .map(|x| Expr::lower(Some(x), errors, types, declarations, expressions));
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
        expressions: &mut IndexVec<ExprIdx, Expr>,
    ) -> Self {
        let constraints = match decl.constraints() {
            None => vec![],
            Some(x) => x
                .map(|x| (Token::lower(x.class()), Type::lower(x.type_())))
                .collect(),
        };
        let defined_class = Token::lower(decl.defined_class());
        let defined_type = Type::lower(decl.defined_type());
        let body = Expr::lower(decl.block(), errors, types, declarations, expressions);
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
        expressions: &mut IndexVec<ExprIdx, Expr>,
    ) -> Self {
        let constraints = match decl.constraints() {
            None => vec![],
            Some(x) => x
                .map(|x| (Token::lower(x.class()), Type::lower(x.type_())))
                .collect(),
        };
        let defined_class = Token::lower(decl.defined_class());
        let defined_type = Type::lower(decl.defined_type());
        let body = Expr::lower(decl.block(), errors, types, declarations, expressions);
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
        expressions: &mut IndexVec<ExprIdx, Expr>,
    ) -> Self {
        let name = Token::lower(decl.name());
        let body = Expr::lower(decl.body(), errors, types, declarations, expressions);
        Self::Module { name, body }
    }

    fn union_lower(decl: crate::reparse::decl::Union) -> Self {
        let defined = Type::lower(decl.defined_type());
        let variants = decl.variants().map(|x| Variant::lower(Some(x))).collect();
        Self::Union { defined, variants }
    }

    fn struct_lower(decl: crate::reparse::decl::Struct) -> Self {
        let defined = Type::lower(decl.defined_type());
        let body = StructBody::lower(decl.struct_body());
        Self::Struct { defined, body }
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum StructBody {
    Missing,
    TupleStruct(Type),
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
                    Self::TupleStruct(Type::lower(Some(tuple)))
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
    TupleVariant(Token, Type),
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
                    Type::lower(tuple.tuple_type()),
                ),
                crate::reparse::decl::Variant::Struct(struct_) => Self::StructVariant(
                    Token::lower(struct_.identifier()),
                    struct_.parameters().map(Parameter::lower).collect(),
                ),
            },
        }
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Expr {
    Missing,
    Unit,
    Placeholder,
    Block {
        symbols: SymbolTable,
        declarations: Vec<DeclIdx>,
        terminator: Option<ExprIdx>,
    },
    Binary {
        op: Token,
        op_idx: Option<usize>,
        lhs: ExprIdx,
        rhs: ExprIdx,
    },
    Grouping(ExprIdx), //needed to resolve binary expression precedence during static analysis
    Prefix {
        op: Token,
        op_idx: Option<usize>,
        expr: ExprIdx,
    },
    Tuple(Vec<ExprIdx>),
    ArrayConstructor(Vec<ExprIdx>),
    ArrayIndex {
        indexed: ExprIdx,
        index: ExprIdx,
    },
    If {
        condition: ExprIdx,
        then: ExprIdx,
        else_: Option<ExprIdx>,
    },
    While {
        condition: ExprIdx,
        body: ExprIdx,
    },
    Loop(ExprIdx),
    Assignment {
        assigned: ExprIdx,
        value: ExprIdx,
    },
    Identifier {
        name: Token,
        index: Option<usize>,
    },
    Literal(Token),
    Unsafe(ExprIdx),
    Dereference {
        expr: ExprIdx,
    },
    Reference {
        mutable: bool,
        expr: ExprIdx,
    },
    Return(ExprIdx),
    Path {
        lhs: ExprIdx,
        rhs: ExprIdx,
    },
    FieldCall {
        lhs: ExprIdx,
        rhs: ExprIdx,
    },
    FunctionCall {
        lhs: ExprIdx,
        arguments: Vec<ExprIdx>,
    },
    StructInit {
        lhs: ExprIdx,
        arguments: Vec<ExprIdx>,
    },
    Lambda {
        symbols: SymbolTable,
        args: Vec<Pattern>,
        body: ExprIdx,
    },
}
impl Expr {
    pub fn lower(
        cst: Option<crate::reparse::expr::Expr>,
        errors: &mut Vec<String>,
        types: &mut IndexVec<TypeIdx, Decl>,
        declarations: &mut IndexVec<DeclIdx, Decl>,
        expressions: &mut IndexVec<ExprIdx, Expr>,
    ) -> ExprIdx {
        if let Some(cst) = cst {
            match cst {
                crate::reparse::expr::Expr::Unit(_) => expressions.push(Self::Unit),
                crate::reparse::expr::Expr::Placeholder(_) => expressions.push(Self::Placeholder),
                crate::reparse::expr::Expr::Block(cst) => {
                    let result = Self::lower_block(cst, errors, types, declarations, expressions);
                    expressions.push(result)
                }
                crate::reparse::expr::Expr::If(if_expr) => {
                    let result = Self::lower_if(if_expr, errors, types, declarations, expressions);
                    expressions.push(result)
                }
                crate::reparse::expr::Expr::While(while_expr) => {
                    let result =
                        Self::lower_while(while_expr, errors, types, declarations, expressions);
                    expressions.push(result)
                }
                crate::reparse::expr::Expr::Loop(loop_expr) => {
                    let result =
                        Self::lower_loop(loop_expr, errors, types, declarations, expressions);
                    expressions.push(result)
                }
                crate::reparse::expr::Expr::Assignment(assignment) => {
                    let result = Self::lower_assignment(
                        assignment,
                        errors,
                        types,
                        declarations,
                        expressions,
                    );
                    expressions.push(result)
                }
                crate::reparse::expr::Expr::Grouping(expr) => {
                    let result =
                        Self::lower_grouping(expr, errors, types, declarations, expressions);
                    expressions.push(result)
                }
                crate::reparse::expr::Expr::Literal(literal) => {
                    expressions.push(Self::Literal(Token::lower(literal.token())))
                }
                crate::reparse::expr::Expr::Identifier(id) => expressions.push(Self::Identifier {
                    name: Token::lower(id.id()),
                    index: None,
                }),
                crate::reparse::expr::Expr::Lambda(lambda) => {
                    let result =
                        Self::lower_lambda(lambda, errors, types, declarations, expressions);
                    expressions.push(result)
                }
                crate::reparse::expr::Expr::Tuple(tuple) => {
                    let result = Self::lower_tuple(tuple, errors, types, declarations, expressions);
                    expressions.push(result)
                }
                crate::reparse::expr::Expr::Prefix(prefix) => {
                    let result =
                        Self::lower_prefix_op(prefix, errors, types, declarations, expressions);
                    expressions.push(result)
                }
                crate::reparse::expr::Expr::Binary(binary) => {
                    let result =
                        Self::lower_binary_op(binary, errors, types, declarations, expressions);
                    expressions.push(result)
                }
                crate::reparse::expr::Expr::Unsafe(unsafe_) => {
                    let result =
                        Self::lower_unsafe(unsafe_, errors, types, declarations, expressions);
                    expressions.push(result)
                }
                crate::reparse::expr::Expr::Path(path) => {
                    let result = Self::lower_path(path, errors, types, declarations, expressions);
                    expressions.push(result)
                }
                crate::reparse::expr::Expr::Dereference(deref) => {
                    let result = Self::lower_deref(deref, errors, types, declarations, expressions);
                    expressions.push(result)
                }
                crate::reparse::expr::Expr::Reference(ref_) => {
                    let result = Self::lower_ref(ref_, errors, types, declarations, expressions);
                    expressions.push(result)
                }
                crate::reparse::expr::Expr::FieldCall(field_call) => {
                    let result = Self::lower_field_call(
                        field_call,
                        errors,
                        types,
                        declarations,
                        expressions,
                    );
                    expressions.push(result)
                }
                crate::reparse::expr::Expr::FunctionCall(function_call) => {
                    let result = Self::lower_function_call(
                        function_call,
                        errors,
                        types,
                        declarations,
                        expressions,
                    );
                    expressions.push(result)
                }
                crate::reparse::expr::Expr::ArrayIndex(array_index) => {
                    let result = Self::lower_array_index(
                        array_index,
                        errors,
                        types,
                        declarations,
                        expressions,
                    );
                    expressions.push(result)
                }
                crate::reparse::expr::Expr::ArrayConstructor(array_constructor) => {
                    let result = Self::lower_array_constructor(
                        array_constructor,
                        errors,
                        types,
                        declarations,
                        expressions,
                    );
                    expressions.push(result)
                }
                crate::reparse::expr::Expr::Return(return_) => {
                    let result =
                        Self::lower_return(return_, errors, types, declarations, expressions);
                    expressions.push(result)
                }
                crate::reparse::expr::Expr::StructInit(struct_init) => {
                    let result = Self::lower_struct_init(
                        struct_init,
                        errors,
                        types,
                        declarations,
                        expressions,
                    );
                    expressions.push(result)
                }
            }
        } else {
            expressions.push(Self::Missing)
        };
        (expressions.len() - 1).into()
    }

    fn lower_block(
        block: crate::reparse::expr::BlockExpr,
        errors: &mut Vec<String>,
        types: &mut IndexVec<TypeIdx, Decl>,
        declarations: &mut IndexVec<DeclIdx, Decl>,
        expressions: &mut IndexVec<ExprIdx, Expr>,
    ) -> Self {
        let decls: Vec<DeclIdx> = block
            .declarations()
            .map(|x| Decl::lower(Some(x), errors, types, declarations, expressions))
            .collect();
        let terminator = block
            .terminator()
            .map(|x| Expr::lower(Some(x), errors, types, declarations, expressions));
        let symbols = symbols_from_declarations(&decls, declarations, expressions, errors, types);
        Self::Block {
            symbols,
            declarations: decls,
            terminator,
        }
    }

    fn lower_if(
        if_expr: crate::reparse::expr::IfExpr,
        errors: &mut Vec<String>,
        types: &mut IndexVec<TypeIdx, Decl>,
        declarations: &mut IndexVec<DeclIdx, Decl>,
        expressions: &mut IndexVec<ExprIdx, Expr>,
    ) -> Self {
        let condition = Expr::lower(
            if_expr.condition(),
            errors,
            types,
            declarations,
            expressions,
        );
        let then = Expr::lower(
            if_expr.then_block(),
            errors,
            types,
            declarations,
            expressions,
        );
        let else_ = if_expr
            .else_block()
            .map(|x| Expr::lower(Some(x), errors, types, declarations, expressions));
        Self::If {
            condition,
            then,
            else_,
        }
    }

    fn lower_while(
        while_expr: crate::reparse::expr::WhileExpr,
        errors: &mut Vec<String>,
        types: &mut IndexVec<TypeIdx, Decl>,
        declarations: &mut IndexVec<DeclIdx, Decl>,
        expressions: &mut IndexVec<ExprIdx, Expr>,
    ) -> Self {
        let condition = Expr::lower(
            while_expr.condition(),
            errors,
            types,
            declarations,
            expressions,
        );
        let body = Expr::lower(while_expr.block(), errors, types, declarations, expressions);
        Self::While { condition, body }
    }

    fn lower_loop(
        loop_expr: crate::reparse::expr::LoopExpr,
        errors: &mut Vec<String>,
        types: &mut IndexVec<TypeIdx, Decl>,
        declarations: &mut IndexVec<DeclIdx, Decl>,
        expressions: &mut IndexVec<ExprIdx, Expr>,
    ) -> Self {
        let body = Expr::lower(loop_expr.block(), errors, types, declarations, expressions);
        Self::Loop(body)
    }

    fn lower_assignment(
        expr: crate::reparse::expr::AssignmentExpr,
        errors: &mut Vec<String>,
        types: &mut IndexVec<TypeIdx, Decl>,
        declarations: &mut IndexVec<DeclIdx, Decl>,
        expressions: &mut IndexVec<ExprIdx, Expr>,
    ) -> Self {
        let lhs = Expr::lower(expr.lhs(), errors, types, declarations, expressions);
        let rhs = Expr::lower(expr.rhs(), errors, types, declarations, expressions);
        Self::Assignment {
            assigned: lhs,
            value: rhs,
        }
    }

    fn lower_grouping(
        expr: crate::reparse::expr::GroupingExpr,
        errors: &mut Vec<String>,
        types: &mut IndexVec<TypeIdx, Decl>,
        declarations: &mut IndexVec<DeclIdx, Decl>,
        expressions: &mut IndexVec<ExprIdx, Expr>,
    ) -> Self {
        Self::Grouping(Expr::lower(
            expr.expr(),
            errors,
            types,
            declarations,
            expressions,
        ))
    }

    fn lower_lambda(
        lambda: crate::reparse::expr::LambdaExpr,
        errors: &mut Vec<String>,
        types: &mut IndexVec<TypeIdx, Decl>,
        declarations: &mut IndexVec<DeclIdx, Decl>,
        expressions: &mut IndexVec<ExprIdx, Expr>,
    ) -> Self {
        let mut symbols = SymbolTable::new();
        let args: Vec<Pattern> = lambda.args().map(|x| Pattern::lower(Some(x))).collect();
        for arg in &args {
            let decl = Decl::Variable {
                mutable: false,
                pattern: arg.clone(),
                type_: Some(Type::Var(Token {
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
        let body = Expr::lower(lambda.body(), errors, types, declarations, expressions);
        Self::Lambda {
            symbols,
            args,
            body,
        }
    }

    fn lower_tuple(
        tuple: crate::reparse::expr::TupleExpr,
        errors: &mut Vec<String>,
        types: &mut IndexVec<TypeIdx, Decl>,
        declarations: &mut IndexVec<DeclIdx, Decl>,
        expressions: &mut IndexVec<ExprIdx, Expr>,
    ) -> Self {
        let values = tuple
            .values()
            .map(|x| Expr::lower(Some(x), errors, types, declarations, expressions))
            .collect();
        Self::Tuple(values)
    }

    fn lower_prefix_op(
        prefix: crate::reparse::expr::PrefixExpr,
        errors: &mut Vec<String>,
        types: &mut IndexVec<TypeIdx, Decl>,
        declarations: &mut IndexVec<DeclIdx, Decl>,
        expressions: &mut IndexVec<ExprIdx, Expr>,
    ) -> Self {
        let op = Token::lower(prefix.operator());
        let expr = Expr::lower(prefix.expr(), errors, types, declarations, expressions);
        Self::Prefix {
            op,
            op_idx: None,
            expr,
        }
    }

    fn lower_binary_op(
        binary: crate::reparse::expr::BinaryExpr,
        errors: &mut Vec<String>,
        types: &mut IndexVec<TypeIdx, Decl>,
        declarations: &mut IndexVec<DeclIdx, Decl>,
        expressions: &mut IndexVec<ExprIdx, Expr>,
    ) -> Self {
        let lhs = Expr::lower(binary.lhs(), errors, types, declarations, expressions);
        let op = Token::lower(binary.operator());
        let rhs = Expr::lower(binary.rhs(), errors, types, declarations, expressions);
        Self::Binary {
            op,
            op_idx: None,
            lhs,
            rhs,
        }
    }

    fn lower_unsafe(
        unsafe_: crate::reparse::expr::UnsafeExpr,
        errors: &mut Vec<String>,
        types: &mut IndexVec<TypeIdx, Decl>,
        declarations: &mut IndexVec<DeclIdx, Decl>,
        expressions: &mut IndexVec<ExprIdx, Expr>,
    ) -> Self {
        let expr = Expr::lower(unsafe_.expr(), errors, types, declarations, expressions);
        Self::Unsafe(expr)
    }

    fn lower_path(
        path: crate::reparse::expr::PathExpr,
        errors: &mut Vec<String>,
        types: &mut IndexVec<TypeIdx, Decl>,
        declarations: &mut IndexVec<DeclIdx, Decl>,
        expressions: &mut IndexVec<ExprIdx, Expr>,
    ) -> Self {
        let lhs = Expr::lower(path.lhs(), errors, types, declarations, expressions);
        let rhs = Expr::lower(path.rhs(), errors, types, declarations, expressions);
        Self::Path { lhs, rhs }
    }

    fn lower_deref(
        deref: crate::reparse::expr::DereferenceExpr,
        errors: &mut Vec<String>,
        types: &mut IndexVec<TypeIdx, Decl>,
        declarations: &mut IndexVec<DeclIdx, Decl>,
        expressions: &mut IndexVec<ExprIdx, Expr>,
    ) -> Self {
        let expr = Expr::lower(deref.expr(), errors, types, declarations, expressions);
        Self::Dereference { expr }
    }

    fn lower_ref(
        ref_: crate::reparse::expr::ReferenceExpr,
        errors: &mut Vec<String>,
        types: &mut IndexVec<TypeIdx, Decl>,
        declarations: &mut IndexVec<DeclIdx, Decl>,
        expressions: &mut IndexVec<ExprIdx, Expr>,
    ) -> Self {
        let mutable = ref_.mutable();
        let expr = Expr::lower(ref_.expr(), errors, types, declarations, expressions);
        Self::Reference { mutable, expr }
    }

    fn lower_field_call(
        field_call: crate::reparse::expr::FieldCallExpr,
        errors: &mut Vec<String>,
        types: &mut IndexVec<TypeIdx, Decl>,
        declarations: &mut IndexVec<DeclIdx, Decl>,
        expressions: &mut IndexVec<ExprIdx, Expr>,
    ) -> Self {
        let lhs = Expr::lower(field_call.lhs(), errors, types, declarations, expressions);
        let field = Expr::lower(field_call.field(), errors, types, declarations, expressions);
        Self::FieldCall { lhs, rhs: field }
    }

    fn lower_function_call(
        function_call: crate::reparse::expr::FunctionCallExpr,
        errors: &mut Vec<String>,
        types: &mut IndexVec<TypeIdx, Decl>,
        declarations: &mut IndexVec<DeclIdx, Decl>,
        expressions: &mut IndexVec<ExprIdx, Expr>,
    ) -> Self {
        let lhs = Expr::lower(
            function_call.lhs(),
            errors,
            types,
            declarations,
            expressions,
        );
        let arguments = function_call
            .args()
            .map(|x| Expr::lower(Some(x), errors, types, declarations, expressions))
            .collect();
        dbg!(&arguments);
        Self::FunctionCall { lhs, arguments }
    }

    fn lower_array_index(
        array_index: crate::reparse::expr::ArrayIndexExpr,
        errors: &mut Vec<String>,
        types: &mut IndexVec<TypeIdx, Decl>,
        declarations: &mut IndexVec<DeclIdx, Decl>,
        expressions: &mut IndexVec<ExprIdx, Expr>,
    ) -> Self {
        let lhs = Expr::lower(array_index.lhs(), errors, types, declarations, expressions);
        let index = Expr::lower(
            array_index.index(),
            errors,
            types,
            declarations,
            expressions,
        );
        Self::ArrayIndex {
            indexed: lhs,
            index,
        }
    }

    fn lower_array_constructor(
        array_constructor: crate::reparse::expr::ArrayConstructorExpr,
        errors: &mut Vec<String>,
        types: &mut IndexVec<TypeIdx, Decl>,
        declarations: &mut IndexVec<DeclIdx, Decl>,
        expressions: &mut IndexVec<ExprIdx, Expr>,
    ) -> Self {
        let values = array_constructor
            .values()
            .map(|x| Expr::lower(Some(x), errors, types, declarations, expressions))
            .collect();
        Self::ArrayConstructor(values)
    }

    fn lower_return(
        return_: crate::reparse::expr::ReturnExpr,
        errors: &mut Vec<String>,
        types: &mut IndexVec<TypeIdx, Decl>,
        declarations: &mut IndexVec<DeclIdx, Decl>,
        expressions: &mut IndexVec<ExprIdx, Expr>,
    ) -> Self {
        let expr = Expr::lower(return_.expr(), errors, types, declarations, expressions);
        Self::Return(expr)
    }

    fn lower_struct_init(
        struct_init: crate::reparse::expr::StructInitExpr,
        errors: &mut Vec<String>,
        types: &mut IndexVec<TypeIdx, Decl>,
        declarations: &mut IndexVec<DeclIdx, Decl>,
        expressions: &mut IndexVec<ExprIdx, Expr>,
    ) -> Self {
        let lhs = Expr::lower(struct_init.lhs(), errors, types, declarations, expressions);
        let values = struct_init
            .values()
            .map(|x| Expr::lower(Some(x), errors, types, declarations, expressions))
            .collect();
        Self::StructInit {
            lhs,
            arguments: values,
        }
    }
}

pub fn into_ast(source: &str) -> (SourceFile, Vec<String>) {
    let (tree, mut errors) = crate::reparse::into_tree(source);
    let ast = SourceFile::lower(tree, &mut errors);
    println!("{:#?}", ast);
    (ast, errors)
}
