mod data_types;
pub mod decl;
pub mod expr;
mod patterns;
pub mod types;
use index_vec::{index_vec, IndexVec};

pub use data_types::{DeclIdx, ExprIdx, TypeIdx, SymbolTable, Token};

pub use types::ArrowType;

/// expects only the parts of the path that are namespaces.
/// for example, if the full path is core::mem::x,
/// the path provided to this function would be core::mem.
fn namespace_from_path<'a, 'b>(
    source: ExprIdx,
    scopes: &'b Vec<&'a SymbolTable>,
    expressions: &'a IndexVec<ExprIdx, expr::Expr>
) -> Option<&'a SymbolTable> {
    match expressions.get(source)? {
        expr::Expr::Identifier { name, .. } => {
            for scope in scopes.iter().rev() {
                let scope = scope.find_namespace(name.str());
                if scope.is_some() {
                    return scope;
                }
            }
        }
        expr::Expr::Path { lhs, rhs } => {
            let scope = namespace_from_path(*lhs, scopes, expressions)?;
            if let Some(expr::Expr::Identifier { name, .. }) = expressions.get(*rhs) {
                return scope.find_namespace(name.str());
            }
        }
        _ => unreachable!(),
    };
    None
}


#[derive(PartialEq, Eq, Debug, Clone)]
pub struct SourceFile {
    pub declarations: IndexVec<DeclIdx, decl::Decl>,
    pub top_level_declarations: Vec<DeclIdx>,
    pub expressions: IndexVec<ExprIdx, expr::Expr>,
    pub symbol_names: SymbolTable,
    pub types: IndexVec<TypeIdx, decl::Decl>,
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
        let mut types = index_vec![decl::Decl::Missing; 17];
        let mut expressions: IndexVec<ExprIdx, expr::Expr> = index_vec![];
        let mut declarations: IndexVec<DeclIdx, decl::Decl> = index_vec![];
        let top_level_declarations = source
            .decls()
            .map(|x| {
                decl::Decl::lower(
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
    declarations: &mut IndexVec<DeclIdx, decl::Decl>,
    expressions: &mut IndexVec<ExprIdx, expr::Expr>,
    errors: &mut Vec<String>,
    types: &mut IndexVec<TypeIdx, decl::Decl>,
) -> SymbolTable {
    let mut symbols = SymbolTable::new();
    for declaration in declaration_ids {
        if let Some(decl) = declarations.get_mut(*declaration) {
            match decl {
                decl::Decl::Variable { pattern, value, .. } => {
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
                decl::Decl::Function { name, body, symbols:fn_symbols,.. } => {
                    let name = name.str.clone();
                    let fn_symbols = fn_symbols.clone();
                    match body {
                        None => (),
                        Some(body) => {
                            let body = *body;
                            identifier_counts(body, &fn_symbols, declarations, expressions);
                            identifier_counts(body, &symbols, declarations, expressions);
                        },
                    };
                    match symbols.value_names.entry(name.clone()) {
                        std::collections::hash_map::Entry::Occupied(_) => {
                            errors.push(format!("Attempted redefinition of symbol \"{}\"", &name));
                        }
                        std::collections::hash_map::Entry::Vacant(vacant) => {
                            vacant.insert(vec![symbols.value_declarations.len()]);
                            let declaration = declarations.get(*declaration).expect("Invalid Declaration Index!").clone();
                            symbols.value_declarations.push(declaration);
                        }
                    };
                }
                decl::Decl::Class {
                    defined_class,
                    body,
                    ..
                } => {
                    let name = defined_class.str.clone();
                    let index = *body;
                    identifier_counts(index, &symbols, declarations, expressions);
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
                    match symbols.namespaces.entry(name.clone()) {
                        std::collections::hash_map::Entry::Occupied(_) => {
                            errors.push(format!("Attempted redefinition of namespace \"{}\"", name));
                        }
                        std::collections::hash_map::Entry::Vacant(vacant) => {
                            if let expr::Expr::Block { symbols, ..} = &expressions[index] {
                                vacant.insert(symbols.clone());
                            }
                        }
                    }
                }
                decl::Decl::Implementation {
                    defined_class,
                    defined_type,
                    body,
                    ..
                } => {
                    let name = defined_type.named_type();
                    let index = *body;
                    let class = defined_class.str.clone();
                    identifier_counts(index, &symbols, declarations, expressions);
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

                                match table.namespaces.entry(class.clone()) {
                                    std::collections::hash_map::Entry::Occupied(_) => {
                                        errors.push(format!("Impl for class \"{}\" for type \"{}\" already exists!", class, name));
                                    }
                                    std::collections::hash_map::Entry::Vacant(vacant) => {
                                        if let expr::Expr::Block { symbols, ..} = &expressions[index] {
                                            vacant.insert(symbols.clone());
                                        }
                                    }
                                }
                            }
                            std::collections::hash_map::Entry::Vacant(_) => {
                                errors.push(format!("Could not find type \"{}\"", name));
                            }
                        }
                    }
                }
                decl::Decl::Module { name, body } => {
                    let name = name.str.clone();
                    let index = *body;
                    identifier_counts(index, &symbols, declarations, expressions);
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

                    match symbols.namespaces.entry(name.clone()) {
                        std::collections::hash_map::Entry::Occupied(_) => {
                            errors.push(format!("Attempted redefinition of namespace \"{}\"", name));
                        }
                        std::collections::hash_map::Entry::Vacant(vacant) => {
                            if let expr::Expr::Block { symbols, ..} = &expressions[index] {
                                vacant.insert(symbols.clone());
                            }
                            
                        }
                    }
                }
                decl::Decl::Operator { op, .. } => match symbols.operator_tokens.entry(op.str.clone()) {
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
                decl::Decl::Using { .. } => {}
                decl::Decl::Union { defined, .. } => {
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

                        match symbols.namespaces.entry(name.clone()) {
                            std::collections::hash_map::Entry::Occupied(_) => errors
                            .push(format!("Namespace \"{}\" already exists!", name)),
                            std::collections::hash_map::Entry::Vacant(vacant) => {
                                vacant.insert(SymbolTable::new());
                            },
                        }
                    }
                }
                decl::Decl::Struct { defined, .. } => {
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

                        match symbols.namespaces.entry(name.clone()) {
                            std::collections::hash_map::Entry::Occupied(_) => errors
                            .push(format!("Namespace \"{}\" already exists!", name)),
                            std::collections::hash_map::Entry::Vacant(vacant) => {
                                vacant.insert(SymbolTable::new());
                            },
                        }
                    }
                }
                decl::Decl::Expr(expr) => identifier_counts(*expr, &symbols, declarations, expressions),
                decl::Decl::Missing => (),
            }
        }
    }
    symbols
}

fn identifier_counts(
    expr: ExprIdx,
    symbols: &SymbolTable,
    declarations: &mut IndexVec<DeclIdx, decl::Decl>,
    expressions: &mut IndexVec<ExprIdx, expr::Expr>,
) {
    if let Some(expression) = expressions.get(expr) {
        let expression = expression.clone();
        match expression {
            expr::Expr::Missing => (),
            expr::Expr::Unit => (),
            expr::Expr::Placeholder => (),
            expr::Expr::Block {
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
            expr::Expr::Binary {
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
                                        expr::Expr::Binary { op_idx, .. } => *op_idx = Some(vec.len()),
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
            expr::Expr::Grouping(expr) => {
                identifier_counts(expr, symbols, declarations, expressions);
            }
            expr::Expr::Prefix { expr, op, op_idx } => {
                match op_idx {
                    Some(_) => (),
                    None => match symbols.value_names.get(op.str()) {
                        None => (),
                        Some(vec) => {
                            if vec.len() != 0 {
                                match expressions.get_mut(expr) {
                                    None => unreachable!("Invalid Expression Index!"),
                                    Some(expr) => match expr {
                                        expr::Expr::Binary { op_idx, .. } => *op_idx = Some(vec.len()),
                                        _ => unreachable!(),
                                    },
                                }
                            }
                        }
                    },
                };
                identifier_counts(expr, symbols, declarations, expressions)
            }
            expr::Expr::Tuple(exprs) => {
                for expr in exprs {
                    identifier_counts(expr, symbols, declarations, expressions)
                }
            }
            expr::Expr::ArrayConstructor(exprs) => {
                for expr in exprs {
                    identifier_counts(expr, symbols, declarations, expressions)
                }
            }
            expr::Expr::ArrayIndex { indexed, index } => {
                identifier_counts(indexed, symbols, declarations, expressions);
                identifier_counts(index, symbols, declarations, expressions);
            }
            expr::Expr::If {
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
            expr::Expr::While { condition, body } => {
                identifier_counts(condition, symbols, declarations, expressions);
                identifier_counts(body, symbols, declarations, expressions);
            }
            expr::Expr::Loop(body) => identifier_counts(body, symbols, declarations, expressions),
            expr::Expr::Assignment { assigned, value } => {
                identifier_counts(assigned, symbols, declarations, expressions);
                identifier_counts(value, symbols, declarations, expressions);
            }
            expr::Expr::Identifier { name, index } => match index {
                Some(_) => (),
                None => match symbols.value_names.get(name.str()) {
                    None => println!("{:?}", symbols.value_names), //todo!() remove this and replace with ()
                    Some(vec) => {
                        if vec.len() != 0 {
                            match expressions.get_mut(expr) {
                                None => unreachable!("Invalid Expression Index!"),
                                Some(expr) => match expr {
                                    expr::Expr::Identifier { index, .. } => *index = Some(vec.len()),
                                    _ => unreachable!(),
                                },
                            }
                        }
                    }
                },
            },
            expr::Expr::Literal(_) => (),
            expr::Expr::Unsafe(expr)
            | expr::Expr::Dereference { expr }
            | expr::Expr::Reference { expr, .. }
            | expr::Expr::Return(expr) => identifier_counts(expr, symbols, declarations, expressions),
            expr::Expr::Path {
                lhs,
                rhs,
            } => {
                //todo!() clean this shit up by removing clone of expressions vector while still finding
                //symbol table
                identifier_counts(lhs, symbols, declarations, expressions);
                let expr = expressions.clone();
                let scope = namespace_from_path(lhs, &vec![symbols], &expr);
                match scope {
                    Some(scope) => identifier_counts(rhs, &scope, declarations, expressions),
                    None => ()
                }
                
            },
            expr::Expr::FieldCall { lhs, .. } => {
                identifier_counts(lhs, symbols, declarations, expressions)
            }
            expr::Expr::FunctionCall { lhs, arguments } => {
                for arg in arguments {
                    identifier_counts(arg, symbols, declarations, expressions);
                }
                identifier_counts(lhs, symbols, declarations, expressions);
            },
            expr::Expr::StructInit { lhs, arguments } => {
                for (_, arg) in arguments {
                    identifier_counts(arg, symbols, declarations, expressions);
                }
                identifier_counts(lhs, symbols, declarations, expressions);
            }
            expr::Expr::Lambda {
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
    declarations: &mut IndexVec<DeclIdx, decl::Decl>,
    expressions: &mut IndexVec<ExprIdx, expr::Expr>,
) {
    if let Some(decl) = declarations.get_mut(decl) {
        match decl {
            decl::Decl::Missing => (),
            decl::Decl::Variable { value, .. } => match value {
                None => (),
                Some(value) => identifier_counts(*value, symbols, declarations, expressions),
            },
            decl::Decl::Function { body, symbols, .. } => match body {
                None => (),
                Some(body) => identifier_counts(*body, &symbols.clone(), declarations, expressions),
            },
            decl::Decl::Operator { .. } => (),
            decl::Decl::Class { body, .. } => {
                identifier_counts(*body, symbols, declarations, expressions)
            }
            decl::Decl::Implementation { body, .. } => {
                identifier_counts(*body, symbols, declarations, expressions)
            }
            decl::Decl::Module { body, .. } => {
                identifier_counts(*body, symbols, declarations, expressions)
            }
            decl::Decl::Union { .. } => (),
            decl::Decl::Struct { .. } => (),
            decl::Decl::Using { .. } => (),
            decl::Decl::Expr(expr) => identifier_counts(*expr, symbols, declarations, expressions),
        };
    } else {
        unreachable!("invalid Declaration Index was provided!");
    }
}

pub fn into_ast(source: &str) -> (SourceFile, Vec<String>) {
    let (tree, mut errors) = crate::reparse::into_tree(source);
    let ast = SourceFile::lower(tree, &mut errors);
    println!("{:#?}", ast);
    (ast, errors)
}
