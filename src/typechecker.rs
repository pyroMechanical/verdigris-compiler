use crate::ast;
use crate::parser::data_types::TokenKind;
use ast::{ArrowType, DeclIdx, ExprIdx, SymbolTable, TypeIdx};
use ast::decl::Decl;
use ast::expr::Expr;
use index_vec::{index_vec, IndexVec};
use smol_str::SmolStr;
use std::collections::VecDeque;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct TypeVarID(usize);
impl TypeVarID {
    fn _increment(&mut self) -> Self {
        self.0 += 1;
        *self
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct LifetimeID(usize);
impl LifetimeID {
    fn _increment(&mut self) -> Self {
        self.0 += 1;
        *self
    }
}
#[derive(Clone, Debug, PartialEq, Eq)]
enum Type {
    Applied(Box<Type>, Vec<Type>), //used for all higher-order types, including functions, slices, and tuples
    Basic(TypeIdx),                //Represents a unique type that has a definition
    Array(Option<usize>),          //[] or [1]
    Tuple(usize),                  //(), (,), (,,), etc.
    Reference {
        mutable: bool,
    }, //& and &mut
    Pointer {
        mutable: bool,
    }, //% and %mut
    TypeVar(TypeVarID),            // 'T
    Lifetime(LifetimeID),          // #a
    Arrow,                         // (->)
    MutArrow,                      // (*>)
    OnceArrow,                     // (+>)
    Struct {
        values: Vec<(SmolStr, Type)>,
        complete: bool,
    },
}

const TYPE_BOOL: usize = 0;
const TYPE_I8: usize = 1;
const TYPE_I16: usize = 2;
const TYPE_I32: usize = 3;
const TYPE_I64: usize = 4;
const TYPE_I128: usize = 5;
const TYPE_ISIZE: usize = 6;
const TYPE_U8: usize = 7;
const TYPE_U16: usize = 8;
const TYPE_U32: usize = 9;
const TYPE_U64: usize = 10;
const TYPE_U128: usize = 11;
const TYPE_USIZE: usize = 12;
const TYPE_F32: usize = 13;
const TYPE_F64: usize = 14;
const TYPE_CHAR: usize = 15;
const TYPE_STR: usize = 16;

impl Type {
    fn lower(
        t: &crate::ast::types::Type,
        scopes: &Vec<&SymbolTable>,
        vartypes: &mut usize,
    ) -> Option<Self> {
        let result = match t {
            ast::types::Type::Missing => return None,
            ast::types::Type::Unit => Self::Tuple(0),
            ast::types::Type::Function {
                arrow,
                input,
                output,
            } => {
                let arrow = Box::new(match arrow {
                    ArrowType::Missing => return None,
                    ArrowType::Arrow => Self::Arrow,
                    ArrowType::MutArrow => Self::MutArrow,
                    ArrowType::OnceArrow => Self::OnceArrow,
                });
                let input = Self::lower(input, scopes, vartypes)?;
                let output = Self::lower(output, scopes, vartypes)?;
                Self::Applied(arrow, vec![input, output])
            }
            ast::types::Type::Applied { applied, type_vars } => {
                let applied = Box::new(Self::lower(applied, scopes, vartypes)?);
                let type_vars = type_vars
                    .iter()
                    .map(|x| Self::lower(x, scopes, vartypes))
                    .flatten()
                    .collect();
                Self::Applied(applied, type_vars)
            }
            ast::types::Type::Reference { mutable, type_ } => {
                let type_ = Self::lower(type_, scopes, vartypes)?;
                Self::Applied(Box::new(Self::Reference { mutable: *mutable }), vec![type_])
            }
            ast::types::Type::Pointer { mutable, type_ } => {
                let type_ = Self::lower(type_, scopes, vartypes)?;
                Self::Applied(Box::new(Self::Pointer { mutable: *mutable }), vec![type_])
            }
            ast::types::Type::Array { type_, size } => {
                let str = match size {
                    None => None,
                    Some(t) => match t.str().parse::<usize>() {
                        Ok(size) => Some(size),
                        Err(_) => None,
                    },
                };
                let array = Box::new(Self::Array(str));
                let type_ = Self::lower(type_, scopes, vartypes)?;
                Self::Applied(array, vec![type_])
            }
            ast::types::Type::Tuple(types) => {
                let size = types.len();
                let tuple = Box::new(Self::Tuple(size));
                let types: Vec<Self> = types
                    .iter()
                    .map(|x| Self::lower(x, scopes, vartypes))
                    .flatten()
                    .collect();
                if size != types.len() {
                    return None;
                } else {
                    Self::Applied(tuple, types)
                }
            }
            ast::types::Type::Lifetime(_) => {
                let x = *vartypes;
                *vartypes += 1;
                Self::Lifetime(LifetimeID(x))
            }
            ast::types::Type::Basic(name) => {
                macro_rules! name_match {
                    ($obj: expr, $($str: pat => $type:ident),*) => {
                        match $obj {
                            $(
                                $str => return Some(Type::Basic($type.into())),
                            )*
                            _ => ()
                        }
                    }
                }
                name_match! {
                    name.str(),
                    "bool" => TYPE_BOOL,
                    "i8" => TYPE_I8,
                    "i16" => TYPE_I16,
                    "i32" => TYPE_I32,
                    "i64" => TYPE_I64,
                    "i128" => TYPE_I128,
                    "isize" => TYPE_ISIZE,
                    "u8" => TYPE_U8,
                    "u16" => TYPE_U16,
                    "u32" => TYPE_U32,
                    "u64" => TYPE_U64,
                    "u128" => TYPE_U128,
                    "usize" => TYPE_USIZE,
                    "f32" => TYPE_F32,
                    "f64" => TYPE_F64,
                    "char" => TYPE_CHAR,
                    "str" => TYPE_STR
                };
                let mut id: Option<TypeIdx> = None;
                for scope in scopes.iter().rev() {
                    if id.is_some() {
                        break;
                    } else {
                        id = scope.find_type(name.str());
                    }
                }
                match id {
                    None => return None,
                    Some(id) => Self::Basic(id),
                }
            }
            ast::types::Type::Var(_) => new_type_var(vartypes),
        };
        Some(result)
    }
}

fn new_type_var(count: &mut usize) -> Type {
    let x = *count;
    *count += 1;
    Type::TypeVar(TypeVarID(x))
}

fn identifier_definition<'a>(
    scopes: &'a Vec<&SymbolTable>,
    id: &str,
    index: usize,
) -> Option<&'a Decl> {
    for scope in scopes.iter().rev() {
        match scope.find_identifier(id, index) {
            None => (),
            x => return x,
        }
    }
    return None;
}

fn type_constraints(
    source: ast::SourceFile,
    expr_types: &mut IndexVec<ExprIdx, Option<Type>>,
) -> Vec<(Type, Type)> {
    let mut symbol_tables = vec![&source.symbol_names];
    let mut type_indices = vec![];
    let mut var_types = 0;
    for decl in &source.top_level_declarations {
        type_indices.append(&mut decl_type_constraints(
            *decl,
            &mut symbol_tables,
            &source.declarations,
            &source.expressions,
            expr_types,
            &mut var_types,
        ));
    }
    type_indices
}

#[allow(unused)]
fn function_return_type_constraints<'a>(
    body: crate::ast::ExprIdx,
    scopes: &mut Vec<&'a SymbolTable>,
    declarations: &'a IndexVec<DeclIdx, Decl>,
    expressions: &'a IndexVec<ExprIdx, Expr>,
    expr_types: &mut IndexVec<ExprIdx, Option<Type>>,
    var_types: &mut usize,
) -> Vec<(Type, Type)> {
    match &expressions[body] {
        Expr::Block {
            symbols,
            declarations: decls,
            terminator,
        } => {
            scopes.push(symbols);
            let mut constraints: Vec<(Type, Type)> = vec![];
            for declaration in decls {
                match &declarations[*declaration] {
                    Decl::Variable { value, .. } => match value {
                        None => (),
                        Some(expr_id) => {
                            constraints.append(&mut function_return_type_constraints(
                                *expr_id,
                                scopes,
                                declarations,
                                expressions,
                                expr_types,
                                var_types,
                            ));
                        }
                    },
                    Decl::Expr(expr) => constraints.append(&mut function_return_type_constraints(
                        *expr,
                        scopes,
                        declarations,
                        expressions,
                        expr_types,
                        var_types,
                    )),
                    _ => (),
                }
            }
            match terminator {
                None => (),
                Some(terminator) => constraints.append(&mut function_return_type_constraints(
                    *terminator,
                    scopes,
                    declarations,
                    expressions,
                    expr_types,
                    var_types,
                )),
            };
            scopes.pop();
            constraints
        }
        Expr::Unit => vec![],
        Expr::Placeholder => vec![],
        Expr::Grouping(expr_id) => function_return_type_constraints(
            *expr_id,
            scopes,
            declarations,
            expressions,
            expr_types,
            var_types,
        ),
        Expr::Tuple(exprs) | Expr::ArrayConstructor(exprs) => exprs
            .iter()
            .flat_map(|expr| {
                function_return_type_constraints(
                    *expr,
                    scopes,
                    declarations,
                    expressions,
                    expr_types,
                    var_types,
                )
            })
            .collect(),
        Expr::ArrayIndex { indexed, index } => {
            let mut constraints = function_return_type_constraints(
                *indexed,
                scopes,
                declarations,
                expressions,
                expr_types,
                var_types,
            );
            constraints.append(&mut function_return_type_constraints(
                *index,
                scopes,
                declarations,
                expressions,
                expr_types,
                var_types,
            ));
            constraints
        }
        Expr::If {
            condition,
            then,
            else_,
        } => {
            let mut constraints = function_return_type_constraints(
                *condition,
                scopes,
                declarations,
                expressions,
                expr_types,
                var_types,
            );
            constraints.append(&mut function_return_type_constraints(
                *then,
                scopes,
                declarations,
                expressions,
                expr_types,
                var_types,
            ));
            match else_ {
                None => (),
                Some(else_) => constraints.append(&mut function_return_type_constraints(
                    *else_,
                    scopes,
                    declarations,
                    expressions,
                    expr_types,
                    var_types,
                )),
            };
            constraints
        }
        Expr::While { condition, body } => {
            let mut constraints = function_return_type_constraints(
                *condition,
                scopes,
                declarations,
                expressions,
                expr_types,
                var_types,
            );
            constraints.append(&mut function_return_type_constraints(
                *body,
                scopes,
                declarations,
                expressions,
                expr_types,
                var_types,
            ));
            constraints
        }
        Expr::Loop(body) => function_return_type_constraints(
            *body,
            scopes,
            declarations,
            expressions,
            expr_types,
            var_types,
        ),
        Expr::Assignment { assigned, value } => {
            let mut constraints = function_return_type_constraints(
                *assigned,
                scopes,
                declarations,
                expressions,
                expr_types,
                var_types,
            );
            constraints.append(&mut function_return_type_constraints(
                *value,
                scopes,
                declarations,
                expressions,
                expr_types,
                var_types,
            ));
            constraints
        }
        Expr::Identifier { .. } => vec![],
        Expr::Literal(_) => vec![],
        Expr::Unsafe(expr)
        | Expr::Prefix { expr, .. }
        | Expr::Dereference { expr }
        | Expr::Reference { expr, .. } => function_return_type_constraints(
            *expr,
            scopes,
            declarations,
            expressions,
            expr_types,
            var_types,
        ),
        Expr::Return(expr_id) => vec![(
            expr_types[body].clone().unwrap(),
            expr_types[*expr_id].clone().unwrap(),
        )],
        Expr::Path { lhs, rhs } | Expr::FieldCall { lhs, rhs } | Expr::Binary { lhs, rhs, .. } => {
            let mut constraints = function_return_type_constraints(
                *lhs,
                scopes,
                declarations,
                expressions,
                expr_types,
                var_types,
            );
            constraints.append(&mut function_return_type_constraints(
                *rhs,
                scopes,
                declarations,
                expressions,
                expr_types,
                var_types,
            ));
            constraints
        }
        Expr::FunctionCall { lhs, arguments } | Expr::StructInit { lhs, arguments } => {
            let mut constraints = function_return_type_constraints(
                *lhs,
                scopes,
                declarations,
                expressions,
                expr_types,
                var_types,
            );
            for arg in arguments {
                constraints.append(&mut function_return_type_constraints(
                    *arg,
                    scopes,
                    declarations,
                    expressions,
                    expr_types,
                    var_types,
                ));
            }
            constraints
        }
        Expr::Lambda { body, .. } => function_return_type_constraints(
            *body,
            scopes,
            declarations,
            expressions,
            expr_types,
            var_types,
        ),
        _ => unreachable!(),
    }
}

fn decl_type_constraints<'a>(
    source: crate::ast::DeclIdx,
    scopes: &mut Vec<&'a SymbolTable>,
    declarations: &'a IndexVec<DeclIdx, Decl>,
    expressions: &'a IndexVec<ExprIdx, Expr>,
    expr_types: &mut IndexVec<ExprIdx, Option<Type>>,
    var_types: &mut usize,
) -> Vec<(Type, Type)> {
    if let Some(decl) = declarations.get(source) {
        match decl {
            Decl::Missing => vec![],
            Decl::Variable { type_, value, .. } => match value {
                Some(value) => {
                    let mut result = expr_type_constraints(
                        *value,
                        scopes,
                        declarations,
                        expressions,
                        expr_types,
                        var_types,
                    );
                    if let Some(type_) = type_ {
                        let type_ = Type::lower(type_, scopes, var_types);
                        if let Some(type_) = type_ {
                            result.push((type_.clone(), expr_types[*value].clone().unwrap()));
                            result
                        } else {
                            expr_type_constraints(
                                *value,
                                scopes,
                                declarations,
                                expressions,
                                expr_types,
                                var_types,
                            )
                        }
                    } else {
                        expr_type_constraints(
                            *value,
                            scopes,
                            declarations,
                            expressions,
                            expr_types,
                            var_types,
                        )
                    }
                }
                None => vec![],
            },
            Decl::Function {
                type_,
                symbols,
                body,
                ..
            } => {
                scopes.push(symbols);
                match body {
                    None => vec![],
                    Some(body) => {
                        let mut constraints = expr_type_constraints(
                            *body,
                            scopes,
                            declarations,
                            expressions,
                            expr_types,
                            var_types,
                        );
                        scopes.pop();
                        let mut types = vec![];
                        for arg in &symbols.value_declarations {
                            match arg {
                                Decl::Variable { type_, .. } => types.push(
                                    Type::lower(type_.as_ref().unwrap(), scopes, var_types)
                                        .unwrap(),
                                ),
                                _ => unreachable!(),
                            };
                        }
                        let mut return_type = expr_types[*body].clone().unwrap();
                        for type_ in types.into_iter().rev() {
                            return_type =
                                Type::Applied(Box::new(Type::Arrow), vec![type_, return_type]);
                        }
                        dbg!(type_);
                        constraints.push((
                            return_type,
                            Type::lower(type_.as_ref().unwrap(), scopes, var_types).unwrap(),
                        ));
                        constraints
                    }
                }
            }
            Decl::Operator { .. } => vec![],
            Decl::Class { body, .. } => expr_type_constraints(
                *body,
                scopes,
                declarations,
                expressions,
                expr_types,
                var_types,
            ),
            Decl::Implementation { body, .. } => expr_type_constraints(
                *body,
                scopes,
                declarations,
                expressions,
                expr_types,
                var_types,
            ),
            Decl::Module { body, .. } => expr_type_constraints(
                *body,
                scopes,
                declarations,
                expressions,
                expr_types,
                var_types,
            ),
            Decl::Union { .. } => vec![],
            Decl::Struct { .. } => vec![],
            Decl::Using { .. } => vec![],
            Decl::Expr(expr) => expr_type_constraints(
                *expr,
                scopes,
                declarations,
                expressions,
                expr_types,
                var_types,
            ),
        }
    } else {
        unreachable!("invalid Declaration Index!");
    }
}

fn expr_type_constraints<'a>(
    source: ExprIdx,
    scopes: &mut Vec<&'a SymbolTable>,
    declarations: &'a IndexVec<DeclIdx, Decl>,
    expressions: &'a IndexVec<ExprIdx, Expr>,
    expr_types: &mut IndexVec<ExprIdx, Option<Type>>,
    var_types: &mut usize,
) -> Vec<(Type, Type)> {
    if let Some(expr) = expressions.get(source) {
        match expr {
            Expr::Missing => vec![],
            Expr::Unit => {
                expr_types[source] = Some(Type::Tuple(0));
                vec![]
            }
            Expr::Placeholder => {
                expr_types[source] = Some(new_type_var(var_types));
                vec![]
            }
            Expr::Block {
                symbols,
                declarations: decls,
                terminator,
            } => {
                scopes.push(symbols);
                let mut constraints = vec![];
                for decl in decls {
                    constraints.append(&mut decl_type_constraints(
                        *decl,
                        scopes,
                        declarations,
                        expressions,
                        expr_types,
                        var_types,
                    ));
                }
                match terminator {
                    None => expr_types[source] = Some(Type::Tuple(0)),
                    Some(expr) => {
                        let result = constraints.append(&mut expr_type_constraints(
                            *expr,
                            scopes,
                            declarations,
                            expressions,
                            expr_types,
                            var_types,
                        ));
                        expr_types[source] = expr_types[*expr].clone();
                        result
                    }
                };
                scopes.pop();
                constraints
            }
            Expr::Binary {
                op,
                op_idx,
                lhs,
                rhs,
            } => {
                let mut result = expr_type_constraints(
                    *lhs,
                    scopes,
                    declarations,
                    expressions,
                    expr_types,
                    var_types,
                );
                result.append(&mut expr_type_constraints(
                    *rhs,
                    scopes,
                    declarations,
                    expressions,
                    expr_types,
                    var_types,
                ));
                let lhs_type = expr_types[*lhs].clone().unwrap();
                let rhs_type = expr_types[*rhs].clone().unwrap();
                let out = Type::Applied(
                    Box::new(Type::Arrow),
                    vec![rhs_type, new_type_var(var_types)],
                );
                let func_type = Type::Applied(Box::new(Type::Arrow), vec![lhs_type, out]);

                let decl = identifier_definition(&scopes, op.str(), op_idx.unwrap());
                let operator_type = if let Some(decl) = decl {
                    match decl {
                        Decl::Variable { type_, .. } | Decl::Function { type_, .. } => {
                            match type_ {
                                None => None,
                                Some(t) => Type::lower(&t, scopes, var_types),
                            }
                        }
                        _ => None,
                    }
                } else {
                    None
                };
                match operator_type {
                    None => (),
                    Some(type_) => result.push((func_type, type_.clone())),
                };
                result
            }
            Expr::Grouping(expr) => {
                let result = expr_type_constraints(
                    *expr,
                    scopes,
                    declarations,
                    expressions,
                    expr_types,
                    var_types,
                );
                expr_types[source] = expr_types[*expr].clone();
                result
            }
            Expr::Prefix { op, op_idx, expr } => {
                let mut result = expr_type_constraints(
                    *expr,
                    scopes,
                    declarations,
                    expressions,
                    expr_types,
                    var_types,
                );
                let decl = identifier_definition(&scopes, op.str(), op_idx.unwrap());

                let expr_type = expr_types[*expr].clone().unwrap();
                let func_type = Type::Applied(
                    Box::new(Type::Arrow),
                    vec![expr_type, new_type_var(var_types)],
                );

                let operator_type = if let Some(decl) = decl {
                    match decl {
                        Decl::Variable { type_, .. } | Decl::Function { type_, .. } => {
                            match type_ {
                                None => None,
                                Some(t) => Type::lower(&t, scopes, var_types),
                            }
                        }
                        _ => None,
                    }
                } else {
                    None
                };
                match operator_type {
                    None => (),
                    Some(type_) => result.push((func_type, type_.clone())),
                };
                result
            }
            Expr::Tuple(exprs) => {
                let result: Vec<_> = exprs
                    .iter()
                    .flat_map(|expr| {
                        expr_type_constraints(
                            *expr,
                            scopes,
                            declarations,
                            expressions,
                            expr_types,
                            var_types,
                        )
                    })
                    .collect();
                let tuple_types: Vec<_> = exprs
                    .iter()
                    .map(|expr| expr_types[*expr].clone().unwrap())
                    .collect();
                expr_types[source] = Some(Type::Applied(
                    Box::new(Type::Tuple(tuple_types.len())),
                    tuple_types,
                ));
                result
            }
            Expr::ArrayConstructor(exprs) => {
                let mut result: Vec<_> = exprs
                    .into_iter()
                    .flat_map(|x| {
                        expr_type_constraints(
                            *x,
                            scopes,
                            declarations,
                            expressions,
                            expr_types,
                            var_types,
                        )
                    })
                    .collect();
                let var = new_type_var(var_types);
                let array_type =
                    Type::Applied(Box::new(Type::Array(Some(exprs.len()))), vec![var.clone()]);
                for expr in exprs {
                    result.push((var.clone(), expr_types[*expr].clone().unwrap()));
                }
                expr_types[source] = Some(array_type);
                result
            }
            Expr::ArrayIndex { .. } => {
                todo!()
            }
            Expr::If {
                condition,
                then,
                else_,
            } => {
                let mut result = expr_type_constraints(
                    *condition,
                    scopes,
                    declarations,
                    expressions,
                    expr_types,
                    var_types,
                );
                result.append(&mut expr_type_constraints(
                    *then,
                    scopes,
                    declarations,
                    expressions,
                    expr_types,
                    var_types,
                ));
                match else_ {
                    None => (),
                    Some(else_) => {
                        result.append(&mut expr_type_constraints(
                            *then,
                            scopes,
                            declarations,
                            expressions,
                            expr_types,
                            var_types,
                        ));
                        result.push((
                            expr_types[*then].clone().unwrap(),
                            expr_types[*else_].clone().unwrap(),
                        ))
                    }
                };
                result.push((
                    expr_types[*condition].clone().unwrap(),
                    Type::Basic(TYPE_BOOL.into()),
                ));
                result
            }
            Expr::While { condition, body } => {
                let mut result = expr_type_constraints(
                    *condition,
                    scopes,
                    declarations,
                    expressions,
                    expr_types,
                    var_types,
                );
                result.append(&mut expr_type_constraints(
                    *body,
                    scopes,
                    declarations,
                    expressions,
                    expr_types,
                    var_types,
                ));
                result.push((
                    expr_types[*condition].clone().unwrap(),
                    Type::Basic(TYPE_BOOL.into()),
                ));
                result
            }
            Expr::Loop(expr) => {
                let result = expr_type_constraints(
                    *expr,
                    scopes,
                    declarations,
                    expressions,
                    expr_types,
                    var_types,
                );
                expr_types[source] = expr_types[*expr].clone();
                result
            }
            Expr::Assignment { assigned, value } => {
                let mut result = expr_type_constraints(
                    *assigned,
                    scopes,
                    declarations,
                    expressions,
                    expr_types,
                    var_types,
                );
                result.append(&mut expr_type_constraints(
                    *value,
                    scopes,
                    declarations,
                    expressions,
                    expr_types,
                    var_types,
                ));
                result.push((
                    expr_types[*assigned].clone().unwrap(),
                    expr_types[*value].clone().unwrap(),
                ));
                result
            }
            Expr::Identifier { name: id, index } => {
                assert!(index.is_some());
                let decl = identifier_definition(&scopes, id.str(), index.unwrap());
                let identifier_type = if let Some(decl) = decl {
                    match decl {
                        Decl::Variable { type_, .. } | Decl::Function { type_, .. } => {
                            match type_ {
                                None => Some(new_type_var(var_types)),
                                Some(t) => Type::lower(&t, scopes, var_types),
                            }
                        }
                        _ => Some(new_type_var(var_types)),
                    }
                } else {
                    None
                };
                expr_types[source] = identifier_type;
                vec![]
            }
            Expr::Literal(t) => {
                match t.kind() {
                    TokenKind::True | TokenKind::False => {
                        expr_types[source] = Some(Type::Basic(TYPE_BOOL.into()))
                    }
                    TokenKind::Int | TokenKind::HexInt | TokenKind::OctInt | TokenKind::BinInt => {
                        expr_types[source] = Some(Type::Basic(TYPE_I32.into()))
                    }
                    TokenKind::Float => expr_types[source] = Some(Type::Basic(TYPE_F32.into())),
                    TokenKind::Double => expr_types[source] = Some(Type::Basic(TYPE_F64.into())),
                    TokenKind::Char => expr_types[source] = Some(Type::Basic(TYPE_CHAR.into())),
                    TokenKind::String => expr_types[source] = Some(Type::Basic(TYPE_STR.into())),
                    _ => unreachable!(),
                };
                vec![]
            }
            Expr::Unsafe(expr) => {
                let result = expr_type_constraints(
                    *expr,
                    scopes,
                    declarations,
                    expressions,
                    expr_types,
                    var_types,
                );
                expr_types[source] = expr_types[*expr].clone();
                result
            }
            Expr::Dereference { expr } => {
                let mut result = expr_type_constraints(
                    *expr,
                    scopes,
                    declarations,
                    expressions,
                    expr_types,
                    var_types,
                );
                expr_types[source] = Some(new_type_var(var_types));
                let type_ = Type::Applied(
                    Box::new(new_type_var(var_types)),
                    vec![expr_types[source].clone().unwrap()],
                );
                result.push((type_, expr_types[*expr].clone().unwrap()));
                result
            }
            Expr::Reference { mutable, expr } => {
                let result = expr_type_constraints(
                    *expr,
                    scopes,
                    declarations,
                    expressions,
                    expr_types,
                    var_types,
                );
                let ref_type = Type::Applied(
                    Box::new(Type::Reference { mutable: *mutable }),
                    vec![expr_types[*expr].clone().unwrap()],
                );
                expr_types[source] = Some(ref_type);
                result
            }
            Expr::Return(expr) => {
                expr_types[source] = Some(Type::Tuple(0));
                expr_type_constraints(
                    *expr,
                    scopes,
                    declarations,
                    expressions,
                    expr_types,
                    var_types,
                )
            }
            Expr::Path { lhs, rhs } => {
                let scope = namespace_from_path(*lhs, scopes, declarations, expressions);
                match scope {
                    None => todo!("handle error for failure path"),
                    Some(scope) => {
                        let mut scopes = vec![scope];
                        expr_type_constraints(
                            *rhs,
                            &mut scopes,
                            declarations,
                            expressions,
                            expr_types,
                            var_types,
                        )
                    }
                }
            }
            Expr::FunctionCall { lhs, arguments } => {
                let mut constraints = expr_type_constraints(
                    *lhs,
                    scopes,
                    declarations,
                    expressions,
                    expr_types,
                    var_types,
                );
                let fn_type = expr_types[*lhs].clone().unwrap();
                for arg in arguments {
                    constraints.append(&mut expr_type_constraints(
                        *arg,
                        scopes,
                        declarations,
                        expressions,
                        expr_types,
                        var_types,
                    ));
                }
                let mut return_type = new_type_var(var_types);
                dbg!(&arguments);
                for arg in arguments.iter().rev() {
                    let arg_type = expr_types[*arg].clone().unwrap();
                    return_type = Type::Applied(Box::new(Type::Arrow), vec![arg_type, return_type]);
                }
                constraints.push((fn_type, return_type));
                constraints
            }
            Expr::Lambda {
                body,
                symbols,
                args,
            } => {
                scopes.push(symbols);
                let constraints = expr_type_constraints(
                    *body,
                    scopes,
                    declarations,
                    expressions,
                    expr_types,
                    var_types,
                );
                let mut return_type = expr_types[*body].clone().unwrap();
                for _arg in args {
                    return_type = Type::Applied(
                        Box::new(Type::Arrow),
                        vec![
                            new_type_var(var_types), /*todo!() replace this with type of each of the arguments */
                            return_type,
                        ],
                    );
                }
                expr_types[source] = Some(return_type);
                scopes.pop();
                constraints
            }
            Expr::StructInit { .. } => {
                todo!()
            }
            Expr::FieldCall { lhs, rhs } => {
                let mut constraints = expr_type_constraints(
                    *lhs,
                    scopes,
                    declarations,
                    expressions,
                    expr_types,
                    var_types,
                );
                let name = match &expressions[*rhs] {
                    ast::expr::Expr::Identifier { name, .. } => name.clone(),
                    _ => unreachable!(),
                };
                let rhs_type = new_type_var(var_types);
                expr_types[*rhs] = Some(rhs_type.clone());
                constraints.push((
                    expr_types[*lhs].clone().unwrap(),
                    Type::Struct {
                        values: vec![(name.str().into(), rhs_type)],
                        complete: false,
                    },
                ));
                constraints
            }
        }
    } else {
        unreachable!("Invalid Expression Index!")
    }
}
/// expects only the parts of the path that are namespaces.
/// for example, if the full path is core::mem::x,
/// the path provided to this function would be core::mem.
fn namespace_from_path<'a>(
    source: ExprIdx,
    scopes: &mut Vec<&'a SymbolTable>,
    declarations: &'a IndexVec<DeclIdx, Decl>,
    expressions: &'a IndexVec<ExprIdx, Expr>,
) -> Option<&'a SymbolTable> {
    match expressions.get(source)? {
        Expr::Identifier { name, .. } => {
            for scope in scopes.iter().rev() {
                let scope = scope.find_namespace(name.str());
                if scope.is_some() {
                    return scope;
                }
            }
        }
        Expr::Path { lhs, rhs } => {
            let scope = namespace_from_path(*lhs, scopes, declarations, expressions)?;
            if let Some(Expr::Identifier { name, .. }) = expressions.get(*rhs) {
                return scope.find_namespace(name.str());
            }
        }
        _ => unreachable!(),
    };
    None
}

#[derive(Clone, Debug)]
#[allow(unused)]
struct RewriteRule {
    type_replaced: Type,
    replaced_by: Type,
}

fn solve_constraints(constraints: Vec<(Type, Type)>) -> (Vec<RewriteRule>, Vec<String>) {
    let mut constraints: VecDeque<_> = constraints.into();
    let mut rewrite_rules = vec![];
    let mut errors = vec![];
    while !constraints.is_empty() {
        let constraint = constraints.pop_front();
        if let Some(constraint) = constraint {
            match constraint {
                (Type::Applied(t1, ts1), Type::Applied(t2, ts2)) => {
                    constraints.push_back((*t1, *t2));
                    if ts1.len() != ts2.len() {
                        errors.push(format!(
                            "Types do not have the same number of type parameters!"
                        ));
                    } else {
                        let applied =
                            Iterator::zip(ts1.into_iter(), ts2.into_iter()).collect::<Vec<_>>();
                        for x in applied {
                            constraints.push_back(x)
                        }
                    }
                }
                (Type::Basic(x), Type::Basic(y)) => {
                    if x != y {
                        errors.push(format!("Types {:?} and {:?} do not match!", x, y));
                    }
                }
                (Type::TypeVar(var_id), other_type) | (other_type, Type::TypeVar(var_id)) => {
                    rewrite_rules.push(RewriteRule {
                        type_replaced: Type::TypeVar(var_id),
                        replaced_by: other_type.clone(),
                    });
                    constraints.iter_mut().for_each(|(x, y)| {
                        if *x == Type::TypeVar(var_id) {
                            *x = other_type.clone();
                        }
                        if *y == Type::TypeVar(var_id) {
                            *y = other_type.clone();
                        }
                    });
                }
                (Type::Array(size1), Type::Array(size2)) => match (size1, size2) {
                    (Some(x), Some(y)) => {
                        if x != y {
                            errors
                                .push(format!("Found different array lengths {:?} and {:?}", x, y))
                        }
                    }
                    _ => (),
                },
                (Type::Tuple(size1), Type::Tuple(size2)) => {
                    if size1 != size2 {
                        errors.push(format!(
                            "Tuples of size {:?} and {:?} do not match!",
                            size1, size2
                        ))
                    }
                }
                _ => (),
            }
        }
    }
    (rewrite_rules, errors)
}

pub fn typecheck(source: &str) {
    let (ast, errors) = crate::ast::into_ast(source);
    let mut expr_types: IndexVec<ExprIdx, Option<Type>> = index_vec![None; ast.expressions.len()];
    for err in &errors {
        println!("Error: {}", err);
    }
    let constraints = type_constraints(ast, &mut expr_types);
    println!("{:?}", constraints);
    let (rewrite_rules, errors) = solve_constraints(constraints);
    for err in &errors {
        println!("Error: {}", err);
    }
    println!("{:?}", rewrite_rules);
    todo!()
}
