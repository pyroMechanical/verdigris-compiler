mod data_types;
use crate::ast;
use crate::parser::data_types::TokenKind;
use ast::{decl::Decl, DeclIdx, expr::Expr, ExprIdx, SymbolTable};
use data_types::{new_unknown_type, RewriteRule, State, Type, TypeVarID};
use index_vec::{index_vec, IndexVec};
use smol_str::SmolStr;
use std::{collections::{VecDeque, HashMap, HashSet}};

fn identifier_definition<'a>(
    scopes: &'a Vec<&SymbolTable>,
    declarations: &'a IndexVec<DeclIdx, Decl>,
    id: &str,
    index: usize,
) -> Option<&'a Decl> {
    for scope in scopes.iter().rev() {
        match scope.find_identifier(id, index) {
            None => (),
            Some(x) => return declarations.get(x),
        }
    }
    return None;
}

fn identifier_definition_index<'a>(
    scopes: &'a Vec<&SymbolTable>,
    id: &str,
    index: usize,
) -> Option<DeclIdx> {
    for scope in scopes.iter().rev() {
        match scope.find_identifier(id, index) {
            None => (),
            x => return x,
        }
    }
    return None;
}

fn type_from_path_expression<'a, 'b>(expr: ExprIdx, state: &mut State<'a, 'b>) -> Option<Type>{
    match &state.expressions[expr] {
        Expr::Identifier { name, .. } => {
            for scope in state.scopes.iter().rev() {
                match scope.find_type(name.str()) {
                    None => (),
                    Some(x) => return Some(Type::Basic(x)),
                }
            };
            return None;
        },
        Expr::Path { lhs, rhs } => {
            let scope = namespace_from_path(*lhs, state);
            match scope {
                None => return None,
                Some(scope) => {
                    let mut new_state = State {
                        scopes: &mut vec![scope],
                        declarations: state.declarations,
                        decl_types: state.decl_types,
                        expressions: state.expressions,
                        expr_types: state.expr_types,
                        var_types: state.var_types,
                        map: state.map
                    };
                    type_from_path_expression(*rhs, &mut new_state)
                }
            }
        },
        _ => None
    }
}

fn convert_var_to_unknown(t: &Type, map: &mut HashMap<TypeVarID, usize>, var_types: &mut usize) -> Type {
    match t {
        Type::Applied(app, types) => {
            let app = convert_var_to_unknown(&**app, map, var_types);
            let types = types.iter().map(|x| convert_var_to_unknown(x, map, var_types)).collect();
            Type::Applied(Box::new(app), types)
        },
        Type::TypeVar(var_id) => {
            match map.entry(*var_id) {
                std::collections::hash_map::Entry::Occupied(occupied) => return Type::Unknown(TypeVarID(*occupied.get())),
                std::collections::hash_map::Entry::Vacant(vacant) => {
                    let x = *var_types;
                    *var_types += 1;
                    vacant.insert(x);
                    return Type::Unknown(TypeVarID(x))
                },
            };
        },
        Type::Lifetime(_var_id) => todo!(),
        x => x.clone()
    }
}

fn type_constraints(
    source: ast::SourceFile,
    expr_types: &mut IndexVec<ExprIdx, Option<Type>>,
    decl_types: &mut HashMap<DeclIdx, Type>,
) -> Vec<(Type, Type)> {
    let mut symbol_tables = vec![&source.symbol_names];
    let mut type_indices = vec![];
    let mut var_types = 0;
    let mut map = HashMap::new();
    for (idx, declaration) in source.declarations.iter().enumerate() {
        let idx: DeclIdx = idx.into();
        match declaration {
            Decl::Variable {type_, ..} | Decl::Function {type_, ..} => {
                match type_ {
                    None => {decl_types.insert(idx, new_unknown_type(&mut var_types));},
                    Some(type_) => {decl_types.insert(idx, Type::lower(type_, &mut symbol_tables, &mut HashMap::new(), &mut var_types).unwrap());}
                }
            },
            _ => (),
        }
    }
    let mut state = State {
        scopes: &mut symbol_tables,
        declarations: &source.declarations,
        decl_types: &decl_types,
        expressions: &source.expressions,
        expr_types,
        var_types: &mut var_types,
        map: &mut map,
    };
    for decl in &source.top_level_declarations {
        type_indices.append(&mut decl_type_constraints(*decl, &mut state));
    }
    type_indices
}

#[allow(unused)]
fn function_return_type_constraints<'a, 'b>(
    body: crate::ast::ExprIdx,
    state: &mut State<'a, 'b>,
) -> Vec<(Type, Type)> {
    match &state.expressions[body] {
        Expr::Block {
            symbols,
            declarations: decls,
            terminator,
        } => {
            state.scopes.push(symbols);
            let mut constraints: Vec<(Type, Type)> = vec![];
            for declaration in decls {
                match &state.declarations[*declaration] {
                    Decl::Variable { value, .. } => match value {
                        None => (),
                        Some(expr_id) => {
                            constraints
                                .append(&mut function_return_type_constraints(*expr_id, state));
                        }
                    },
                    Decl::Expr(expr) => {
                        constraints.append(&mut function_return_type_constraints(*expr, state))
                    }
                    _ => (),
                }
            }
            match terminator {
                None => (),
                Some(terminator) => {
                    constraints.append(&mut function_return_type_constraints(*terminator, state))
                }
            };
            state.scopes.pop();
            constraints
        }
        Expr::Unit => vec![],
        Expr::Placeholder => vec![],
        Expr::Grouping(expr_id) => function_return_type_constraints(*expr_id, state),
        Expr::Tuple(exprs) | Expr::ArrayConstructor(exprs) => exprs
            .iter()
            .flat_map(|expr| function_return_type_constraints(*expr, state))
            .collect(),
        Expr::ArrayIndex { indexed, index } => {
            let mut constraints = function_return_type_constraints(*indexed, state);
            constraints.append(&mut function_return_type_constraints(*index, state));
            constraints
        }
        Expr::If {
            condition,
            then,
            else_,
        } => {
            let mut constraints = function_return_type_constraints(*condition, state);
            constraints.append(&mut function_return_type_constraints(*then, state));
            match else_ {
                None => (),
                Some(else_) => {
                    constraints.append(&mut function_return_type_constraints(*else_, state))
                }
            };
            constraints
        }
        Expr::While { condition, body } => {
            let mut constraints = function_return_type_constraints(*condition, state);
            constraints.append(&mut function_return_type_constraints(*body, state));
            constraints
        }
        Expr::Loop(body) => function_return_type_constraints(*body, state),
        Expr::Assignment { assigned, value } => {
            let mut constraints = function_return_type_constraints(*assigned, state);
            constraints.append(&mut function_return_type_constraints(*value, state));
            constraints
        }
        Expr::Identifier { .. } => vec![],
        Expr::Literal(_) => vec![],
        Expr::Unsafe(expr)
        | Expr::Prefix { expr, .. }
        | Expr::Dereference { expr }
        | Expr::Reference { expr, .. } => function_return_type_constraints(*expr, state),
        Expr::Return(expr_id) => vec![(
            state.expr_types[body].clone().unwrap(),
            state.expr_types[*expr_id].clone().unwrap(),
        )],
        Expr::Path { lhs, rhs } | Expr::FieldCall { lhs, rhs } | Expr::Binary { lhs, rhs, .. } => {
            let mut constraints = function_return_type_constraints(*lhs, state);
            constraints.append(&mut function_return_type_constraints(*rhs, state));
            constraints
        }
        Expr::FunctionCall { lhs, arguments } => {
            let mut constraints = function_return_type_constraints(*lhs, state);
            for arg in arguments {
                constraints.append(&mut function_return_type_constraints(*arg, state));
            }
            constraints
        },
        Expr::StructInit { lhs, arguments } => {
            let mut constraints = function_return_type_constraints(*lhs, state);
            for (_, arg) in arguments {
                constraints.append(&mut function_return_type_constraints(*arg, state));
            }
            constraints
        }
        Expr::Lambda { body, .. } => function_return_type_constraints(*body, state),
        _ => unreachable!(),
    }
}

fn decl_type_constraints<'a, 'b>(
    source: crate::ast::DeclIdx,
    state: &mut State<'a, 'b>,
) -> Vec<(Type, Type)> {
    if let Some(decl) = state.declarations.get(source) {
        match decl {
            Decl::Missing => vec![],
            Decl::Variable {value, .. } => match value {
                Some(value) => {
                    let type_ = state.decl_types[&source].clone();
                    let mut result = expr_type_constraints(*value, state);
                    result.push((type_, state.expr_types[*value].clone().unwrap()));
                    result
                }
                None => vec![],
            },
            Decl::Function {
                type_,
                symbols,
                body,
                ..
            } => {
                state.scopes.push(symbols);
                match body {
                    None => vec![],
                    Some(body) => {
                        let mut constraints = expr_type_constraints(*body, state);
                        state.scopes.pop();
                        let mut types = vec![];
                        for arg in &symbols.value_declarations {
                            match &state.declarations[*arg] {
                                Decl::Variable { type_, .. } => types.push(
                                    convert_var_to_unknown(&Type::lower(
                                        type_.as_ref().unwrap(),
                                        state.scopes,
                                        &mut state.map.clone(),
                                        state.var_types,
                                    )
                                    .unwrap(), &mut HashMap::new(), state.var_types)
                                ),
                                _ => unreachable!(),
                            };
                        }
                        let mut return_type = state.expr_types[*body].clone().unwrap();
                        for type_ in types.into_iter().rev() {
                            return_type =
                                Type::Applied(Box::new(Type::Arrow), vec![type_, return_type]);
                        }
                        constraints.push((
                            return_type,
                            Type::lower(type_.as_ref().unwrap(), state.scopes, &mut state.map.clone(), state.var_types)
                                .unwrap(),
                        ));
                        constraints
                    }
                }
            }
            Decl::Operator { .. } => vec![],
            Decl::Class { body, defined_type, .. } => {
                let mut map = HashMap::new();
                Type::lower(&defined_type, state.scopes, &mut map, state.var_types);
                let mut new_state = State {
                    scopes: &mut state.scopes.clone(),
                    declarations: state.declarations,
                    decl_types: state.decl_types,
                    expressions: state.expressions,
                    expr_types: state.expr_types,
                    var_types: state.var_types,
                    map: &mut map,
                };
                 expr_type_constraints(*body, &mut new_state)
            },
            Decl::Implementation { body, .. } => expr_type_constraints(*body, state),
            Decl::Module { body, .. } => expr_type_constraints(*body, state),
            Decl::Union { .. } => vec![],
            Decl::Struct { .. } => vec![],
            Decl::Using { .. } => vec![],
            Decl::Expr(expr) => expr_type_constraints(*expr, state),
        }
    } else {
        unreachable!("invalid Declaration Index!");
    }
}

fn expr_type_constraints<'a, 'b>(source: ExprIdx, state: &mut State<'a, 'b>) -> Vec<(Type, Type)> {
    if let Some(expr) = state.expressions.get(source) {
        match expr {
            Expr::Missing => panic!("nonexistent expression found!"),
            Expr::Unit => {
                state.expr_types[source] = Some(Type::Tuple(0));
                vec![]
            }
            Expr::Placeholder => {
                state.expr_types[source] = Some(Type::Placeholder);
                vec![]
            }
            Expr::Block {
                symbols,
                declarations: decls,
                terminator,
            } => {
                state.scopes.push(symbols);
                let mut constraints = vec![];
                for decl in decls {
                    constraints.append(&mut decl_type_constraints(*decl, state));
                }
                match terminator {
                    None => state.expr_types[source] = Some(Type::Tuple(0)),
                    Some(expr) => {
                        let result = constraints.append(&mut expr_type_constraints(*expr, state));
                        state.expr_types[source] = state.expr_types[*expr].clone();
                        result
                    }
                };
                state.scopes.pop();
                constraints
            }
            Expr::Binary {
                op,
                op_idx,
                lhs,
                rhs,
            } => {
                let mut result = expr_type_constraints(*lhs, state);
                result.append(&mut expr_type_constraints(*rhs, state));
                let lhs_type = state.expr_types[*lhs].clone().unwrap();
                let rhs_type = state.expr_types[*rhs].clone().unwrap();
                let out = Type::Applied(
                    Box::new(Type::Arrow),
                    vec![rhs_type, new_unknown_type(state.var_types)],
                );
                let func_type = Type::Applied(Box::new(Type::Arrow), vec![lhs_type, out]);

                let decl = identifier_definition(state.scopes, state.declarations, op.str(), op_idx.unwrap());
                let operator_type = if let Some(decl) = decl {
                    match decl {
                        Decl::Variable { type_, .. } | Decl::Function { type_, .. } => {
                            match type_ {
                                None => None,
                                Some(t) => Type::lower(&t, state.scopes, &mut state.map.clone(), state.var_types),
                            }
                        }
                        _ => None,
                    }
                } else {
                    None
                };
                match operator_type {
                    None => (),
                    Some(type_) => result.push((func_type, convert_var_to_unknown(&type_, &mut HashMap::new(), state.var_types))),
                };
                result
            }
            Expr::Grouping(expr) => {
                let result = expr_type_constraints(*expr, state);
                state.expr_types[source] = state.expr_types[*expr].clone();
                result
            }
            Expr::Prefix { op, op_idx, expr } => {
                let mut result = expr_type_constraints(*expr, state);
                let decl = identifier_definition(state.scopes, state.declarations, op.str(), op_idx.unwrap());

                let expr_type = state.expr_types[*expr].clone().unwrap();
                let func_type = Type::Applied(
                    Box::new(Type::Arrow),
                    vec![expr_type, new_unknown_type(state.var_types)],
                );

                let operator_type = if let Some(decl) = decl {
                    match decl {
                        Decl::Variable { type_, .. } | Decl::Function { type_, .. } => {
                            match type_ {
                                None => None,
                                Some(t) => Type::lower(&t, state.scopes, &mut state.map.clone(), state.var_types),
                            }
                        }
                        _ => None,
                    }
                } else {
                    None
                };
                match operator_type {
                    None => (),
                    Some(type_) => result.push((func_type, convert_var_to_unknown(&type_, &mut HashMap::new(), state.var_types))),
                };
                result
            }
            Expr::Tuple(exprs) => {
                let result: Vec<_> = exprs
                    .iter()
                    .flat_map(|expr| expr_type_constraints(*expr, state))
                    .collect();
                let tuple_types: Vec<_> = exprs
                    .iter()
                    .map(|expr| state.expr_types[*expr].clone().unwrap())
                    .collect();
                state.expr_types[source] = Some(Type::Applied(
                    Box::new(Type::Tuple(tuple_types.len())),
                    tuple_types,
                ));
                result
            }
            Expr::ArrayConstructor(exprs) => {
                let mut result: Vec<_> = exprs
                    .into_iter()
                    .flat_map(|x| expr_type_constraints(*x, state))
                    .collect();
                let var = new_unknown_type(state.var_types);
                let array_type =
                    Type::Applied(Box::new(Type::Array(Some(exprs.len()))), vec![var.clone()]);
                for expr in exprs {
                    result.push((var.clone(), state.expr_types[*expr].clone().unwrap()));
                }
                state.expr_types[source] = Some(array_type);
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
                let mut result = expr_type_constraints(*condition, state);
                result.append(&mut expr_type_constraints(*then, state));
                state.expr_types[source] = state.expr_types[*then].clone();
                match else_ {
                    None => (),
                    Some(else_) => {
                        result.append(&mut expr_type_constraints(*then, state));
                        result.push((
                            state.expr_types[*then].clone().unwrap(),
                            state.expr_types[*else_].clone().unwrap(),
                        ))
                    }
                };
                result.push((
                    state.expr_types[*condition].clone().unwrap(),
                    Type::Basic(data_types::TYPE_BOOL.into()),
                ));
                result
            }
            Expr::While { condition, body } => {
                let mut result = expr_type_constraints(*condition, state);
                result.append(&mut expr_type_constraints(*body, state));
                result.push((
                    state.expr_types[*condition].clone().unwrap(),
                    Type::Basic(data_types::TYPE_BOOL.into()),
                ));
                state.expr_types[source] = state.expr_types[*body].clone();
                result
            }
            Expr::Loop(expr) => {
                let result = expr_type_constraints(*expr, state);
                state.expr_types[source] = state.expr_types[*expr].clone();
                result
            }
            Expr::Assignment { assigned, value } => {
                let mut result = expr_type_constraints(*assigned, state);
                result.append(&mut expr_type_constraints(*value, state));
                result.push((
                    state.expr_types[*assigned].clone().unwrap(),
                    state.expr_types[*value].clone().unwrap(),
                ));
                result
            }
            Expr::Identifier { name: id, index } => {
                assert!(index.is_some());
                let decl = identifier_definition_index(&state.scopes, id.str(), index.unwrap());
                let identifier_type = if let Some(decl) = decl {
                    match state.decl_types.get(&decl) {
                        None => None,
                        Some(type_) => Some(type_.clone()),
                    }
                } else {
                    None
                };
                state.expr_types[source] = identifier_type.clone();
                vec![]
            }
            Expr::Literal(t) => {
                match t.kind() {
                    TokenKind::True | TokenKind::False => {
                        state.expr_types[source] = Some(Type::Basic(data_types::TYPE_BOOL.into()))
                    }
                    TokenKind::Int | TokenKind::HexInt | TokenKind::OctInt | TokenKind::BinInt => {
                        state.expr_types[source] = Some(Type::Basic(data_types::TYPE_I32.into()))
                    }
                    TokenKind::Float => {
                        state.expr_types[source] = Some(Type::Basic(data_types::TYPE_F32.into()))
                    }
                    TokenKind::Double => {
                        state.expr_types[source] = Some(Type::Basic(data_types::TYPE_F64.into()))
                    }
                    TokenKind::Char => {
                        state.expr_types[source] = Some(Type::Basic(data_types::TYPE_CHAR.into()))
                    }
                    TokenKind::String => {
                        state.expr_types[source] = Some(Type::Basic(data_types::TYPE_STR.into()))
                    }
                    _ => unreachable!(),
                };
                vec![]
            }
            Expr::Unsafe(expr) => {
                let result = expr_type_constraints(*expr, state);
                state.expr_types[source] = state.expr_types[*expr].clone();
                result
            }
            Expr::Dereference { expr } => {
                let mut result = expr_type_constraints(*expr, state);
                state.expr_types[source] = Some(new_unknown_type(state.var_types));
                let type_ = Type::Applied(
                    Box::new(new_unknown_type(state.var_types)),
                    vec![state.expr_types[source].clone().unwrap()],
                );
                result.push((type_, state.expr_types[*expr].clone().unwrap()));
                result
            }
            Expr::Reference { mutable, expr } => {
                let result = expr_type_constraints(*expr, state);
                let ref_type = Type::Applied(
                    Box::new(Type::Reference { mutable: *mutable }),
                    vec![state.expr_types[*expr].clone().unwrap()],
                );
                state.expr_types[source] = Some(ref_type);
                result
            }
            Expr::Return(expr) => {
                state.expr_types[source] = Some(Type::Tuple(0));
                expr_type_constraints(*expr, state)
            }
            Expr::Path { lhs, rhs } => {
                let scope = namespace_from_path(*lhs, state);
                match scope {
                    None => todo!("handle error for failure path"),
                    Some(scope) => {
                        let mut scopes = vec![scope];
                        let mut new_state = State {
                            scopes: &mut scopes,
                            declarations: state.declarations,
                            decl_types: state.decl_types,
                            expressions: state.expressions,
                            expr_types: state.expr_types,
                            var_types: state.var_types,
                            map: state.map,
                        };
                        let result = expr_type_constraints(*rhs, &mut new_state);
                        state.expr_types[source] = state.expr_types[*rhs].clone();
                        result
                    }
                }
            }
            Expr::FunctionCall { lhs, arguments } => {
                let mut constraints = expr_type_constraints(*lhs, state);
                let fn_type = state.expr_types[*lhs].clone().unwrap();
                for arg in arguments {
                    constraints.append(&mut expr_type_constraints(*arg, state));
                }
                let mut return_type = new_unknown_type(state.var_types);
                for arg in arguments.iter().rev() {
                    let arg_type = state.expr_types[*arg].clone().unwrap();
                    return_type = Type::Applied(Box::new(Type::Arrow), vec![arg_type, return_type]);
                }
                constraints.push((convert_var_to_unknown(&fn_type, &mut HashMap::new(), state.var_types), return_type));
                constraints
            }
            Expr::Lambda {
                body,
                symbols,
                args,
            } => {
                state.scopes.push(symbols);
                let constraints = expr_type_constraints(*body, state);
                let mut return_type = state.expr_types[*body].clone().unwrap();
                for _arg in args {
                    return_type = Type::Applied(
                        Box::new(Type::Arrow),
                        vec![
                            new_unknown_type(state.var_types), /*todo!() replace this with type of each of the arguments */
                            return_type,
                        ],
                    );
                }
                state.expr_types[source] = Some(return_type);
                state.scopes.pop();
                constraints
            }
            Expr::StructInit { lhs, arguments  } => {
                let type_ = type_from_path_expression(*lhs, state).or_else(|| Some(new_unknown_type(state.var_types)));
                state.expr_types[*lhs] = type_.clone();
                let mut constraints = vec![];
                let mut values: Vec<(SmolStr, Type)> = vec![];
                for (name, argument) in arguments {
                    constraints.append(&mut expr_type_constraints(*argument, state));
                    values.push((name.str.clone(), state.expr_types[*argument].clone().unwrap_or(new_unknown_type(state.var_types))));
                }
                state.expr_types[source] = Some(Type::Struct{struct_type: Box::new(type_.unwrap_or(new_unknown_type(state.var_types))), values, complete:true});
                constraints
            }
            Expr::FieldCall { lhs, rhs } => {
                let mut constraints = expr_type_constraints(*lhs, state);
                let name = match &state.expressions[*rhs] {
                    ast::expr::Expr::Identifier { name, .. } => name.clone(),
                    _ => unreachable!(),
                };
                let rhs_type = new_unknown_type(state.var_types);
                state.expr_types[*rhs] = Some(rhs_type.clone());
                state.expr_types[source] = Some(rhs_type.clone());
                constraints.push((
                    state.expr_types[*lhs].clone().unwrap(),
                    Type::Struct {
                        struct_type: Box::new(new_unknown_type(state.var_types)),
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
fn namespace_from_path<'a, 'b>(
    source: ExprIdx,
    state: &mut State<'a, 'b>,
) -> Option<&'a SymbolTable> {
    match state.expressions.get(source)? {
        Expr::Identifier { name, .. } => {
            for scope in state.scopes.iter().rev() {
                let scope = scope.find_namespace(name.str());
                if scope.is_some() {
                    return scope;
                }
            }
        }
        Expr::Path { lhs, rhs } => {
            let scope = namespace_from_path(*lhs, state)?;
            if let Some(Expr::Identifier { name, .. }) = state.expressions.get(*rhs) {
                return scope.find_namespace(name.str());
            }
        }
        _ => unreachable!(),
    };
    None
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
                (Type::Unknown(var_id), other_type) | (other_type, Type::Unknown(var_id)) => {
                    rewrite_rules.push(RewriteRule {
                        type_replaced: Type::Unknown(var_id),
                        replaced_by: other_type.clone(),
                    });
                    constraints.iter_mut().for_each(|(x, y)| {
                        replace_unknown_with_other(x, var_id, &other_type);
                        replace_unknown_with_other(y, var_id, &other_type);
                    });
                }
                (Type::Array(size1), Type::Array(size2)) => match (size1, size2) {
                    (Some(size1), Some(size2)) => {
                        if size1 != size2 {
                            errors
                                .push(format!("Found different array lengths {:?} and {:?}", size1, size2))
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
                },
                (Type::Struct{ struct_type: type1, values:values1, complete:_complete1}, Type::Struct { struct_type: type2, values:values2, complete:_complete2 }) => {
                    constraints.push_back((*type1, *type2));
                    let mut map1 = HashMap::new();
                    let mut map2 = HashMap::new();
                    let mut shared_names = HashSet::new();
                    for (name, value) in &values1 {
                        map1.insert(name, value);
                        shared_names.insert(name);
                    }
                    for (name, value) in &values2 {
                        map2.insert(name, value);
                        shared_names.insert(name);
                    }
                    for name in shared_names {
                        let x = map1.get(name);
                        let y = map2.get(name);
                        if let (Some(&x), Some(&y)) = (x, y) {
                            constraints.push_back((x.clone(), y.clone()));
                        }
                    }
                },
                _ => (),
            }
        }
    }
    (rewrite_rules, errors)
}

fn replace_unknown_with_other(x: &mut Type, var_id: TypeVarID, other: &Type) {
    match x {
        Type::Applied(app, vars) => {
            replace_unknown_with_other(app, var_id, other);
            vars.iter_mut().for_each(|x| replace_unknown_with_other(x, var_id, other));
        },
        Type::Unknown(id) => if *id == var_id {
            *x = other.clone();
        },
        Type::Struct { values, .. } => {
            values.iter_mut().for_each(|(_, x)| replace_unknown_with_other(x, var_id, other));
        },
        _ => ()
    }
}

pub fn typecheck(source: &str) {
    let (ast, errors) = crate::ast::into_ast(source);
    let mut expr_types: IndexVec<ExprIdx, Option<Type>> = index_vec![None; ast.expressions.len()];
    let mut decl_types: HashMap<DeclIdx, Type> = HashMap::new();
    for err in &errors {
        println!("Error: {}", err);
    }
    let constraints = type_constraints(ast.clone(), &mut expr_types, &mut decl_types);
    println!("{:?}", constraints);
    print!("[");
    for (idx, expr) in ast.expressions.iter().enumerate() {
        print!(" {:?}: {:?},", expr, expr_types[idx]);
    };
    println!("]");
    for type_ in &expr_types {
        if type_.is_none() { 
            panic!("could not find type for an expression!");
        }
    }
    let mut expr_types: Vec<_> = expr_types.into_iter().map(|x| x.unwrap()).collect();
    let (rewrite_rules, errors) = solve_constraints(constraints);
    for err in &errors {
        println!("Error: {}", err);
    };
    println!("{:?}", rewrite_rules);
    for type_ in expr_types.iter_mut() {
        for rule in &rewrite_rules {
            if *type_ == rule.type_replaced {
                *type_ = rule.replaced_by.clone();
            }
        }
    };
    for (_, type_) in decl_types.iter_mut() {
        for rule in &rewrite_rules {
            if *type_ == rule.type_replaced {
                *type_ = rule.replaced_by.clone();
            }
        }
    }
    println!("{:?}", expr_types);
    println!("{:?}", decl_types);
    todo!()
}
