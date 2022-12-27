use std::collections::HashMap;
use crate::lexer::Token;
use crate::parser::{Decl, Expr, Pattern, Ty, Param, Fixity, CaseExpr};
use crate::SpanWith;

struct Scope<'a, 'b> where 'b: 'a {
    values: HashMap<&'a str, &'b Decl<'a>>,
    types: HashMap<&'a str, &'b Decl<'a>>,
    macros: HashMap<&'a str, &'b Decl<'a>>,
    type_vars: u64
} 

impl<'a, 'b> Scope<'a, 'b>{
    fn new() -> Self {
        Scope{values: HashMap::new(), types: HashMap::new(), macros: HashMap::new(), type_vars: 0}
    }
    
}

struct Node<T>{parent: Option<*const Node<T>>, children: Vec<Node<T>>, value: T}

impl<T> Node<T> {
    fn new(parent: Option<*const Node<T>>, value: T) -> Self {
        Node{parent, children: vec![], value}
    }

    fn parent(&self) -> Option<&Node<T>>{
        self.parent.and_then(|parent| {Some(unsafe{&*parent})})
    }

    fn insert_child(&mut self, value: T) -> &mut Self {
        self.children.push(Self::new(Some(self), value));
        self.children.last_mut().unwrap()
    }
}

enum Namespace {
    Value,
    Type,
    Macro
}

fn find_in_scope<'a, 'b>(identifier: &str, scope: &'b Node<Scope<'a, 'b>>, namespace: Namespace) -> Option<&'b Decl<'a>> where 'a: 'b {

    let name = match namespace {
        Namespace::Value =>scope.value.values.get(identifier),
        Namespace::Type =>scope.value.types.get(identifier),
        Namespace::Macro => scope.value.macros.get(identifier),
    };
    match name {
        Some(&name) => Some(name),
        None => {
            let parent = scope.parent();
            match parent {
                Some(parent) => find_in_scope(identifier, parent, namespace),
                None => None
            }
        }
    }
}

pub fn typecheck(source: SpanWith) -> Result<(), nom::error::Error<SpanWith>> {
    let result = crate::parser::parse(source)?;
    let mut scope_stack = Node::new(None, Scope::new());
    for  decl in & result {
        resolve_symbols(decl.as_ref(), &mut scope_stack);
    }
    Ok(())
}

fn new_type_var<'a, 'b>(scope: &mut Node<Scope<'a, 'b>>) -> Box<Ty<'a>> {
    let typevar = &mut scope.value.type_vars;
    let value = *typevar;
    *typevar += 1;
    Box::new(Ty::GeneratedVar(value))
}

fn resolve_symbols<'a, 'b>(source: &'b Decl<'a>, scope: &mut Node<Scope<'a, 'b>>) {
    match source {
        Decl::Variable{pattern, ..} => 
        {
            let identifiers = crate::parser::identifiers_from_pattern(pattern.as_ref());
            for id in identifiers {
                scope.value.values.insert(id.fragment(), source);
            }
        },
        Decl::Function{name, ..} => {
            scope.value.values.insert(name, source);
        },
        Decl::Operator{name, ..} => {
            if let Token::Identifier(symbol) = *name {
                scope.value.values.insert(symbol.fragment(), source);
            }
        },
        Decl::Module{name, ..} => {
            if let Token::Identifier(symbol) = *name {
                scope.value.types.insert(symbol.fragment(), source);
            }
        },
        Decl::Class{class, ..} => {
            if let Token::Identifier(symbol) = *class {
                scope.value.types.insert(symbol.fragment(), source);
            }
        },
        Decl::Implementation{class, ..} => {
            if let Token::Identifier(symbol) = *class {
                scope.value.types.insert(symbol.fragment(), source);
            }
        },
        Decl::NewType{defined, replacing} => {
            todo!()
        },
        Decl::Struct{name, members} => {
            todo!()
        },
        Decl::Union{name, variants} => {
            todo!()
        },
        Decl::Expr(expr) => resolve_expression(expr.as_ref(), scope),
        Decl::Error => ()
    }
}

fn resolve_expression<'a, 'b>(expr: &'b Expr<'a>, scope: &mut Node<Scope<'a, 'b>>) {
    match expr {
        Expr::If { cond, true_branch, else_branch } => {
            resolve_expression(cond.as_ref(), scope);
            resolve_expression(true_branch.as_ref(), scope);
            match else_branch {
                None => (),
                Some(else_branch) => resolve_expression(else_branch.as_ref(), scope)
            }
        },
        Expr::While { cond, loop_ } => {
            resolve_expression(cond.as_ref(), scope);
            resolve_expression(loop_.as_ref(), scope);
        },
        Expr::Loop { loop_ } => {
            resolve_expression(loop_.as_ref(), scope);
        },
        Expr::Switch{expr, arms} => {
            todo!()
        },
        Expr::Assignment { assigned, value } => todo!(),
        Expr::Literal(literal_token) => todo!(),
        Expr::Identifier(identifier_token) => {
            if let Token::Identifier(str) = identifier_token {
                let id = find_in_scope(str, scope, Namespace::Value);
                if id.is_none() {
                    println!("Cannot find variable {} in scope!", str);
                }
            }
            else {
                unreachable!()
            }
        },
        Expr::Placeholder => {
            
        },
        Expr::Tuple(expr_list) => {
            for expr in expr_list {
                resolve_expression(expr.as_ref(), scope);
            }
        },
        Expr::Block{declarations, ret} => {
            let block_scope = scope.insert_child(Scope::new());
            for decl in declarations {
                resolve_symbols(decl.as_ref(), block_scope);
            }

            match ret {
                Some(ret) => resolve_expression(ret.as_ref(), scope),
                None => ()
            }
        },
        Expr::ArrayConstructor(array_values) => {
            for value in array_values {
                resolve_expression(value.as_ref(), scope);
            }
        },
        Expr::Unsafe(unsafe_expr) => {
            resolve_expression(unsafe_expr.as_ref(), scope);
        },
        Expr::Return(return_expr) => {
            resolve_expression(return_expr.as_ref(), scope);
        },
        Expr::Dereference(dereferenced_expr) => {
            resolve_expression(dereferenced_expr.as_ref(), scope);
        },
        Expr::Reference{ref_, mutable} => {
            resolve_expression(ref_.as_ref(), scope);
        },
        Expr::Prefix{op, expr} 
        | Expr::Postfix{op, expr} => todo!(),
        Expr::Binary{expr1, op, expr2} => todo!(),
        Expr::FieldCall(expr, field) => todo!(),
        Expr::FunctionCall{function, args} => todo!(),
        Expr::ArrayIndex{array, arg} => todo!(),
        Expr::StructInit{id, args} => todo!(),
        _ => unimplemented!()
    }
}

fn pattern_to_type<'a, 'b>(pattern: &'b Pattern<'a>, scope: &mut Node<Scope<'a, 'b>>) -> Box<Ty<'a>> {
    match pattern {
        Pattern::Identifier(_)
         |Pattern::Literal(_) => new_type_var(scope),
        Pattern::Tuple(pattern_list) => Box::new(Ty::Tuple(
            pattern_list.iter().map(|x| pattern_to_type(x, scope)).collect()
        )),
        Pattern::VariantUnwrap(variant, exprs) => {
            //return to this when you figure out how to check if a variant exists
            todo!()
        },
        Pattern::Placeholder => new_type_var(scope),
        Pattern::Array(pattern_list) => {
            let mut type_list = Vec::new();
            for pattern in pattern_list {
                type_list.push(pattern_to_type(pattern.as_ref(), scope));
            }
            //change this line to an error after you figure out how to proccess errors in type checking
            assert!(type_list.windows(2).all(|w| w[0] == w[1]));

            let array_type = match type_list.pop() {
                None => new_type_var(scope),
                Some(x) => x
            };
            Box::new(Ty::Array(array_type, None))
        },
        Pattern::Error => todo!()
    }
}