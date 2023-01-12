use std::collections::HashMap;
use crate::parser::ParsedTree;



//#[derive(Clone, Debug)]
//pub struct CaseExpr<'a>(Box<Pattern<'a>>, Box<Expr<'a>>);
//
//#[derive(Clone, Debug)]
//pub enum Expr<'a> {
//    If{cond: Box<Expr<'a>>, true_branch: Box<Expr<'a>>, else_branch: Option<Box<Expr<'a>>>},
//    //For{init: Option<Box<Expression<'a>>>, cond: Option<Box<Expression<'a>>>, incr: Option<Box<Expression<'a>>>, _loop: Box<Expression<'a>>},
//    While{cond: Box<Expr<'a>>, loop_: Box<Expr<'a>>},
//    Loop{loop_: Box<Expr<'a>>},
//    Switch{expr: Box<Expr<'a>>, arms: Vec<Box<CaseExpr<'a>>>},
//    Assignment{assigned: Box<Expr<'a>>, value: Box<Expr<'a>>},
//    Literal(Token<'a>),
//    _Lambda,
//    Identifier(Token<'a>),
//    Placeholder,
//    Tuple(Vec<Box<Expr<'a>>>),
//    Prefix{op: Token<'a>, expr: Box<Expr<'a>>},
//    Block{declarations: Vec<Box<Decl<'a>>>, ret: Option<Box<Expr<'a>>>},
//    ArrayConstructor(Vec<Box<Expr<'a>>>),
//    Unsafe(Box<Expr<'a>>),
//    Unit,
//    Return(Box<Expr<'a>>),
//    //left-recursive expressions
//    Dereference(Box<Expr<'a>>),
//    Reference{ref_: Box<Expr<'a>>, mutable: bool},
//    Postfix{op: Token<'a>, expr: Box<Expr<'a>>},
//    Binary{expr1: Box<Expr<'a>>, op: Token<'a>, expr2: Box<Expr<'a>>},
//    Path{identifier: Box<Expr<'a>>, expr: Box<Expr<'a>>},
//    FieldCall(Box<Expr<'a>>, Box<Expr<'a>>),
//    FunctionCall{function:  Box<Expr<'a>>, args: Vec<Box<Expr<'a>>>},
//    ArrayIndex{array:Box<Expr<'a>>, arg: Box<Expr<'a>>},
//    StructInit{id: Box<Expr<'a>>, args: Vec<Box<Expr<'a>>>},
//    Error
//}
//
//#[derive(Clone, Debug, PartialEq) ]
//pub enum Ty<'a> {
//    Basic(Token<'a>),
//    ArrayBasic(Option<Token<'a>>), //todo: make this a number
//    Unit,
//    Reference,
//    Pointer,
//    Var(Token<'a>),
//    GeneratedVar(u64),
//    Applied(Box<Ty<'a>>, Vec<Box<Ty<'a>>>),
//    Array(Box<Ty<'a>>, Option<Token<'a>>), //todo: make size a number
//    Tuple(Vec<Box<Ty<'a>>>),
//    Error
//}
//
//#[derive(Clone, Debug)]
//pub enum Pattern<'a> {
//    Identifier(&'a str),
//    Literal(Token<'a>),
//    Tuple(Vec<Box<Pattern<'a>>>),
//    Placeholder,
//    Array(Vec<Box<Pattern<'a>>>),
//    VariantUnwrap(Box<Pattern<'a>>, Vec<Box<Pattern<'a>>>),
//    Error
//}
//#[derive(Clone, Debug)]
//pub struct Param<'a> {
//    name: Box<Pattern<'a>>,
//    type_: Option<Box<Ty<'a>>>
//}
//#[derive(Clone, Debug)]
//pub enum Fixity {
//    Prefix,
//    Infix,
//    Postfix
//}
//
//#[derive(Clone, Debug)]
//pub enum Decl<'a> {
//    Variable{mutable: bool, pattern: Box<Pattern<'a>>, type_: Option<Box<Ty<'a>>>, value: Option<Box<Expr<'a>>>},
//    Function{ name: &'a str, args: Vec<Param<'a>>, return_type: Option<Box<Ty<'a>>>, body: Box<Expr<'a>>},
//    Operator{name: Token<'a>, fixity: Fixity, precedence: Token<'a>},
//    Class{class: Token<'a>, types: Vec<Box<Ty<'a>>>, constraints: Option<Vec<Box<Ty<'a>>>>, body: Box<Expr<'a>>},
//    Implementation{class: Token<'a>, types: Vec<Box<Ty<'a>>>, constraints: Option<Vec<Box<Ty<'a>>>>, body: Box<Expr<'a>>}, //finish this
//    Module{name: Token<'a>, body: Box<Expr<'a>>},
//    Union{name: Box<Ty<'a>>, variants: Vec<Param<'a>>},
//    Struct{name: Box<Ty<'a>>, members: Vec<Param<'a>>},
//    NewType{defined: Box<Ty<'a>>, replacing: Box<Ty<'a>>},
//    Expr(Box<Expr<'a>>),
//    Error
//}
//
//struct Scope<'a, 'b> where 'b: 'a {
//    values: HashMap<&'a str, &'b Decl<'a>>,
//    types: HashMap<&'a str, &'b Decl<'a>>,
//    macros: HashMap<&'a str, &'b Decl<'a>>,
//    type_vars: u64
//} 
//
//impl<'a, 'b> Scope<'a, 'b>{
//    fn new() -> Self {
//        Scope{values: HashMap::new(), types: HashMap::new(), macros: HashMap::new(), type_vars: 0}
//    }
//    
//}
//
//struct Node<T>{parent: Option<*const Node<T>>, children: Vec<Node<T>>, value: T}
//
//impl<T> Node<T> {
//    fn new(parent: Option<*const Node<T>>, value: T) -> Self {
//        Node{parent, children: vec![], value}
//    }
//
//    fn parent(&self) -> Option<&Node<T>>{
//        self.parent.and_then(|parent| {Some(unsafe{&*parent})})
//    }
//
//    fn insert_child(&mut self, value: T) -> &mut Self {
//        self.children.push(Self::new(Some(self), value));
//        self.children.last_mut().unwrap()
//    }
//}
//
//enum Namespace {
//    Value,
//    Type,
//    Macro
//}
//
//fn find_in_scope<'a, 'b>(identifier: &str, scope: &'b Node<Scope<'a, 'b>>, namespace: Namespace) -> Option<&'b Decl<'a>> where 'a: 'b {
//
//    let name = match namespace {
//        Namespace::Value =>scope.value.values.get(identifier),
//        Namespace::Type =>scope.value.types.get(identifier),
//        Namespace::Macro => scope.value.macros.get(identifier),
//    };
//    match name {
//        Some(&name) => Some(name),
//        None => {
//            let parent = scope.parent();
//            match parent {
//                Some(parent) => find_in_scope(identifier, parent, namespace),
//                None => None
//            }
//        }
//    }
//}

pub fn typecheck(source: &str) -> Result<(), ()> {
  let result = crate::parser::parse(source);
  //let mut scope_stack = Node::new(None, Scope::new());
  //for  decl in & result {
  //    resolve_symbols(decl.as_ref(), &mut scope_stack);
  //}
  if result.errors.is_empty() { Ok(())}
  else {Err(())}
}

//fn new_type_var<'a, 'b>(scope: &mut Node<Scope<'a, 'b>>) -> Box<Ty<'a>> {
//    let typevar = &mut scope.value.type_vars;
//    let value = *typevar;
//    *typevar += 1;
//    Box::new(Ty::GeneratedVar(value))
//}
//
//fn resolve_symbols<'a, 'b>(source: &'b Decl<'a>, scope: &mut Node<Scope<'a, 'b>>) {
//    todo!()
//    match source {
//        Decl::Variable{pattern, ..} => 
//        {
//            let identifiers = crate::parser::identifiers_from_pattern(pattern.as_ref());
//            for id in identifiers {
//                scope.value.values.insert(id.fragment(), source);
//            }
//        },
//        Decl::Function{name, ..} => {
//            scope.value.values.insert(name, source);
//        },
//        Decl::Operator{name, ..} => {
//            if let Token::Identifier(symbol) = *name {
//                scope.value.values.insert(symbol.fragment(), source);
//            }
//        },
//        Decl::Module{name, ..} => {
//            if let Token::Identifier(symbol) = *name {
//                scope.value.types.insert(symbol.fragment(), source);
//            }
//        },
//        Decl::Class{class, ..} => {
//            if let Token::Identifier(symbol) = *class {
//                scope.value.types.insert(symbol.fragment(), source);
//            }
//        },
//        Decl::Implementation{class, ..} => {
//            if let Token::Identifier(symbol) = *class {
//                scope.value.types.insert(symbol.fragment(), source);
//            }
//        },
//        Decl::NewType{defined, replacing} => {
//            todo!()
//        },
//        Decl::Struct{name, members} => {
//            todo!()
//        },
//        Decl::Union{name, variants} => {
//            todo!()
//        },
//        Decl::Expr(expr) => resolve_expression(expr.as_ref(), scope),
//        Decl::Error => ()
//    }
//}
//
//fn resolve_expression<'a, 'b>(expr: &'b Expr<'a>, scope: &mut Node<Scope<'a, 'b>>) {
//    todo!()
//    match expr {
//        Expr::If { cond, true_branch, else_branch } => {
//            resolve_expression(cond.as_ref(), scope);
//            resolve_expression(true_branch.as_ref(), scope);
//            match else_branch {
//                None => (),
//                Some(else_branch) => resolve_expression(else_branch.as_ref(), scope)
//            }
//        },
//        Expr::While { cond, loop_ } => {
//            resolve_expression(cond.as_ref(), scope);
//            resolve_expression(loop_.as_ref(), scope);
//        },
//        Expr::Loop { loop_ } => {
//            resolve_expression(loop_.as_ref(), scope);
//        },
//        Expr::Switch{expr, arms} => {
//            todo!()
//        },
//        Expr::Assignment { assigned, value } => todo!(),
//        Expr::Literal(literal_token) => todo!(),
//        Expr::Identifier(identifier_token) => {
//            if let Token::Identifier(str) = identifier_token {
//                let id = find_in_scope(str, scope, Namespace::Value);
//                if id.is_none() {
//                    println!("Cannot find variable {} in scope!", str);
//                }
//            }
//            else {
//                unreachable!()
//            }
//        },
//        Expr::Placeholder => {
//            
//        },
//        Expr::Tuple(expr_list) => {
//            for expr in expr_list {
//                resolve_expression(expr.as_ref(), scope);
//            }
//        },
//        Expr::Block{declarations, ret} => {
//            let block_scope = scope.insert_child(Scope::new());
//            for decl in declarations {
//                resolve_symbols(decl.as_ref(), block_scope);
//            }
//
//            match ret {
//                Some(ret) => resolve_expression(ret.as_ref(), scope),
//                None => ()
//            }
//        },
//        Expr::ArrayConstructor(array_values) => {
//            for value in array_values {
//                resolve_expression(value.as_ref(), scope);
//            }
//        },
//        Expr::Unsafe(unsafe_expr) => {
//            resolve_expression(unsafe_expr.as_ref(), scope);
//        },
//        Expr::Return(return_expr) => {
//            resolve_expression(return_expr.as_ref(), scope);
//        },
//        Expr::Dereference(dereferenced_expr) => {
//            resolve_expression(dereferenced_expr.as_ref(), scope);
//        },
//        Expr::Reference{ref_, mutable} => {
//            resolve_expression(ref_.as_ref(), scope);
//        },
//        Expr::Prefix{op, expr} 
//        | Expr::Postfix{op, expr} => todo!(),
//        Expr::Binary{expr1, op, expr2} => todo!(),
//        Expr::FieldCall(expr, field) => todo!(),
//        Expr::FunctionCall{function, args} => todo!(),
//        Expr::ArrayIndex{array, arg} => todo!(),
//        Expr::StructInit{id, args} => todo!(),
//        _ => unimplemented!()
//    }
//}
//
//fn pattern_to_type<'a, 'b>(pattern: &'b Pattern<'a>, scope: &mut Node<Scope<'a, 'b>>) -> Box<Ty<'a>> {
//    match pattern {
//        Pattern::Identifier(_)
//         |Pattern::Literal(_) => new_type_var(scope),
//        Pattern::Tuple(pattern_list) => Box::new(Ty::Tuple(
//            pattern_list.iter().map(|x| pattern_to_type(x, scope)).collect()
//        )),
//        Pattern::VariantUnwrap(variant, exprs) => {
//            //return to this when you figure out how to check if a variant exists
//            todo!()
//        },
//        Pattern::Placeholder => new_type_var(scope),
//        Pattern::Array(pattern_list) => {
//            let mut type_list = Vec::new();
//            for pattern in pattern_list {
//                type_list.push(pattern_to_type(pattern.as_ref(), scope));
//            }
//            //change this line to an error after you figure out how to proccess errors in type checking
//            assert!(type_list.windows(2).all(|w| w[0] == w[1]));
//
//            let array_type = match type_list.pop() {
//                None => new_type_var(scope),
//                Some(x) => x
//            };
//            Box::new(Ty::Array(array_type, None))
//        },
//        Pattern::Error => todo!()
//    }
//}