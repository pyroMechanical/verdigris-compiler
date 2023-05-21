use super::data_types::{SymbolTable, DeclIdx, ExprIdx, TypeIdx, Token};
use super::decl;
use super::patterns;
use super::types;
use crate::parser::data_types::TokenKind;
use index_vec::{IndexVec};
use smol_str::SmolStr;

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
        args: Vec<patterns::Pattern>,
        body: ExprIdx,
    },
}
impl Expr {
    pub fn lower(
        cst: Option<crate::reparse::expr::Expr>,
        errors: &mut Vec<String>,
        types: &mut IndexVec<TypeIdx, decl::Decl>,
        declarations: &mut IndexVec<DeclIdx, decl::Decl>,
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
        types: &mut IndexVec<TypeIdx, decl::Decl>,
        declarations: &mut IndexVec<DeclIdx, decl::Decl>,
        expressions: &mut IndexVec<ExprIdx, Expr>,
    ) -> Self {
        let decls: Vec<DeclIdx> = block
            .declarations()
            .map(|x| decl::Decl::lower(Some(x), errors, types, declarations, expressions))
            .collect();
        let terminator = block
            .terminator()
            .map(|x| Expr::lower(Some(x), errors, types, declarations, expressions));
        let symbols = super::symbols_from_declarations(&decls, declarations, expressions, errors, types);
        Self::Block {
            symbols,
            declarations: decls,
            terminator,
        }
    }

    fn lower_if(
        if_expr: crate::reparse::expr::IfExpr,
        errors: &mut Vec<String>,
        types: &mut IndexVec<TypeIdx, decl::Decl>,
        declarations: &mut IndexVec<DeclIdx, decl::Decl>,
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
        types: &mut IndexVec<TypeIdx, decl::Decl>,
        declarations: &mut IndexVec<DeclIdx, decl::Decl>,
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
        types: &mut IndexVec<TypeIdx, decl::Decl>,
        declarations: &mut IndexVec<DeclIdx, decl::Decl>,
        expressions: &mut IndexVec<ExprIdx, Expr>,
    ) -> Self {
        let body = Expr::lower(loop_expr.block(), errors, types, declarations, expressions);
        Self::Loop(body)
    }

    fn lower_assignment(
        expr: crate::reparse::expr::AssignmentExpr,
        errors: &mut Vec<String>,
        types: &mut IndexVec<TypeIdx, decl::Decl>,
        declarations: &mut IndexVec<DeclIdx, decl::Decl>,
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
        types: &mut IndexVec<TypeIdx, decl::Decl>,
        declarations: &mut IndexVec<DeclIdx, decl::Decl>,
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
        types: &mut IndexVec<TypeIdx, decl::Decl>,
        declarations: &mut IndexVec<DeclIdx, decl::Decl>,
        expressions: &mut IndexVec<ExprIdx, Expr>,
    ) -> Self {
        let mut symbols = SymbolTable::new();
        let args: Vec<patterns::Pattern> = lambda.args().map(|x| patterns::Pattern::lower(Some(x))).collect();
        for arg in &args {
            let decl = decl::Decl::Variable {
                mutable: false,
                pattern: arg.clone(),
                type_: Some(types::Type::Var(Token {
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
        types: &mut IndexVec<TypeIdx, decl::Decl>,
        declarations: &mut IndexVec<DeclIdx, decl::Decl>,
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
        types: &mut IndexVec<TypeIdx, decl::Decl>,
        declarations: &mut IndexVec<DeclIdx, decl::Decl>,
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
        types: &mut IndexVec<TypeIdx, decl::Decl>,
        declarations: &mut IndexVec<DeclIdx, decl::Decl>,
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
        types: &mut IndexVec<TypeIdx, decl::Decl>,
        declarations: &mut IndexVec<DeclIdx, decl::Decl>,
        expressions: &mut IndexVec<ExprIdx, Expr>,
    ) -> Self {
        let expr = Expr::lower(unsafe_.expr(), errors, types, declarations, expressions);
        Self::Unsafe(expr)
    }

    fn lower_path(
        path: crate::reparse::expr::PathExpr,
        errors: &mut Vec<String>,
        types: &mut IndexVec<TypeIdx, decl::Decl>,
        declarations: &mut IndexVec<DeclIdx, decl::Decl>,
        expressions: &mut IndexVec<ExprIdx, Expr>,
    ) -> Self {
        let lhs = Expr::lower(path.lhs(), errors, types, declarations, expressions);
        let rhs = Expr::lower(path.rhs(), errors, types, declarations, expressions);
        Self::Path { lhs, rhs }
    }

    fn lower_deref(
        deref: crate::reparse::expr::DereferenceExpr,
        errors: &mut Vec<String>,
        types: &mut IndexVec<TypeIdx, decl::Decl>,
        declarations: &mut IndexVec<DeclIdx, decl::Decl>,
        expressions: &mut IndexVec<ExprIdx, Expr>,
    ) -> Self {
        let expr = Expr::lower(deref.expr(), errors, types, declarations, expressions);
        Self::Dereference { expr }
    }

    fn lower_ref(
        ref_: crate::reparse::expr::ReferenceExpr,
        errors: &mut Vec<String>,
        types: &mut IndexVec<TypeIdx, decl::Decl>,
        declarations: &mut IndexVec<DeclIdx, decl::Decl>,
        expressions: &mut IndexVec<ExprIdx, Expr>,
    ) -> Self {
        let mutable = ref_.mutable();
        let expr = Expr::lower(ref_.expr(), errors, types, declarations, expressions);
        Self::Reference { mutable, expr }
    }

    fn lower_field_call(
        field_call: crate::reparse::expr::FieldCallExpr,
        errors: &mut Vec<String>,
        types: &mut IndexVec<TypeIdx, decl::Decl>,
        declarations: &mut IndexVec<DeclIdx, decl::Decl>,
        expressions: &mut IndexVec<ExprIdx, Expr>,
    ) -> Self {
        let lhs = Expr::lower(field_call.lhs(), errors, types, declarations, expressions);
        let field = Expr::lower(field_call.field(), errors, types, declarations, expressions);
        Self::FieldCall { lhs, rhs: field }
    }

    fn lower_function_call(
        function_call: crate::reparse::expr::FunctionCallExpr,
        errors: &mut Vec<String>,
        types: &mut IndexVec<TypeIdx, decl::Decl>,
        declarations: &mut IndexVec<DeclIdx, decl::Decl>,
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
        types: &mut IndexVec<TypeIdx, decl::Decl>,
        declarations: &mut IndexVec<DeclIdx, decl::Decl>,
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
        types: &mut IndexVec<TypeIdx, decl::Decl>,
        declarations: &mut IndexVec<DeclIdx, decl::Decl>,
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
        types: &mut IndexVec<TypeIdx, decl::Decl>,
        declarations: &mut IndexVec<DeclIdx, decl::Decl>,
        expressions: &mut IndexVec<ExprIdx, Expr>,
    ) -> Self {
        let expr = Expr::lower(return_.expr(), errors, types, declarations, expressions);
        Self::Return(expr)
    }

    fn lower_struct_init(
        struct_init: crate::reparse::expr::StructInitExpr,
        errors: &mut Vec<String>,
        types: &mut IndexVec<TypeIdx, decl::Decl>,
        declarations: &mut IndexVec<DeclIdx, decl::Decl>,
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