use crate::ast;
use ast::{ArrowType, SymbolTable, TypeIdx, expr::Expr, decl::Decl, DeclIdx, ExprIdx};
use smol_str::SmolStr;
use index_vec::IndexVec;
use std::collections::HashMap;

#[derive(Clone, Debug)]
#[allow(unused)]
pub(crate) struct RewriteRule {
    pub(crate) type_replaced: Type,
    pub(crate) replaced_by: Type,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub(crate) struct TypeVarID(pub usize);

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) struct LifetimeID(usize);
impl LifetimeID {
    fn _increment(&mut self) -> Self {
        self.0 += 1;
        *self
    }
}

pub(crate) struct State<'a, 'b> {
    pub(crate) scopes: &'a mut Vec<&'a SymbolTable>,
    pub(crate) declarations: &'a IndexVec<DeclIdx, Decl>,
    pub(crate) decl_types: &'a HashMap<DeclIdx, Type>,
    pub(crate) expressions: &'a IndexVec<ExprIdx, Expr>,
    pub(crate) expr_types: &'b mut IndexVec<ExprIdx, Option<Type>>,
    pub(crate) var_types: &'b mut usize,
    pub(crate) map: &'b mut HashMap<String, usize>
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) enum Type {
    Applied(Box<Type>, Vec<Type>), //used for all higher-order types, including functions, slices, and tuples
    Basic(TypeIdx),                //Represents a unique type that has a definition
    Array(Option<usize>),          //[] or [1]
    Tuple(usize),                  //(), (,), (,,), etc.
    Placeholder,                   //used exclusively for placeholder expressions, to resolve partial application
    Reference {
        mutable: bool,
    },                             //& and &mut
    Pointer {
        mutable: bool,
    },                             //% and %mut
    TypeVar(TypeVarID),            // 'T
    Unknown(TypeVarID),            // all types that are unknown from the source file, but have exactly one correct answer
    Lifetime(LifetimeID),          // #a
    Arrow,                         // (->)
    MutArrow,                      // (*>)
    OnceArrow,                     // (+>)
    Struct {
        struct_type: Box<Type>,
        values: Vec<(SmolStr, Type)>,
        complete: bool,
    },
}

pub(crate) const TYPE_BOOL: usize = 0;
pub(crate) const TYPE_I8: usize = 1;
pub(crate) const TYPE_I16: usize = 2;
pub(crate) const TYPE_I32: usize = 3;
pub(crate) const TYPE_I64: usize = 4;
pub(crate) const TYPE_I128: usize = 5;
pub(crate) const TYPE_ISIZE: usize = 6;
pub(crate) const TYPE_U8: usize = 7;
pub(crate) const TYPE_U16: usize = 8;
pub(crate) const TYPE_U32: usize = 9;
pub(crate) const TYPE_U64: usize = 10;
pub(crate) const TYPE_U128: usize = 11;
pub(crate) const TYPE_USIZE: usize = 12;
pub(crate) const TYPE_F32: usize = 13;
pub(crate) const TYPE_F64: usize = 14;
pub(crate) const TYPE_CHAR: usize = 15;
pub(crate) const TYPE_STR: usize = 16;

impl Type {
    pub(crate) fn lower(
        t: &crate::ast::types::Type,
        scopes: &Vec<&SymbolTable>,
        map: &mut HashMap<String, usize>,
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
                let input = Self::lower(input, scopes, map, vartypes)?;
                let output = Self::lower(output, scopes, map, vartypes)?;
                Self::Applied(arrow, vec![input, output])
            }
            ast::types::Type::Applied { applied, type_vars } => {
                let applied = Box::new(Self::lower(applied, scopes, map, vartypes)?);
                let type_vars = type_vars
                    .iter()
                    .map(|x| Self::lower(x, scopes, map, vartypes))
                    .flatten()
                    .collect();
                Self::Applied(applied, type_vars)
            }
            ast::types::Type::Reference { mutable, type_ } => {
                let type_ = Self::lower(type_, scopes, map, vartypes)?;
                Self::Applied(Box::new(Self::Reference { mutable: *mutable }), vec![type_])
            }
            ast::types::Type::Pointer { mutable, type_ } => {
                let type_ = Self::lower(type_, scopes, map, vartypes)?;
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
                let type_ = Self::lower(type_, scopes, map, vartypes)?;
                Self::Applied(array, vec![type_])
            }
            ast::types::Type::Tuple(types) => {
                let size = types.len();
                let tuple = Box::new(Self::Tuple(size));
                let types: Vec<Self> = types
                    .iter()
                    .map(|x| Self::lower(x, scopes, map, vartypes))
                    .flatten()
                    .collect();
                if size != types.len() {
                    return None;
                } else {
                    Self::Applied(tuple, types)
                }
            }
            ast::types::Type::Lifetime(id) => match map.entry(id.str().into()) {
                std::collections::hash_map::Entry::Occupied(occupied) => {
                    Self::Lifetime(LifetimeID(*occupied.get()))
                },
                std::collections::hash_map::Entry::Vacant(vacant) => {
                let x = *vartypes;
                vacant.insert(x);
                *vartypes += 1;
                Self::Lifetime(LifetimeID(x))
                }
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
            ast::types::Type::Var(id) => match map.entry(id.str().into()) {
                std::collections::hash_map::Entry::Occupied(occupied) => {
                    Self::TypeVar(TypeVarID(*occupied.get()))
                },
                std::collections::hash_map::Entry::Vacant(vacant) => {
                let x = *vartypes;
                vacant.insert(x);
                *vartypes += 1;
                Self::TypeVar(TypeVarID(x))
                }
            },
        };
        Some(result)
    }
}

pub(crate) fn new_unknown_type(count: &mut usize) -> Type {
    let x = *count;
    *count += 1;
    Type::Unknown(TypeVarID(x))
}