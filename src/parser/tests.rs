#[allow(unused_imports)]
use super::parse;
#[allow(unused_imports)]
use super::data_types::{ParsedTree, TokenKind};
#[allow(unused_imports)]
use rowan::{GreenNode, GreenNodeBuilder};
#[allow(unused_macros)]
macro_rules! build_tree {
    ($str: ident, $($rest: tt)*) => {
        {
            let mut builder = GreenNodeBuilder::new();
            build_tree_helper!(builder, $str, $($rest)*);
            builder.finish()
        }
    }
}
#[allow(unused_macros)]
macro_rules! build_tree_helper {
    ($builder: ident, $str: ident,) => {};
    ($builder: ident, $str: ident, $token: ident [ $range: expr ] $($list:tt)* ) => {
        $builder.token(TokenKind::$token.into(), &$str[$range]);
        build_tree_helper!($builder, $str, $($list)*)
    };
    ($builder: ident, $str: ident, $token: ident { $($list: tt)* } $($rest:tt)*) => { 
        $builder.start_node(TokenKind::$token.into());
        build_tree_helper!($builder, $str, $($list)*);
        $builder.finish_node();
        build_tree_helper!($builder, $str, $($rest)*);
    }
}

#[test]
fn empty() {
    let program = "";
    let result = parse(program);
    let tree = build_tree!(program, SourceFile {});
    let parsed_tree = ParsedTree{tree, errors: vec![]};
    assert_eq!(result, parsed_tree);
}
#[test]
fn basic_expressions() {
    let program = "1;test;3.14f;3.14;\"test\";'t';";
    let result = parse(program);
    let tree = build_tree!(program, SourceFile {
        ExprDecl {
            LiteralExpr {
                Int[0..1]
            }
            Semicolon[1..2]
        }
        ExprDecl {
            IdentifierExpr {
                Identifier[2..6]
            }
            Semicolon[6..7]
        }
        ExprDecl {
            LiteralExpr {
                Float[7..12]
            }
            Semicolon[12..13]
        }
        ExprDecl {
            LiteralExpr {
                Double[13..17]
            }
            Semicolon[17..18]
        }
        ExprDecl {
            LiteralExpr {
                String[18..24]
            }
            Semicolon[24..25]
        }
        ExprDecl {
            LiteralExpr {
                Char[25..28]
            }
            Semicolon[28..29]
        }
    });
    let parsed_tree = ParsedTree{tree, errors: vec![]};
    assert_eq!(result, parsed_tree);
}

#[test]
fn unary_expr() {
    let program = "+x;-%y;";
    let result = parse(program);
    let tree = build_tree!(program, SourceFile {
        ExprDecl {
          PrefixExpr {
            Operator [0..1]    
            IdentifierExpr {
              Identifier [1..2]
            }
          }
          Semicolon [2..3]
        }
        ExprDecl {
          PrefixExpr {
            Operator [3..5]
            IdentifierExpr {
              Identifier [5..6]
            }
          }
          Semicolon[6..7]
        }
      });
      let parsed_tree = ParsedTree{tree, errors: vec![]};
      assert_eq!(result, parsed_tree);
}

#[test]
fn binary_expr() {
    let program = "x+y;x==y;x!=y;";
    let result = parse(program);
    let tree = build_tree!(program, SourceFile {
        ExprDecl {
          BinaryExpr {      
            IdentifierExpr {
              Identifier [0..1]
            }
            Operator [1..2]
            IdentifierExpr {
              Identifier [2..3]
            }
          }
          Semicolon [3..4]
        }
        ExprDecl {
          BinaryExpr {
            IdentifierExpr {
              Identifier [4..5]
            }
            Operator [5..7]
            IdentifierExpr {
              Identifier [7..8]
            }
          }
          Semicolon [8..9]
        }
        ExprDecl {
          BinaryExpr {
            IdentifierExpr {
              Identifier [9..10]
            }
            Operator [10..12]
            IdentifierExpr {
              Identifier [12..13]
            }
          }
          Semicolon [13..14]
        }
      });
    let parsed_tree = ParsedTree{tree, errors: vec![]};
    assert_eq!(result, parsed_tree);
}

#[test]
fn paren_expr() {
    let program = "(x);";
    let result = parse(program);
    let tree = build_tree!(program, SourceFile {
        ExprDecl {
          GroupingExpr {       
            Paren [0..1]       
            IdentifierExpr {   
              Identifier [1..2]
            }
            CloseParen [2..3]  
          }
          Semicolon [3..4]
        }
      });
    let parsed_tree = ParsedTree{tree, errors: vec![]};
    assert_eq!(result, parsed_tree);
}

#[test]
fn field_call() {
    let program = "x.y;";
    let result = parse(program);
    let tree = build_tree!(program, SourceFile {
        ExprDecl {
          FieldCallExpr {      
            IdentifierExpr {   
              Identifier [0..1]
            }
            Dot [1..2]
            IdentifierExpr {
              Identifier [2..3]
            }
          }
          Semicolon [3..4]
        }
      });
    let parsed_tree = ParsedTree{tree, errors: vec![]};
    assert_eq!(result, parsed_tree);
}

#[test]
fn function_call() {
    let program = "foo(x);bar(x,y);bar(x,y,);";
    let result = parse(program);
    let tree = build_tree!(program, SourceFile {
        ExprDecl {
          FunctionCallExpr {   
            IdentifierExpr {   
              Identifier [0..3]
            }
            Paren [3..4]
            IdentifierExpr {
              Identifier [4..5]
            }
            CloseParen [5..6]
          }
          Semicolon [6..7]
        }
        ExprDecl {
          FunctionCallExpr {
            IdentifierExpr {
              Identifier [7..10]
            }
            Paren [10..11]
            IdentifierExpr {
              Identifier [11..12]
            }
            Comma [12..13]
            IdentifierExpr {
              Identifier [13..14]
            }
            CloseParen [14..15]
          }
          Semicolon [15..16]
        }
        ExprDecl {
          FunctionCallExpr {
            IdentifierExpr {
              Identifier [16..19]
            }
            Paren [19..20]
            IdentifierExpr {
              Identifier [20..21]
            }
            Comma [21..22]
            IdentifierExpr {
              Identifier [22..23]
            }
            Comma [23..24]
            CloseParen [24..25]
          }
          Semicolon [25..26]
        }
      });
    let parsed_tree = ParsedTree{tree, errors: vec![]};
    assert_eq!(result, parsed_tree);
}

#[test]
fn block_expr() {
    let program = "{};{test};{test;};";
    let result = parse(program);
    let tree = build_tree!(program, SourceFile {
        ExprDecl {
          BlockExpr {        
            Brace [0..1]     
            CloseBrace [1..2]
          }
          Semicolon [2..3]   
        }
        ExprDecl {
          BlockExpr {
            Brace [3..4]
            IdentifierExpr {
              Identifier [4..8]
            }
            CloseBrace [8..9]
          }
          Semicolon [9..10]
        }
        ExprDecl {
          BlockExpr {
            Brace [10..11]
            ExprDecl {
              IdentifierExpr {
                Identifier [11..15]
              }
              Semicolon [15..16]
            }
            CloseBrace [16..17]
          }
          Semicolon [17..18]
        }
      });
    let parsed_tree = ParsedTree{tree, errors: vec![]};
    assert_eq!(result, parsed_tree);
}

#[test]
fn if_expr() {
    let program = "if x {y}; if x {y} else {z};";
    let result = parse(program);
    let tree = build_tree!(program, SourceFile {
        ExprDecl {
          IfExpr {
            If [0..2]
            WhiteSpace [2..3]  
            IdentifierExpr {   
              Identifier [3..4]
              WhiteSpace [4..5]
            }
            BlockExpr {
              Brace [5..6]
              IdentifierExpr {
                Identifier [6..7]
              }
              CloseBrace [7..8]
            }
          }
          Semicolon [8..9]
          WhiteSpace [9..10]
        }
        ExprDecl {
          IfExpr {
            If [10..12]
            WhiteSpace [12..13]
            IdentifierExpr {
              Identifier [13..14]
              WhiteSpace [14..15]
            }
            BlockExpr {
              Brace [15..16]
              IdentifierExpr {
                Identifier [16..17]
              }
              CloseBrace [17..18]
              WhiteSpace [18..19]
            }
            Else [19..23]
            WhiteSpace [23..24]
            BlockExpr {
              Brace [24..25]
              IdentifierExpr {
                Identifier [25..26]
              }
              CloseBrace [26..27]
            }
          }
          Semicolon [27..28]
        }
      });
    let parsed_tree = ParsedTree{tree, errors: vec![]};
    assert_eq!(result, parsed_tree);
}

#[test]
fn while_expr() {
    let program = "while x {y;};";
    let result = parse(program);
    let tree = build_tree!(program, SourceFile {      
        ExprDecl {      
          WhileExpr {   
            While [0..5]
            WhiteSpace [5..6]
            IdentifierExpr {
              Identifier [6..7]
              WhiteSpace [7..8]
            }
            BlockExpr {
              Brace [8..9]
              ExprDecl {
                IdentifierExpr {
                  Identifier [9..10]
                }
                Semicolon [10..11]
              }
              CloseBrace [11..12]
            }
          }
          Semicolon[12..13]
        }
      });
    let parsed_tree = ParsedTree{tree, errors: vec![]};
    assert_eq!(result, parsed_tree);
}

#[test]
fn loop_expr() {
    let program = "loop {x};";
    let result = parse(program);
    let tree = build_tree!(program, SourceFile {     
        ExprDecl {     
          LoopExpr {   
            Loop [0..4]
            WhiteSpace [4..5]
            BlockExpr {
              Brace [5..6]
              IdentifierExpr {
                Identifier [6..7]
              }
              CloseBrace [7..8]
            }
          }
          Semicolon [8..9]
        }
      });
    let parsed_tree = ParsedTree{tree, errors: vec![]};
    assert_eq!(result, parsed_tree);
}

#[test]
fn array_expr() {
    let program = "[1,];[1, 2, 3];";
    let result = parse(program);
    let tree = build_tree!(program, SourceFile {
        ExprDecl {
          ArrayConstructorExpr {
            Bracket [0..1]      
            LiteralExpr {       
              Int [1..2]
            }
            Comma [2..3]
            CloseBracket [3..4]
          }
          Semicolon [4..5]
        }
        ExprDecl {
          ArrayConstructorExpr {
            Bracket [5..6]
            LiteralExpr {
              Int [6..7]
            }
            Comma [7..8]
            WhiteSpace [8..9]
            LiteralExpr {
              Int [9..10]
            }
            Comma [10..11]
            WhiteSpace [11..12]
            LiteralExpr {
              Int [12..13]
            }
            CloseBracket [13..14]
          }
          Semicolon [14..15]
        }
      });
    let parsed_tree = ParsedTree{tree, errors: vec![]};
    assert_eq!(result, parsed_tree);
}

#[test]
fn unsafe_expr() {
    let program = "unsafe x;";
    let result = parse(program);
    let tree = build_tree!(program, SourceFile {
        ExprDecl {
          UnsafeExpr {       
            Unsafe [0..6]    
            WhiteSpace [6..7]
            IdentifierExpr {
              Identifier [7..8]
            }
          }
          Semicolon [8..9]
        }
      });
    let parsed_tree = ParsedTree{tree, errors: vec![]};
    assert_eq!(result, parsed_tree);
}

#[test]
fn return_expr() {
    let program = "return x;";
    let result = parse(program);
    let tree = build_tree!(program, SourceFile {
        ExprDecl {
          ReturnExpr {       
            Return [0..6]    
            WhiteSpace [6..7]
            IdentifierExpr {
              Identifier [7..8]
            }
          }
          Semicolon [8..9]
        }
      });
    let parsed_tree = ParsedTree{tree, errors: vec![]};
    assert_eq!(result, parsed_tree);
}

#[test]
fn lambda_expr() {
    let program = "lambda(x) x;";
    let result = parse(program);
    let tree = build_tree!(program, SourceFile {       
        ExprDecl {       
          LambdaExpr {   
            Lambda [0..6]
            Paren [6..7] 
            IdentifierPattern {
              Identifier [7..8]
            }
            CloseParen [8..9]
            WhiteSpace [9..10]
            IdentifierExpr {
              Identifier [10..11]
            }
          }
          Semicolon [11..12]
        }
      });
    let parsed_tree = ParsedTree{tree, errors: vec![]};
    assert_eq!(result, parsed_tree);
}

#[test]
fn dereference() {
    let program = "x@;";
    let result = parse(program);
    let tree = build_tree!(program, SourceFile {
        ExprDecl {
          DereferenceExpr {    
            IdentifierExpr {   
              Identifier [0..1]
            }
            Dereference [1..2] 
          }
          Semicolon [2..3]
        }
      });
    let parsed_tree = ParsedTree{tree, errors: vec![]};
    assert_eq!(result, parsed_tree);
}

#[test]
fn variable_declarations() {
    let program = "let x;let y:i32;let z=5;";
    let result = parse(program);
    let tree = build_tree!(program, SourceFile {
        VariableDecl {
            Let[0..3]
            WhiteSpace[3..4]
            IdentifierPattern {
                Identifier[4..5]
            }
            Semicolon[5..6]
        }
        VariableDecl {
            Let[6..9]
            WhiteSpace[9..10]
            IdentifierPattern {
                Identifier[10..11]
            }
            Colon[11..12]
            BasicType {
                Identifier[12..15]
            }
            Semicolon[15..16]
        }
        VariableDecl {
            Let[16..19]
            WhiteSpace[19..20]
            IdentifierPattern {
                Identifier[20..21]
            }
            Equal[21..22]
            LiteralExpr {
                Int[22..23]
            }
            Semicolon[23..24]
        }
    });
    let parsed_tree = ParsedTree{tree, errors: vec![]};
    assert_eq!(result, parsed_tree);
}